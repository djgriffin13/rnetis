# Test columns, significance marks and fit lines for the printed model tables.
#
# print() and summary() of a fit follow the layout a reader already knows from
# lm() and lmerTest: estimate, standard error, a test statistic, its degrees of
# freedom where the method defines them, a p-value and significance marks. What
# differs between backends is which of those the method actually supplies, and
# this file is where that is decided, once, for both printed surfaces.
#
#   stats (OLS)  t on the residual degrees of freedom, as summary.lm().
#   lme4         t on Satterthwaite's degrees of freedom, as lmerTest reports
#                them. The fit IS an lmerTest fit; nothing is re-derived here.
#   sna::lnam    z, asymptotic normal, as sna's own summary reports.
#   Selection    a posterior, so no test. The parallel columns are the ratio of
#                the posterior mean to the posterior SD and a two-sided
#                posterior tail probability, and they are labelled as that.

#' Significance marks on lm's cutoffs
#'
#' @param p p-values or posterior tail probabilities; `NA` gets no mark.
#' @return a character vector the length of `p`.
#' @noRd
.netis_stars <- function(p) {
  out <- rep("", length(p))
  ok <- !is.na(p)
  out[ok] <- as.character(cut(
    p[ok], breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
    labels = c("***", "**", "*", ".", ""), right = TRUE
  ))
  out
}

.NETIS_SIGNIF_LEGEND <- "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"

#' Format a probability for a printed table
#'
#' @param p probabilities.
#' @param floor the smallest value the column can resolve; anything below it
#'   prints as `<floor`. One value, or one per element of `p`.
#' @noRd
.netis_format_p <- function(p, floor = 0.001) {
  floor <- rep_len(floor, length(p))
  fmt <- function(v) {
    ifelse(v < 0.001, formatC(v, digits = 1, format = "e"), sprintf("%.3f", v))
  }
  out <- ifelse(is.na(p), "", ifelse(p < floor, paste0("<", fmt(floor)), fmt(p)))
  unname(out)
}

#' Format degrees of freedom: whole where they are whole, one decimal otherwise
#' @noRd
.netis_format_df <- function(df) {
  df[!is.finite(df)] <- NA_real_
  if (all(is.na(df) | abs(df - round(df)) < 1e-8)) {
    # ROUNDED, NOT TRUNCATED. A boundary fit's Satterthwaite df is a whole
    # number less floating-point noise (20.9999999997), and an integer format
    # applied to that prints 20 beside a test computed on 21.
    return(ifelse(is.na(df), "", formatC(round(df), format = "d", big.mark = "")))
  }
  ifelse(is.na(df), "", formatC(df, digits = 1, format = "f"))
}

#' Print a display table as one block, however narrow the console
#'
#' `print.data.frame()` wraps a table wider than the console into stacked
#' blocks, which strips a table's last columns -- the significance marks, a
#' "Counted" column -- from the rows they belong to and prints them as an
#' orphan block underneath. A long line is the lesser fault, and the one
#' `summary.lm()` also accepts. The caller's `width` is restored on exit.
#'
#' @param tab a display data frame.
#' @param right passed to `print.data.frame()`.
#' @noRd
.print_one_block <- function(tab, right = TRUE) {
  op <- options(width = max(getOption("width"), 200L))
  on.exit(options(op), add = TRUE)
  print(tab, row.names = FALSE, right = right)
  invisible(tab)
}

#' Print a coefficient table with its legend and its notes
#'
#' @param tab a display data frame; the attribute `netis_notes` holds the lines
#'   to print under it.
#' @noRd
.print_coef_table <- function(tab) {
  notes <- attr(tab, "netis_notes")
  .print_one_block(tab, right = TRUE)
  stars <- tab[[" "]]
  if (!is.null(stars) && any(nzchar(stars))) {
    cat("---\n", .NETIS_SIGNIF_LEGEND, "\n", sep = "")
  }
  for (n in notes) {
    cat(paste(strwrap(n, width = 88, exdent = 2), collapse = "\n"), "\n", sep = "")
  }
  invisible(tab)
}

# ---- Influence ---------------------------------------------------------------

#' The test each Influence backend supplies for its coefficients
#'
#' @param x a `netis_influence` fit.
#' @return a list: `kind` (`"t"` or `"z"`), `statistic`, `df` and `p` (either
#'   may be `NULL` where the method defines none), and `note`.
#' @noRd
.influence_coef_tests <- function(x) {
  nms <- names(x$coefficients)
  est <- unname(x$coefficients)
  se <- unname(sqrt(diag(x$vcov))[nms])
  stat <- est / se
  if (identical(x$backend, "stats")) {
    df <- rep(x$residual_df %||% NA_real_, length(nms))
    p <- suppressWarnings(2 * stats::pt(-abs(stat), df))
    return(list(kind = "t", statistic = stat, df = df, p = p, note = NULL))
  }
  if (identical(x$backend, "lme4")) {
    ct <- x$coef_tests
    if (!is.null(ct)) {
      m <- match(nms, ct$term)
      return(list(
        kind = "t", statistic = ct$statistic[m], df = ct$df[m], p = ct$p.value[m],
        note = "t tests and intervals use Satterthwaite's degrees of freedom, as lmerTest reports them."
      ))
    }
    # TWO DIFFERENT ABSENCES. A fit saved before the backend fitted through
    # lmerTest has none to report and a refit supplies them; a current fit for
    # which lmerTest could not compute them will not get them from a refit.
    current <- isTRUE(tryCatch(methods::is(x$backend_fit, "lmerModLmerTest"),
                               error = function(e) FALSE))
    return(list(
      kind = "t", statistic = stat, df = NULL, p = NULL,
      note = if (current) {
        "No degrees of freedom or p-values: lmerTest could not compute Satterthwaite's degrees of freedom for this fit. The intervals use the normal quantile."
      } else {
        "No degrees of freedom or p-values: this fit carries no lmerTest results. The intervals use the normal quantile. Refit with the current rnetis for Satterthwaite t tests and intervals."
      }
    ))
  }
  if (!any(is.finite(stat))) {
    # sna::lnam returned no information matrix: no standard error, so no test.
    return(list(kind = "z", statistic = stat, df = NULL, p = NULL,
                note = "No z tests: this fit has no standard errors."))
  }
  list(kind = "z", statistic = stat, df = NULL,
       p = 2 * stats::pnorm(-abs(stat)),
       note = "z tests: maximum likelihood with asymptotically normal inference, as sna::lnam reports.")
}

#' Append the test columns to a formatted Influence estimate table
#' @noRd
.add_influence_tests <- function(tab, tests) {
  if (!is.null(tests$df)) tab[["df"]] <- .netis_format_df(tests$df)
  if (any(is.finite(tests$statistic))) {
    tab[[sprintf("%s value", tests$kind)]] <-
      ifelse(is.na(tests$statistic), "", sprintf("%.3f", tests$statistic))
  }
  if (!is.null(tests$p)) {
    tab[[sprintf("Pr(>|%s|)", tests$kind)]] <- .netis_format_p(tests$p)
    tab[[" "]] <- .netis_stars(tests$p)
  }
  attr(tab, "netis_notes") <- tests$note
  tab
}

#' The model-fit lines under an Influence coefficient table
#'
#' What `summary.lm()` and lme4 print under theirs: the residual scale and its
#' degrees of freedom, R-squared, and on `full = TRUE` the overall F test and
#' the information criteria.
#'
#' @param x a `netis_influence` fit.
#' @param full `TRUE` for `summary()`, `FALSE` for `print()`.
#' @return a character vector of display lines.
#' @noRd
.influence_fit_lines <- function(x, full = FALSE) {
  out <- character(0)
  n <- .influence_nobs(x)
  ic <- function() {
    ll <- x$loglik
    if (!inherits(ll, "logLik") || !is.finite(as.numeric(ll))) return(character(0))
    reml <- identical(x$backend, "lme4") &&
      isTRUE(tryCatch(lme4::isREML(x$backend_fit), error = function(e) FALSE))
    sprintf("logLik: %.2f%s on %d parameters; AIC: %.2f; BIC: %.2f",
            as.numeric(ll), if (reml) " (REML)" else "", attr(ll, "df"),
            stats::AIC(x), stats::BIC(x))
  }

  if (identical(x$backend, "stats")) {
    out <- c(out, if (isTRUE(is.finite(x$sigma))) {
      sprintf("Residual standard error: %.4g on %s degrees of freedom",
              x$sigma, format(x$residual_df))
    } else {
      # A saturated fit: as many coefficients as rows, nothing left to estimate
      # a residual scale or a test from.
      sprintf("Residual standard error: not estimable on %s degrees of freedom",
              format(x$residual_df))
    })
    if (!is.null(x$r_squared) && !is.na(x$r_squared)) {
      out <- c(out, sprintf("R-squared: %.4f, adjusted: %.4f",
                            x$r_squared, x$adj_r_squared))
    }
    if (full) {
      f <- tryCatch(summary(x$backend_fit)$fstatistic, error = function(e) NULL)
      if (!is.null(f) && all(is.finite(f))) {
        out <- c(out, sprintf(
          "F-statistic: %.4g on %d and %d DF, p-value: %s", f[[1]],
          as.integer(f[[2]]), as.integer(f[[3]]),
          format.pval(stats::pf(f[[1]], f[[2]], f[[3]], lower.tail = FALSE), digits = 4)
        ))
      }
      out <- c(out, ic())
    }
    return(out)
  }

  if (identical(x$backend, "lme4")) {
    lev <- x$re_levels
    out <- c(out, sprintf(
      "Residual SD: %.4g; %d rows%s", x$sigma, n,
      if (length(lev)) {
        paste0("; groups: ", paste(sprintf("%s, %d", names(lev), lev), collapse = "; "))
      } else ""
    ))
    r2 <- tryCatch(pseudo_r2(x), error = function(e) NULL)
    out <- c(out, if (is.null(r2)) {
      "R-squared: not computed for this random-effects structure (see ?pseudo_r2)"
    } else {
      sprintf("R-squared (Nakagawa-Schielzeth): marginal %.4f, conditional %.4f",
              r2$measures$estimate[1], r2$measures$estimate[2])
    })
    if (full) out <- c(out, ic())
    return(out)
  }

  # sna::lnam. No R-squared: part of the outcome variance is generated by the
  # autoregressive feedback, and pseudo_r2() refuses it for that reason.
  if (!is.null(x$sigma) && is.finite(x$sigma)) {
    out <- c(out, sprintf("Residual SD: %.4g; %d rows", x$sigma, n))
  }
  out <- c(out, "R-squared: not defined for a simultaneous (SAR) fit (see ?pseudo_r2)")
  if (full) out <- c(out, ic())
  out
}

#' The random-effects variance table of a mixed Influence fit, as lme4 prints it
#'
#' @param x a `netis_influence` fit.
#' @return a display data frame, or `NULL` off the lme4 route.
#' @noRd
.influence_random_table <- function(x) {
  vc <- x$var_corr
  if (is.null(vc) || !nrow(vc)) return(NULL)
  is_cor <- !is.na(vc$var2)
  data.frame(
    Groups = vc$grp,
    Name = ifelse(is_cor, paste0("corr(", vc$var1, ", ", vc$var2, ")"),
                  ifelse(is.na(vc$var1), "", vc$var1)),
    Variance = ifelse(is_cor, "", formatC(vc$vcov, digits = 4, format = "g")),
    `Std.Dev.` = ifelse(is_cor, "", formatC(vc$sdcor, digits = 4, format = "g")),
    Corr = ifelse(is_cor, formatC(vc$sdcor, digits = 3, format = "f"), ""),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

#' One line naming the assumption ledger, in place of the ledger
#'
#' The ledger does not change the coefficient table and runs to a page; the
#' printed surfaces say that it exists, how much of it no statistic can test,
#' and which call lists it.
#'
#' @param a a fit's `assumptions`.
#' @return a single display line, or `character(0)` when there is no ledger.
#' @noRd
.format_assumptions_pointer <- function(a) {
  if (is.null(a) || !length(a$entries)) return(character(0))
  n <- length(a$entries)
  not_testable <- sum(vapply(a$entries, function(e) {
    identical(e$diagnostic_evidence, "not_testable")
  }, logical(1)))
  declared <- sum(vapply(a$entries, function(e) identical(e$source, "declared"),
                         logical(1)))
  sprintf(
    "Assumptions: this fit rests on %d assumptions (%d not testable%s). netis_assumptions(fit) lists them.",
    n, not_testable,
    if (declared > 0L) sprintf(", %d declared by you", declared) else ""
  )
}

# ---- Selection ---------------------------------------------------------------

#' The posterior parallel of a test statistic and its p-value
#'
#' `ratio` is the posterior mean over the posterior SD. `tail` is twice the
#' smaller of the posterior probabilities that the coefficient is above and
#' below zero -- a two-sided posterior tail probability, not a p-value. It is
#' estimated from the retained draws and so cannot resolve anything below
#' `2 / S`; `floor` carries that bound, and the marks are assigned from
#' `max(tail, floor)` so that a short chain cannot earn a mark it could not
#' have measured.
#'
#' @param v the retained draws of one coefficient.
#' @return a named numeric vector: `ratio`, `tail`, `floor`.
#' @noRd
.posterior_tail <- function(v) {
  v <- v[is.finite(v)]
  s <- length(v)
  if (s < 2L) return(c(ratio = NA_real_, tail = NA_real_, floor = NA_real_))
  sdv <- stats::sd(v)
  # Draws with no spread are not a posterior to take a tail of: a coefficient
  # the sampler never moved gets no ratio, no tail probability and no mark.
  if (!is.finite(sdv) || sdv <= 0) {
    return(c(ratio = NA_real_, tail = NA_real_, floor = NA_real_))
  }
  c(ratio = mean(v) / sdv,
    tail = 2 * min(mean(v > 0), mean(v < 0)),
    floor = 2 / s)
}

.SELECTION_POSTERIOR_NOTE <- "Posterior summaries, not tests. Estimate is the posterior mean; Est/SD is its ratio to the posterior SD; Tail prob. is twice the posterior probability that the coefficient has the opposite sign, and the marks are on that. It is counted from the retained draws, so its resolution is 2 / (number of draws)."

.SELECTION_POOLED_NOTE <- "z tests from the pooled estimates and their standard errors, on the normal quantile: two-stage pooling keeps no pooled draws to count a tail probability from."

#' Append the posterior columns to a Selection estimate table
#'
#' @param tab the display frame, one row per element of `tails`.
#' @param tails a list of `.posterior_tail()` results, `NULL` entries for rows
#'   with no draws.
#' @noRd
.add_posterior_columns <- function(tab, tails) {
  get <- function(nm) vapply(tails, function(t) {
    if (is.null(t)) NA_real_ else t[[nm]]
  }, numeric(1))
  ratio <- get("ratio")
  tail <- get("tail")
  floor <- get("floor")
  tab[["Est/SD"]] <- ifelse(is.na(ratio), "", sprintf("%.3f", ratio))
  tab[["Tail prob."]] <- .netis_format_p(tail, floor = ifelse(is.na(floor), 0.001, floor))
  tab[[" "]] <- .netis_stars(pmax(tail, floor))
  attr(tab, "netis_notes") <- .SELECTION_POSTERIOR_NOTE
  tab
}

#' The model-fit lines under a Selection coefficient table
#'
#' The count of dyads, the pseudo R-squared measures that cost no refit, and
#' the posterior predictive check. Each names the call that gives the detail,
#' and each says so where the fit cannot supply it instead of leaving a gap.
#'
#' @param x a `netis_selection` fit.
#' @param full `TRUE` for `summary()`, which prints the predictive table.
#' @return a character vector of display lines; on `full = TRUE` the attribute
#'   `ppc_table` holds the table to print after them.
#' @noRd
.selection_fit_lines <- function(x, full = FALSE) {
  n_groups <- x$n_groups %||% length(x$groups)
  out <- if (isTRUE(n_groups > 1L)) {
    sprintf("%d observed dyads in %d groups", .selection_nobs(x), n_groups)
  } else {
    sprintf("%d observed dyads among %s actors", .selection_nobs(x),
            format(sum(x$n_actors %||% NA_integer_)))
  }

  if (.selection_is_hierarchical(x)) {
    return(c(out,
      "Pseudo R-squared and the posterior predictive check are not defined for a hierarchical fit (see ?pseudo_r2 and ?netis_gof)."))
  }

  # Pseudo R-squared: the two measures that are arithmetic on what the fit
  # already holds. The null comparison refits a model and is never run here.
  r2 <- lapply(as.character(x$groups), function(g) {
    tryCatch(pseudo_r2(x, group = g, include_null_comparison = FALSE)$measures,
             error = function(e) NULL)
  })
  r2 <- r2[!vapply(r2, is.null, logical(1))]
  if (length(r2)) {
    share <- vapply(r2, function(m) m$estimate[1], numeric(1))
    tjur <- vapply(r2, function(m) {
      i <- grep("Tjur", m$measure)
      if (length(i)) m$estimate[i[1]] else NA_real_
    }, numeric(1))
    if (length(r2) == 1L) {
      m <- r2[[1]]
      line <- sprintf("Pseudo R-squared: latent-scale variance share %.3f [%.3f, %.3f]",
                      m$estimate[1], m$lo[1], m$hi[1])
      if (!is.na(tjur)) line <- paste0(line, sprintf("; Tjur's D %.3f", tjur))
      out <- c(out, paste0(line, ". What each measures: pseudo_r2(fit)."))
    } else {
      rng <- function(v) {
        v <- v[is.finite(v)]
        sprintf("median %.3f, range %.3f to %.3f", stats::median(v), min(v), max(v))
      }
      line <- sprintf("Pseudo R-squared across %d groups: latent-scale variance share %s",
                      length(r2), rng(share))
      if (any(is.finite(tjur))) line <- paste0(line, "; Tjur's D ", rng(tjur))
      out <- c(out, paste0(line, ". One group: pseudo_r2(fit, group = )."))
    }
  }

  ppc <- .selection_ppc_summary(x)
  out <- c(out, ppc$lines)
  if (full) attr(out, "ppc_table") <- ppc$table
  out
}

#' The posterior predictive check, reduced for a printed fit
#'
#' Counts how many observed statistics fall inside the central 95% of their
#' simulated distributions, and names the ones that do not. A count, with the
#' statistics named: the table and the figure carry the rest.
#'
#' @param x a non-hierarchical `netis_selection` fit.
#' @return a list: `lines` and `table` (`NULL` when nothing was simulated).
#' @noRd
.selection_ppc_summary <- function(x) {
  g <- tryCatch(netis_gof(x), error = function(e) NULL)
  if (is.null(g)) {
    return(list(lines = "Posterior predictive check: no simulated networks were kept with this fit (see ?netis_gof).",
                table = NULL))
  }
  inside <- g$observed >= g$predicted_lower & g$observed <= g$predicted_upper
  # WHAT COUNTS AS A CHECK. Three kinds of row are not one, and counting them
  # would overstate how much was checked:
  #   - no usable simulated distribution, or no observed value (`inside` is NA);
  #   - a statistic with no predictive spread, which the model reproduces by
  #     construction (on a symmetric fit `dyad.dep` is identically 1), and
  #     whose "inside" would be decided by rounding noise;
  #   - a statistic that repeats an earlier one in the same group (on a
  #     symmetric fit the in-degree spread IS the out-degree spread, and the
  #     two triadic statistics coincide).
  # Each is counted and named in the line, never dropped silently.
  unusable <- is.na(inside)
  spread <- g$predicted_sd
  # Constant AND REPRODUCED: the observed value is that constant. A model that
  # generates one value where the network shows another has not reproduced the
  # feature, and that row stays a check, which it fails.
  tol <- 1e-8 * pmax(1, abs(g$predicted_mean))
  constant <- !unusable & is.finite(spread) & spread <= tol &
    abs(g$observed - g$predicted_mean) <= tol
  key <- paste(g$grouping, signif(g$observed, 10), signif(g$predicted_mean, 10),
               signif(g$predicted_sd, 10))
  repeated <- !unusable & !constant & duplicated(key)
  checked <- !unusable & !constant & !repeated
  n_draws <- unique(g$n_draws[checked])
  multi <- length(unique(g$grouping)) > 1L
  head <- sprintf(
    "Posterior predictive check (%s simulated networks%s): the observed network is inside the simulated 95%% range on %d of %d %s.",
    if (length(n_draws) == 1L) format(n_draws) else "the retained",
    if (multi) " per group" else "",
    sum(inside[checked]), sum(checked),
    if (multi) "group-by-statistic checks" else "statistics"
  )
  not_counted <- c(
    if (any(unusable)) sprintf("%d with no usable simulated distribution", sum(unusable)),
    if (any(constant)) sprintf("%d constant under the model", sum(constant)),
    if (any(repeated)) sprintf("%d identical to another statistic", sum(repeated))
  )
  if (length(not_counted)) {
    head <- paste0(head, " Not counted: ", paste(not_counted, collapse = ", "), ".")
  }
  out_rows <- which(checked & !inside)
  if (length(out_rows)) {
    head <- paste(head, if (multi) {
      tab <- table(factor(g$statistic[out_rows], levels = unique(g$statistic)))
      tab <- tab[tab > 0]
      sprintf("Outside: %s.", paste(sprintf("%s (%d group%s)", names(tab), as.integer(tab),
                                            ifelse(tab == 1L, "", "s")), collapse = ", "))
    } else {
      sprintf("Outside: %s.", paste(sprintf("%s (ppp %.2f)", g$statistic[out_rows],
                                            g$ppp[out_rows]), collapse = ", "))
    })
  }
  lines <- c(head, "  Table: netis_gof(fit). Histograms: plot(fit, what = \"gof\").")

  rng <- function(lo, hi) sprintf("[%s, %s]", formatC(lo, digits = 3, format = "f"),
                                  formatC(hi, digits = 3, format = "f"))
  table <- if (multi) {
    stats_ <- unique(g$statistic)
    data.frame(
      Statistic = stats_,
      Measures = sub(" \\(.*$", "", g$feature[match(stats_, g$statistic)]),
      `Groups inside 95% range` = vapply(stats_, function(s) {
        k <- g$statistic == s & checked
        if (any(k)) sprintf("%d of %d", sum(inside[k]), sum(k)) else "not counted"
      }, character(1)),
      check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
    )
  } else {
    data.frame(
      Statistic = g$statistic,
      Measures = sub(" \\(.*$", "", g$feature),
      Observed = formatC(g$observed, digits = 3, format = "f"),
      `Simulated 95% range` = ifelse(unusable, "not available",
                                     rng(g$predicted_lower, g$predicted_upper)),
      ppp = ifelse(is.na(g$ppp) | constant, "",
                   formatC(g$ppp, digits = 2, format = "f")),
      Counted = ifelse(checked, "yes", ifelse(unusable, "no: not available",
                       ifelse(constant, "no: constant", "no: repeats another"))),
      check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
    )
  }
  list(lines = lines, table = table)
}

#' A formula as one line of text
#'
#' `deparse()` breaks a long formula across several strings, each continuation
#' indented; pasted back together that prints as a run of spaces in the middle
#' of the formula.
#' @noRd
.formula_text <- function(f) {
  gsub("\\s+", " ", paste(deparse(f), collapse = " "))
}

#' Print fit lines, wrapped
#' @noRd
.cat_fit_lines <- function(lines) {
  for (l in lines) {
    indent <- nchar(l) - nchar(sub("^ +", "", l))
    cat(paste(strwrap(l, width = 88, indent = indent, exdent = indent + 2L),
              collapse = "\n"), "\n", sep = "")
  }
  invisible(NULL)
}
