# Capture harness for the rnetis tutorial.
#
# Every code block shown in TUTORIAL.html is evaluated here and its real
# printed output written to disk. The code string is the single source of
# truth: the builder reads the same file it was evaluated from, so a block
# and its output cannot drift apart.
#
#   Rscript cap.R <outdir>

args <- commandArgs(trailingOnly = TRUE)
OUT <- if (length(args)) args[[1]] else "out"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT, "png"), showWarnings = FALSE, recursive = TRUE)

options(width = 88, digits = 5)
set.seed(1)

suppressPackageStartupMessages(library(rnetis))

# Load the backends up front. Attaching lme4 after ergm prints an S3-method
# notice, and that notice is an artefact of load order in THIS capture
# session, not part of any result. Getting it out of the way here means no
# shown block has to be edited after the fact.
suppressPackageStartupMessages({
  library(lme4)
  if (requireNamespace("amen", quietly = TRUE)) library(amen)
  if (requireNamespace("latentnet", quietly = TRUE)) library(latentnet)
})

.started <- Sys.time()
.log <- function(...) cat(sprintf("[%6.1fs] ", as.numeric(difftime(Sys.time(), .started, units = "secs"))), ..., "\n", sep = "")

# ---------------------------------------------------------------------------
# cap(): evaluate a code string in the shared tutorial environment, capture
# everything the reader would see, and record both.
#
# `expect_error = TRUE` marks a block whose point IS the refusal. The error is
# captured and written as output rather than stopping the script; a block that
# is supposed to fail and does not is reported, because a refusal page that
# shows no refusal is worse than none.
# ---------------------------------------------------------------------------
ENV <- new.env(parent = globalenv())

cap <- function(id, code, expect_error = FALSE) {
  .log("cap  ", id)
  writeLines(code, file.path(OUT, paste0(id, ".R")))
  exprs <- parse(text = code)
  out <- character(0)
  failed <- FALSE
  con <- textConnection("captured", "w", local = TRUE)
  sink(con, type = "output")
  sink(con, type = "message")
  res <- tryCatch({
    for (e in exprs) {
      v <- withVisible(withCallingHandlers(
        eval(e, envir = ENV),
        warning = function(w) {
          cat("Warning message:\n", conditionMessage(w), "\n", sep = "")
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          cat(conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      ))
      if (v$visible) print(v$value)
    }
    NULL
  }, error = function(e) e)
  sink(type = "message")
  sink(type = "output")
  close(con)
  out <- captured
  if (!is.null(res)) {
    failed <- TRUE
    out <- c(out, paste0("Error: ", conditionMessage(res)))
  }
  if (expect_error && !failed) {
    .log("  !! ", id, " was expected to refuse and did not")
    writeLines("NOT-REFUSED", file.path(OUT, paste0(id, ".problem")))
  }
  if (!expect_error && failed) {
    .log("  !! ", id, " failed unexpectedly: ", conditionMessage(res))
    writeLines(conditionMessage(res), file.path(OUT, paste0(id, ".problem")))
  }
  writeLines(out, file.path(OUT, paste0(id, ".txt")))
  invisible(out)
}

# capfig(): same, but the last expression is a ggplot saved as a PNG.
capfig <- function(id, code, width = 7.2, height = 4.6, dpi = 150) {
  .log("fig  ", id)
  writeLines(code, file.path(OUT, paste0(id, ".R")))
  exprs <- parse(text = code)
  con <- textConnection("captured", "w", local = TRUE)
  sink(con, type = "output"); sink(con, type = "message")
  res <- tryCatch({
    val <- NULL
    for (e in exprs) {
      val <- withCallingHandlers(
        eval(e, envir = ENV),
        warning = function(w) { cat("Warning message:\n", conditionMessage(w), "\n", sep = ""); invokeRestart("muffleWarning") },
        message = function(m) { cat(conditionMessage(m)); invokeRestart("muffleMessage") }
      )
    }
    ggplot2::ggsave(file.path(OUT, "png", paste0(id, ".png")), plot = val,
                    width = width, height = height, dpi = dpi, bg = "white")
    NULL
  }, error = function(e) e)
  sink(type = "message"); sink(type = "output")
  close(con)
  out <- captured
  if (!is.null(res)) {
    .log("  !! ", id, " figure failed: ", conditionMessage(res))
    writeLines(conditionMessage(res), file.path(OUT, paste0(id, ".problem")))
    out <- c(out, paste0("Error: ", conditionMessage(res)))
  }
  writeLines(out, file.path(OUT, paste0(id, ".txt")))
  invisible(NULL)
}

# A plain value recorded for the prose to quote, so no number in the text is
# typed by hand.
fact <- function(id, expr) {
  v <- eval(parse(text = expr), envir = ENV)
  writeLines(as.character(v), file.path(OUT, paste0("fact-", id, ".txt")))
  invisible(v)
}
