#' Autoregressive term for an Influence model formula
#'
#' Used inside a [fit_influence()] formula to specify the autoregressive
#' (prior-state) predictor: the outcome's own value at earlier wave(s),
#' resolved from the `netis_data`'s wave metadata rather than requiring the
#' user to pre-lag anything.
#'
#' `ar()` has exactly one meaning in an Influence formula: lagged values of
#' the dependent variable. It therefore takes **no variable argument** --
#' the autoregressive variable is read automatically from the formula's
#' left-hand side. `ar(y)` (a variable argument) is not valid. Writing a
#' variable there is an error, naming [lagged()] as the replacement when the
#' variable is not the outcome.
#'
#' If you want to lag a variable other than the outcome, use [lagged()]
#' instead: `ar()` is "lag the outcome," `lagged(x)` is "lag anything else."
#'
#' `ar()` (and [exposure()], [latent()], [lagged()]) are only meaningful
#' inside a [fit_influence()] formula; calling it directly returns the
#' captured specification for inspection (with `variable = NULL`, since the
#' outcome is not known outside a formula), but does not compute anything.
#'
#' @param lag one or more wave lags, each a whole number of at least 1.
#'   Default `1` (the immediately preceding wave). A vector such as `1:2`
#'   requests multiple lag terms, when enough waves are available. A lag of 0
#'   would be the outcome itself, and a negative lag a later wave, so both are
#'   refused, as is a fractional lag. Lags count positions back through the
#'   fit's waves: with `waves = c(from, to)`, lag 1 is `from`, and a deeper lag
#'   steps back from `from` through the earlier observed waves.
#' @return a list describing the term, of class `netis_ar_term`.
#' @seealso [lagged()], [exposure()], [latent()], [fit_influence()]
#' @examples
#' ar()
#' ar(lag = 1:2)
#' @export
ar <- function(lag = 1) {
  structure(
    list(variable = NULL, lag = .assert_wave_lag(lag, "ar(lag = )", min = 1L, scalar = FALSE)),
    class = "netis_ar_term"
  )
}

#' Lagged-covariate term for an Influence model formula
#'
#' Used inside a [fit_influence()] formula to specify a predictor's value at
#' an earlier wave, resolved from the `netis_data`'s wave metadata rather
#' than requiring the user to pre-lag anything, as [ar()] and [exposure()] do.
#' `var` is captured as a column name, not evaluated.
#'
#' A bare covariate in the formula (`y ~ x`) is measured at the *focal*
#' wave -- the same wave as the outcome. `lagged(x)` is the prior wave.
#' Both are expressible and neither is implied by the other.
#'
#' # Level resolution
#'
#' `var`'s level (node or group) is resolved automatically from the
#' `netis_data` schema at fit time -- there is no separate
#' `lagged_node()`/`lagged_group()`, because the schema already knows which
#' one applies:
#' \itemize{
#'   \item a **node** variable lags to that same node's value at the
#'     earlier wave;
#'   \item a **group** variable lags to that node's group's value at the
#'     earlier wave, joined onto every member of the group;
#'   \item a **tie** variable is not supported and errors clearly;
#'   \item a variable that is genuinely constant across waves (static) can
#'     still be lagged -- the result is simply the same value, and a NOTE
#'     is added to the fit's diagnostics, since a lag that changes nothing
#'     is often a sign the wrong variable was named, though sometimes
#'     exactly what was intended.
#' }
#'
#' @param var the node or group variable to lag.
#' @param lag one or more wave lags, each a whole number of at least 1.
#'   Default `1` (the immediately preceding wave). A vector such as `1:2`
#'   requests multiple lag terms, when enough waves are available. A lag of 0
#'   is the focal wave, which a bare covariate already reads, and a negative lag
#'   a later wave, so both are refused, as is a fractional lag. Lags count
#'   positions as for [ar()].
#' @return a list describing the term, of class `netis_lagged_term`.
#' @seealso [ar()], [exposure()], [latent()], [fit_influence()]
#' @examples
#' lagged(stress)
#' lagged(group_climate, lag = 2)
#' @export
lagged <- function(var, lag = 1) {
  structure(
    list(variable = deparse(substitute(var)),
         lag = .assert_wave_lag(lag, "lagged(lag = )", min = 1L, scalar = FALSE)),
    class = "netis_lagged_term"
  )
}

#' Exposure term for an Influence model formula
#'
#' Used inside a [fit_influence()] formula to specify a network exposure
#' predictor -- see [compute_exposure()] for the underlying computation and
#' its Direction/Isolates documentation, which applies identically here.
#' `content` and `via` are captured as column names, not evaluated.
#'
#' # Self-ties
#'
#' Under `netis_data(self_ties = "prohibited")`, the default, a self-tie row with a
#' nonzero value is refused with an error when the exposure is computed: it would
#' expose a node to its own value. Remove the row, or declare
#' `self_ties = "permitted"`, in which case the self-tie enters the network as an
#' ordinary tie and the node's own value contributes to its exposure. A stored zero
#' on the diagonal is not a tie and is accepted.
#'
#' @param content name of the node variable whose values flow through the
#'   network.
#' @param via the conduit: a tie-variable name, or a network specification
#'   built with [binarize()], [symmetrize()], or [multiplex()]. Exposure from
#'   ties in **either** direction is `symmetrize()` rather than a third
#'   `direction` value -- symmetrization is a substantive, recorded choice.
#'   See [compute_exposure()] for the full table.
#' @param name name for this exposure term/column. Defaults to
#'   `"ex_<content>_<via>"`, as in [compute_exposure()]. Required when a
#'   formula includes more than one `exposure()` call that would otherwise
#'   collide on this default. A mechanism with several columns (such as
#'   [bi_components()]) names its own columns; a `name` you give prefixes them
#'   (`<name>_A_maxpath`), and two such terms in one formula need one each. The
#'   names of the table's group, node and outcome columns are reserved.
#' @param x_lag wave lag for `content`: a single whole number, `0` (the
#'   outcome wave, a contemporaneous exposure) or more. Default `1`. Negative
#'   and fractional lags are refused. For a history mechanism such as [bi()],
#'   `x_lag` is the lag of the most recent content the accumulation reads:
#'   the default `1` resolves the mechanism at the outcome wave, so it reads
#'   content and networks at every observed wave before it; `x_lag = 0` is
#'   refused for such a mechanism.
#' @param w_lag wave lag for the conduit network `via`, a single whole number
#'   of at least 0. Defaults to `x_lag` when `NULL` (the common case: content
#'   and conduit measured at the same prior wave). Set separately when they
#'   are timed differently; not available with a history mechanism.
#' @param normalize `"none"` (default) or `"row"`. See [compute_exposure()].
#' @param mechanism `"linear"` (default) or a kernel such as [bi()]. See
#'   [compute_exposure()].
#' @param direction `"source_influences_target"` (default) or
#'   `"target_influences_source"`. See [compute_exposure()]'s Direction
#'   section -- picking the wrong one silently reverses who is exposed to
#'   whom.
#' @param isolate_rule `"na"` (default), `"zero"`, or `"error"`. Only
#'   relevant when `normalize = "row"`. See [compute_exposure()].
#' @return a list describing the term, of class `netis_exposure_term`.
#' @references
#' Friedkin, N. E. and Johnsen, E. C. (1990) Social influence and opinions.
#' *Journal of Mathematical Sociology* **15**(3-4), 193-206.
#'
#' Marsden, P. V. and Friedkin, N. E. (1993) Network studies of social
#' influence. *Sociological Methods & Research* **22**(1), 127-151.
#'
#' Leenders, R. Th. A. J. (2002) Modeling social influence through network
#' autocorrelation: constructing the weight matrix. *Social Networks* **24**(1),
#' 21-47. For row normalization.
#' @seealso [ar()], [latent()], [compute_exposure()], [fit_influence()]
#' @examples
#' exposure(negative_affect, via = info_receipt)
#' exposure(negative_affect, via = info_receipt, mechanism = bi(delta = 0.5, alpha = 1, gamma = 0))
#' @export
exposure <- function(content, via, name = NULL,
                      x_lag = 1L, w_lag = NULL,
                      normalize = c("none", "row"),
                      mechanism = "linear",
                      direction = c("source_influences_target", "target_influences_source"),
                      isolate_rule = c("na", "zero", "error")) {
  normalize <- match.arg(normalize)
  direction <- match.arg(direction)
  isolate_rule <- match.arg(isolate_rule)
  if (missing(via)) {
    stop("exposure() needs `via`, the network the content flows through: exposure(x, via = tie).",
         call. = FALSE)
  }
  content_expr <- substitute(content)
  structure(
    list(
      # A quoted name is the same name.
      content = if (is.character(content_expr)) content_expr else deparse(content_expr),
      # `via` goes through the shared network-argument capture rather than
      # deparse(): a bare symbol stays a tie-variable name (unchanged), and a
      # binarize()/symmetrize()/multiplex() call is EVALUATED into a network
      # specification. deparse() would have turned the latter into the literal
      # text of the call, which then failed as a missing column name.
      conduit = .as_conduit(.capture_network_arg(substitute(via), parent.frame())),
      name = name,
      x_lag = .assert_wave_lag(x_lag, "exposure(x_lag = )", min = 0L),
      w_lag = if (is.null(w_lag)) NULL else .assert_wave_lag(w_lag, "exposure(w_lag = )", min = 0L),
      normalize = normalize,
      mechanism = mechanism,
      direction = direction,
      isolate_rule = isolate_rule
    ),
    class = "netis_exposure_term"
  )
}


#' Node-level network-derived control term for an Influence model formula
#'
#' Used inside a [fit_influence()] formula to add a node-level structural
#' quantity -- degree, strength, or (with the `igraph` package installed)
#' betweenness, closeness, eigenvector centrality, PageRank, or constraint --
#' as an ordinary numeric predictor, entered alongside [ar()], [exposure()],
#' and [latent()] terms. Follows the same architecture as those: `centrality()`
#' constructs a specification and computes nothing; an engine resolves it into
#' a predictor column.
#'
#' `network` is captured as a bare tie-variable column name, not evaluated --
#' or the result of a [binarize()]/[symmetrize()]/[multiplex()] call, since
#' every network-taking term in this package accepts the same network
#' specification. If omitted, it is inferred when exactly one
#' tie variable exists in the data and is an error naming the candidates
#' otherwise -- never guessed among several.
#'
#' # Direction is substantive, never computational
#'
#' `mode = "in"`/`"out"` refer to the **declared** edge direction recorded in
#' `netis_data` -- "how many ties point at this node" under whatever
#' semantic the data declares (e.g. "sends information to"). This is
#' deliberately **not** the exposure engine's canonical `W[i,j] = j
#' influences i` orientation ([exposure()], [compute_exposure()]); reusing
#' that transposed convention here would silently reverse in-degree and
#' out-degree for any network whose declared direction runs the other way.
#' `mode` has no effect on an undirected network. Of the `igraph`-backed measures
#' only `"closeness"` has an in/out reading, so `mode = "in"` or `"out"` on the
#' others is refused.
#'
#' # Weighting
#'
#' `measure = "degree"` always counts ties (ignoring magnitude, resolved via
#' the network's declared presence semantics -- see [binarize()]'s
#' `presence()` rule for what that means), and asking it to use weights is
#' refused; `measure = "strength"` always sums the tie values. This is igraph's own [igraph::degree()] /
#' [igraph::strength()] distinction. For igraph-backed measures,
#' `weights = "auto"` (default) follows the resolved network's own weighted/
#' binary classification; `"weighted"`/`"unweighted"` override it explicitly.
#'
#' @param network a tie-variable name, or a [binarize()]/[symmetrize()]/
#'   [multiplex()] result. Inferred when omitted and there is exactly one tie
#'   variable in the data.
#' @param measure one of `"degree"`, `"strength"` (native, no `igraph`
#'   dependency), or `"betweenness"`, `"closeness"`, `"eigenvector"`,
#'   `"pagerank"`, `"constraint"` (require the `igraph` package).
#' @param mode `"all"` (default), `"in"`, or `"out"`. See Direction above.
#' @param weights `"auto"` (default), `"weighted"`, or `"unweighted"`. See
#'   Weighting above.
#' @param weight_transform `NULL` (default), or [inverse()], applied to the edge
#'   weights before the measure reads them. A
#'   measure reads a larger weight either as a shorter distance (`"betweenness"`,
#'   `"closeness"`) or as a stronger tie (the others that use weights). A network
#'   whose declared `weight_semantics` means the other thing is refused unless a
#'   transform is given. A transform on a computation that reads no weights is an
#'   error.
#' @param lag wave lag for the network, resolved the same way [latent()]'s
#'   `lag` is: a single whole number, `0` for the outcome wave's network or
#'   more for an earlier one. Default `1`.
#' @param name name for this term/column. Defaults to
#'   `"cent_<measure>_<network>"` when `network` is a bare tie-variable name,
#'   or `"cent_<measure>"` otherwise. Required when a formula includes more
#'   than one `centrality()` call that would otherwise collide on this
#'   default.
#' @return a list describing the term, of class `netis_centrality_term`.
#' @references
#' Bonacich, P. (1987) Power and centrality: a family of measures. *American
#' Journal of Sociology* **92**(5), 1170-1182. For `"eigenvector"`.
#'
#' Brin, S. and Page, L. (1998) The anatomy of a large-scale hypertextual Web
#' search engine. *Computer Networks and ISDN Systems* **30**, 107-117. For
#' `"pagerank"`.
#'
#' Burt, R. S. (2004) Structural holes and good ideas. *American Journal of
#' Sociology* **110**(2), 349-399. For `"constraint"`.
#'
#' Csardi, G. and Nepusz, T. (2006) The igraph software package for complex
#' network research. *InterJournal*, Complex Systems, 1695. The implementation of
#' every measure other than `"degree"` and `"strength"`.
#'
#' Freeman, L. C. (1978) Centrality in social networks: conceptual clarification.
#' *Social Networks* **1**(3), 215-239. For `"degree"`, `"betweenness"` and
#' `"closeness"`.
#' @seealso [network_stat()], [ar()], [exposure()], [latent()],
#'   [compute_centrality()], [fit_influence()]
#' @examples
#' centrality(communication, measure = "degree")
#' centrality(communication, measure = "degree", mode = "in")
#' @export
centrality <- function(network, measure, mode = c("all", "in", "out"),
                        weights = c("auto", "weighted", "unweighted"),
                        weight_transform = NULL, lag = 1, name = NULL) {
  mode <- match.arg(mode)
  weights <- match.arg(weights)
  d <- .netfeat_descriptor(measure)
  if (!identical(d$level, "node")) {
    stop(
      sprintf("centrality() measures must be node-level; '%s' is group-level. Use network_stat() instead.", measure),
      call. = FALSE
    )
  }
  net_spec <- if (missing(network)) NULL else .capture_network_arg(substitute(network), parent.frame())
  structure(
    list(
      network = net_spec, measure = measure, mode = mode, weights = weights,
      weight_transform = weight_transform,
      lag = .assert_wave_lag(lag, "centrality(lag = )", min = 0L), name = name
    ),
    class = "netis_centrality_term"
  )
}

#' Group-level network-derived control term for an Influence model formula
#'
#' Used inside a [fit_influence()] formula to add a group-level structural
#' quantity -- density, reciprocity -- as an ordinary numeric predictor,
#' fanned out to every member row of that group. Follows the same
#' architecture as [centrality()], [ar()], [exposure()], and [latent()].
#'
#' # Identification
#'
#' A group-level predictor is estimable only when it actually varies at a
#' level the design identifies. With a single group and one resolved
#' snapshot, the value is a constant -- perfectly collinear with the
#' intercept -- and `fit_influence()` refuses rather than silently returning
#' an aliased coefficient. Across multiple groups it is a between-group
#' predictor whose inferential information is carried by the **number of
#' groups**, not the number of rows: five groups give five observations of
#' group density, however many people are in them.
#'
#' # Distributional summaries of a node-level feature
#'
#' `summary = "mean"` (or `"sd"`, `"var"`, `"median"`, `"min"`, `"max"`,
#' `"prop_zero"`) turns `measure` into a *node-level* measure ("degree",
#' "strength", ...) whose values are aggregated within the resolved snapshot
#' into a single group-level number -- mean degree, SD of betweenness, and
#' so on -- reusing the exact node-level computation [centrality()] uses,
#' not a parallel calculation. `"prop_zero"` is "proportion of nodes at the
#' measure's zero point" -- for `measure = "degree"` this is exactly
#' "proportion of isolates."
#' The summary runs over the nodes observed at the resolved wave.
#'
#' @inheritParams centrality
#' @param measure a group-level measure (`"density"`, `"reciprocity"`) when
#'   `summary` is `NULL` (default), or a node-level measure (`"degree"`,
#'   `"strength"`, or an `igraph`-backed one) when `summary` is given. See
#'   Distributional summaries above.
#' @param summary `NULL` (default) for a genuinely group-level `measure`, or
#'   one of `"mean"`, `"sd"`, `"var"`, `"median"`, `"min"`, `"max"`,
#'   `"prop_zero"` to aggregate a node-level `measure`.
#' @param name name for this term/column. Defaults to
#'   `"net_<measure>_<network>"` when `network` is a bare tie-variable name,
#'   or `"net_<measure>"` otherwise.
#' @return a list describing the term, of class `netis_network_stat_term`.
#' @references
#' Wasserman, S. and Faust, K. (1994) *Social Network Analysis: Methods and
#' Applications*. Cambridge University Press. For `"density"` and
#' `"reciprocity"`, standard descriptive measures; reciprocity here is the share
#' of present directed ties that are reciprocated.
#'
#' Bonacich, P. (1987) Power and centrality: a family of measures. *American
#' Journal of Sociology* **92**(5), 1170-1182. For `"eigenvector"`.
#'
#' Brin, S. and Page, L. (1998) The anatomy of a large-scale hypertextual Web
#' search engine. *Computer Networks and ISDN Systems* **30**, 107-117. For
#' `"pagerank"`.
#'
#' Burt, R. S. (2004) Structural holes and good ideas. *American Journal of
#' Sociology* **110**(2), 349-399. For `"constraint"`.
#'
#' Csardi, G. and Nepusz, T. (2006) The igraph software package for complex
#' network research. *InterJournal*, Complex Systems, 1695. The implementation of
#' every measure other than `"degree"` and `"strength"`.
#'
#' Freeman, L. C. (1978) Centrality in social networks: conceptual clarification.
#' *Social Networks* **1**(3), 215-239. For `"degree"`, `"betweenness"` and
#' `"closeness"`.
#' @seealso [centrality()], [ar()], [exposure()], [latent()],
#'   [compute_network_stat()], [fit_influence()]
#' @examples
#' network_stat(communication, measure = "density")
#' network_stat(communication, measure = "degree", summary = "mean")
#' @export
network_stat <- function(network, measure, summary = NULL,
                          weights = c("auto", "weighted", "unweighted"),
                          weight_transform = NULL, lag = 1, name = NULL) {
  weights <- match.arg(weights)
  d <- .netfeat_descriptor(measure)
  if (is.null(summary)) {
    if (!identical(d$level, "group")) {
      stop(
        sprintf(
          "network_stat() measures must be group-level unless summary = is given; '%s' is node-level. Use centrality() instead, or add summary = \"mean\" (or another summary) to aggregate it.",
          measure
        ),
        call. = FALSE
      )
    }
  } else {
    if (!identical(d$level, "node")) {
      stop(
        sprintf("summary = is only meaningful for a node-level measure; '%s' is already group-level.", measure),
        call. = FALSE
      )
    }
    .assert_enum(summary, .netfeat_summary_types, "summary")
  }
  net_spec <- if (missing(network)) NULL else .capture_network_arg(substitute(network), parent.frame())
  structure(
    list(
      network = net_spec, measure = measure, summary = summary, weights = weights,
      weight_transform = weight_transform,
      lag = .assert_wave_lag(lag, "network_stat(lag = )", min = 0L), name = name
    ),
    class = "netis_network_stat_term"
  )
}
