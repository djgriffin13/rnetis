# Column names the plot layer references through ggplot2's aesthetics.
#
# WHY THIS FILE EXISTS. `aes(x = term, y = value)` names COLUMNS of the data
# frame being drawn, not objects in the package's namespace. R CMD check's
# static analyser cannot tell the two apart, so every such name is reported as
# "no visible binding for global variable". Declaring them here is the standard remedy, and it is a declaration to the
# CHECKER rather than a change to any behavior.
#
# THE LIST IS THE PLOT LAYER'S COLUMN VOCABULARY, which is the reason to keep it
# explicit rather than silencing the note some broader way. Every name below is
# a column some `plot_data()` method promises to produce, and a plot that
# referenced a column no method produced would still fail at draw time -- but
# adding a name here without adding the column is at least a visible edit in a
# file that says what these are.
#
# Grouped by where they come from, so a reader can tell a coefficient-table
# column from a network-layout one.
utils::globalVariables(c(
  # Estimate tables: netis_estimates() and the panels built on it.
  "term", "value", "lower", "upper", "level", "missing_state",
  # Selection actor effects, which carry a sender and a receiver interval.
  "sender_lower", "sender_upper", "receiver_lower", "receiver_upper",
  # Network drawings: node positions and the segment endpoints of each tie,
  # produced by the layout layer.
  "x", "y", "xend", "yend", "node", "degree",
  # Data-layer summaries.
  "wave", "n", "family", "draw",
  # Coverage and missingness panels: the (group, node) axis label.
  "unit", "effect",
  # The posterior predictive check: netis_gof()'s columns. `observed` and the
  # two `predicted_*` bounds are two different quantities on one panel -- where
  # the statistic fell against where the model said it would -- so they are
  # NOT the `lower`/`upper`/`value` triple above, and must not be made to share
  # those names.
  "grouping", "statistic", "observed", "predicted_lower", "predicted_upper",
  # The same panel's in-panel label, built from `ppp`.
  "ppp_label",
  # The latent-positions panel.
  "x", "y", "outcome", "node"
))
