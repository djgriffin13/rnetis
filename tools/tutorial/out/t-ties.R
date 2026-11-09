# One row per tie. "source" is the person the relation comes FROM and
# "target" the person it goes TO: this row says Ben gives advice to Ada.
advice <- data.frame(
  group    = 1,
  source   = c("Ben", "Cyd", "Dev", "Eli", "Ada", "Cyd", "Ben", "Dev", "Ada"),
  target   = c("Ada", "Ada", "Ada", "Ada", "Ben", "Ben", "Cyd", "Eli", "Dev"),
  strength = c(    1,     3,     2,     1,     2,     1,     1,     1,     1)
)
advice
