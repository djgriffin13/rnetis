# One row per person per occasion. "node" names the person; everything
# else is a variable measured on them.
people <- data.frame(
  group     = 1,
  node      = c("Ada", "Ben", "Cyd", "Dev", "Eli", "Fay"),
  wave      = rep(1:2, each = 6),
  wellbeing = c(50, 10, 20, 30, 40, 60,    # wave 1
                44, 14, 22, 31, 38, 58),   # wave 2
  tenure    = c(8, 1, 3, 6, 2, 4,          # years; does not change
                8, 1, 3, 6, 2, 4)
)
people
