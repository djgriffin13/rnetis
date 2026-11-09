# The chain settings, all in one place. These are the ones used on this
# page; none of them is a default to accept without looking.
sel <- fit_selection(
  tie ~ sender(tenure) + receiver(tenure) + dyad(same_unit) + prior_tie(),
  data   = staff,
  nscan  = 5000,   # draws kept after burn-in
  burn   = 1000,   # draws discarded first
  odens  = 10,     # thinning: keep one draw in ten
  R      = 0,      # dimension of the multiplicative term
  seed   = 1
)
sel
