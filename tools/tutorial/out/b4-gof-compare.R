# The question B4 exists to answer, as one table: did the latent
# dimensions move the triadic statistics toward the middle?
a <- netis_gof(sel); m <- netis_gof(ame2)
data.frame(
  statistic = a$statistic,
  z_additive = round(a$z, 2),
  z_ame      = round(m$z, 2),
  ppp_additive = a$ppp,
  ppp_ame      = m$ppp
)
