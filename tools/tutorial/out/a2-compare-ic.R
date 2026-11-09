# Information criteria compare models fitted to THE SAME rows. These two
# are, so the comparison is admissible.
plain <- fit_influence(y ~ ar(), data = demo_influence)
withx <- fit_influence(y ~ ar() + exposure(y, via = tie, normalize = "row"),
                       data = demo_influence)

data.frame(
  model = c("ar() only", "ar() + exposure()"),
  nobs  = c(netis_glance(plain)$nobs, netis_glance(withx)$nobs),
  r2    = round(c(netis_glance(plain)$r.squared, netis_glance(withx)$r.squared), 4),
  rmse  = round(c(netis_glance(plain)$rmse, netis_glance(withx)$rmse), 4),
  AIC   = round(c(AIC(plain), AIC(withx)), 2),
  BIC   = round(c(BIC(plain), BIC(withx)), 2)
)
