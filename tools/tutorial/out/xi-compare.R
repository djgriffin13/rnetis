fit_ar <- fit_influence(affect ~ ar(), data = team)

data.frame(
  model     = c("ar() only", "ar() + exposure()"),
  rows      = c(netis_glance(fit_ar)$nobs, netis_glance(fit)$nobs),
  r.squared = round(c(netis_glance(fit_ar)$r.squared, netis_glance(fit)$r.squared), 3),
  AIC       = round(c(AIC(fit_ar), AIC(fit)), 1),
  BIC       = round(c(BIC(fit_ar), BIC(fit)), 1)
)
