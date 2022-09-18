library(PerformanceAnalytics)

# transform portf into xts format so that CAPM function could operate
portf <- as.data.frame(portf)
mkt_xts <- xts(portf[, 5], order.by = portf[, 4])
RF_xts <- xts(portf[, 3], order.by = portf[, 4])
sd_xts <- xts(portf[, 16:20], order.by = portf[, 4])

alphas <- CAPM.alpha(sd_xts, mkt_xts, RF_xts)
betas <- CAPM.beta(sd_xts, mkt_xts, RF_xts)