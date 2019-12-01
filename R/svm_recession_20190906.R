if (FALSE) {
  
  #load("data/recession.Rdata")
  dim(dat)
  best_cost <- 1120
  lag <- 12
  
  # new code
  factors <- "vintage|fwdstate|payems|sp500|yld_curve|man_prod" # .0528
  
  dat[, sp500.13 := NULL]
  dat[, sp500.14 := NULL]
  dat[, sp500.15 := NULL]
  dat[, sp500.16 := NULL]
  dat[, sp500.17 := NULL]
  dat[, sp500.18 := NULL]
  dat[, sp500.19 := NULL]
  dat[, sp500.20 := NULL]
  dat[, sp500.21 := NULL]
  dat[, sp500.22 := NULL]
  dat[, sp500.23 := NULL]
  dat[, sp500.24 := NULL]
  
  dat[, fwdstate := ifelse(recession==1, 1, -1)] #.0971 .7212
  fmla = formula(fwdstate ~ .)
  cols <- grep(factors, names(dat), value = TRUE)
  mydat <- dat[, ..cols]
  
  # tmp <- mydat[, c("vintage","fwdstate")]
  # View(tmp)
  # ret[fwdstate == 1, mean(ret)]
  # ret[fwdstate == 0, mean(ret)]
  # ret[forecast == 1, mean(ret)]
  # ret[forecast == 0, mean(ret)]
  # summary(ret$ret)
  
  b1 <- bt(mydat = mydat, fmla = fmla, mylag = 12, best_cost = NA, flip = TRUE)
  b1$stats  
  b1$error
  b1$best_cost
  tail(b1$ans)
  
  # strategy_ret actual_ret    ret
  # Annualized Return                   0.0924     0.0980 0.0813
  # Annualized Std Dev                  0.1339     0.1288 0.1472
  # Annualized Sharpe (Rf=0%)           0.6901     0.7613 0.5521
  
  layout(1:2)
  plot(b1$ans$fwdstate ~ b1$ans$vintage, type='l', lty='dotted', 
       main='NBER Recessions', xlab='', ylab='Recessions')
  lines(b1$ans$forecast ~ b1$ans$vintage, type='s', col='red')
  plot(b1$ans$prob ~ b1$ans$vintage, type='l', col='red',
        main='Forecast Recession Probabilities', ylab='Probability')
  lines(b1$ans$fwdstate ~ b1$ans$vintage, type='s')
  
  library(ggplot2)
  ggplot(b1$ans, aes(vintage, y = fwdstate)) +
    geom_line() +
    geom_line(data = b1$ans, aes(vintage, y = forecast), colour = 'red', size = 1)
  ggplot(b1$ans, aes(vintage, y = fwdstate)) +
    geom_line() +
    geom_line(data = b1$ans, aes(vintage, y = prob), colour = 'red', size = 1)

}
