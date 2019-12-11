---
title: "README"
author: "Roger J Bos"
date: "12/12/2019"
output: html_document
---

### Installation

```
library(devtools)
install_github("rogerjbos/svm_recession)
library(svm.recession)
```

### Data and sources

The sample data will automatically load and be stored in the object `dat`.  Here are the important variables.

`fwdstate` is 1 if the next month is in a recession and -1 otherwise.
The model is `fwdstate ~ payems + sp500 + yld_curve + man_prod`
where each of the four independant variables includes the current value plus the most recent 11 lagged months, for a total of 48 variables.

* Recessions periods are from the [NBER](https://www.nber.org/cycles.html).
* `payems` is total nonfarm employees from [ALFRED](https://alfred.stlouisfed.org/series?seid=PAYEMS).
* `sp500` is the [S&P 500 monthly closing price](https://www.multpl.com/s-p-500-historical-prices/table/by-month).
* `yld_curve` is [GS10](https://fred.stlouisfed.org/series/GS10) - [FEDFUNDS](https://fred.stlouisfed.org/series/FEDFUNDS) from [FRED](https://fred.stlouisfed.org).
* `man_prod` is the Manufacturing Production Index as published by the Institute for Supply Management(https://www.quandl.com/data/ISM-Institute-for-Supply-Management) downloaded from [Quandl](https://www.quandl.com/data/ISM/MAN_PROD-Manufacturing-Production-Index).

### Running the example

```{r}

library(svm.recession)
# load the sample data (should happen automatically -- object named `dat`)

# Set best_cost to NA to determine best_cost
best_cost <- 1120 
# Lag the independent variables by 12 months so there is no look ahead bias
mylag <- 12
# Determine which factors to include in the model
factors <- "vintage|fwdstate|payems|sp500|yld_curve|man_prod"

# Define `fwdstate`
dat[, fwdstate := ifelse(recession==1, 1, -1)]
fmla = formula(fwdstate ~ .)
cols <- grep(factors, names(dat), value = TRUE)
mydat <- dat[, ..cols]

# Run the backtest
b1 <- bt(mydat = mydat, fmla = fmla, mylag = mylag, best_cost = best_cost, flip = TRUE)

# Model error rate
b1$error

# Investment strategy preformance
b1$stats  

# Model best cost
b1$best_cost

# First few predictions
tail(b1$ans)

# Plot the results
layout(1:2)
plot(b1$ans$fwdstate ~ b1$ans$vintage, type='l', lty='dotted', 
     main='NBER Recessions', xlab='', ylab='Recessions')
lines(b1$ans$forecast ~ b1$ans$vintage, type='s', col='red')
plot(b1$ans$prob ~ b1$ans$vintage, type='l', col='red',
      main='Forecast Recession Probabilities', ylab='Probability')
lines(b1$ans$fwdstate ~ b1$ans$vintage, type='s')

```

### Investment Strategy

The backtest measures the accuracy of the SVM predictions versus the actual recession markers as determined by the NBER.

From an investment strategy standpoint, we go long the S&P 500 when the recession is not forecasted and switch to cash when recession is forecasted.  This provides a modest outperformance versus a buy and hold strategy.

### References

This paper was modeled after [Nowcasting Recessions using the SVM Machine Learning Algorithm](https://ssrn.com/abstract=3316917) by James, Abu-Mostafa, and Qiao.  Available on SSRN at <https://ssrn.com/abstract=3316917>.
