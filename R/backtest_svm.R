.datatable.aware=TRUE
library(data.table)
library(e1071)
library(lubridate)
library(PerformanceAnalytics)

#' Function to replace NAs with zero in a data.table.
#'
#' @name DT_replace_NA
#' @title DT_replace_NA
#' @encoding UTF-8
#' @concept replace NAs with zero in a data.table
#' @param DT data.table which may contain NA values.
#' 
#' @return DT data.table where any NAs have been converted to zero.
#' 
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
DT_replace_NA <- function(DT) {
  for (j in seq_len(ncol(DT)))
    set(DT, which(is.na(DT[[j]])), j, 0)
}

#' Helper function to calculate SVM.
#'
#' @name .calcSVM
#' @title .calcSVM
#' @encoding UTF-8
#' @concept recession forecasting using SVM and backtesting results
#' @param mydat data.table of rawdata
#' @param fmla model formula used to forecast recessions.
#' @param mylag integer number of months to lag the data.
#' @param best_cost integer best_cost or NULL, in which case the best_cost will be determined using k-fold cross-validation.
#' 
#' @return list containing the following outputs:
#' *error* the percentage of months incorrectly classified, 
#' *best_cost* integer value of th best_cost as determined by the k-fold cross validation on the first 14 years of data, and
#' *ans* is the predictions, 1 for recession and -1 for no recession.
#' 
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
.calcSVM <- function(mydat, fmla, mylag, best_cost) {
  
  # Train on the first 14 years of data: 1959-1973 (43-210)
  # use radial basis kernel: K(u,v) = exp(-gamma||u-v||^2)
  # heuristic choice of gamma that equals the inverse of the number of predictors
  # choose softmax based on 10-fold cross validation on EACH of the first 100 months of data
  # compute regret measure for each date by taking the difference between each cross-validation
  # error and the lowest cross-validation error of that date.
  # This measure allows us to compare models across different samples.
  # select softmax that minimizes the total regret over the 100 backtest dates
  
  mydat$vintage <- as.Date(mydat$vintage)
  dates <- sort(unique(mydat$vintage))
  startTrain <- "1967-03-31"
  endTrain <- "1975-06-30"
  start <- which(dates==startTrain)
  end <- which(dates==endTrain)
  
  # Train on data up to 6/30/1975 and use cross validation to find optimal cost
  gamma <- 1 / (ncol(mydat)-2) # 1 / number of predictors
  
  if (is.na(best_cost)) {
    costs <- seq(20, 2000, 20)
    accuracyMat <- matrix(0, ncol=(end-start+1), nrow=length(costs))
    for (i in start:end) {
      for (cc in 1:length(costs)) {
        cost <- costs[cc]
        trainset <- mydat[dat$vintage <= dates[i], -1]
        svm.model <- svm(fmla,
                         data = trainset, 
                         type = "C-classification",
                         kernel = "radial",
                         cross = 10,
                         cost = cost,
                         gamma = gamma)
        accuracyMat[i - start + 1, cc] <- mean(svm.model$accuracies)
      }
    }
    
    # Find highest accuracy and associatd cost
    maxa <- colMeans(accuracyMat)
    maxa[which (max(maxa) == maxa)]
    best_cost <- costs[which (max(maxa) == maxa)]
  }
  
  d0 <- list()
  for (i in (end+1):length(dates)) {
    # i = 737; dates[i]
    trainset <- mydat[mydat$vintage <= dates[i - mylag], -1]
    testset <- mydat[mydat$vintage == dates[i], -c("vintage", "fwdstate")]
    
    svm.model <- svm(fmla, 
                     data = trainset, 
                     type = "C-classification",
                     kernel = "radial",
                     cost = best_cost, 
                     gamma = gamma,
                     probability = TRUE)
    svm.pred <- predict(svm.model, testset, probability = TRUE)
    #ddate <- as.Date(ifelse(i < length(dates), dates[i + 1], dates[i] %m+% months(1)))
    ddate <- ifelse(i < length(dates), dates[i + 1], as.Date(dates[i]) %m+% months(1))
    out <- data.table(vintage = as.Date(ddate, origin = lubridate::origin), 
                      forecast = as.integer(as.character(svm.pred)), 
                      prob = g(attr(svm.pred, "probabilities")[2],2))
    d0[[i]] <- out
    
  }
  out <- rbindlist(d0)
  
  ans <- merge(mydat[, c("vintage","fwdstate")], out, by = "vintage")
  ans <- ans[!is.na(ans$forecast), ]
  ans[fwdstate == -1, fwdstate := 0]
  ans[forecast == -1, forecast := 0]
  error <- 1 - sum(ans$fwdstate==ans$forecast) / nrow(ans)
  return(list(error=error, best_cost=best_cost, ans=ans))
  
}

#' Function to backtest SVM recession predictions.
#'
#' @name bt
#' @title bt
#' @encoding UTF-8
#' @concept recession forecasting using SVM and backtesting results
#' @param mydat data.table of rawdata
#' @param fmla model formula used to forecast recessions
#' @param mylag integer number of months to lag the rawdata to avoid look-ahead bias (12 in this example).
#' @param best_cost integer best_cost or NULL, in which case the best_cost will be determined using k-fold cross-validation.
#' @param flip boolean whether to invest with the prediction or inverse to it. Debault is to long the asset when fwdstate is one and not hold it otherwise.  
#' If flip = TRUE, go long when the fwdstate is zero and not hold when fwdstate is one.
#' 
#' @return list containing the following outputs:
#' *error* the percentage of months incorrectly classified, 
#' *best_cost* integer value of th best_cost as determined by the k-fold cross validation on the first 14 years of data, 
#' *mylag* the number of months the rawdata is lagged before being used by the model,
#' *stats* data.frame of aggregate performance stats generated by PerformanceAnalytics,
#' *calstats* data.frame of calendar-based performance stats generated by PerformanceAnalytics, and
#' *ans* is the predictions, 1 for recession and -1 for no recession.
#' 
#' @examples
#' library(svm.recession)
#' best_cost <- 1120
#' mylag <- 12
#' factors <- "vintage|fwdstate|payems|sp500|yld_curve|man_prod"
#' # Define `fwdstate`
#' dat[, fwdstate := ifelse(recession==1, 1, -1)]
#' fmla = formula(fwdstate ~ .)
#' cols <- grep(factors, names(dat), value = TRUE)
#' mydat <- dat[, ..cols]
#' b1 <- bt(mydat = mydat, fmla = fmla, mylag = mylag, best_cost = best_cost, flip = TRUE)
#' # Model error rate
#' b1$error
#' # Investment strategy preformance
#' b1$stats
#' # Model best cost
#' b1$best_cost
#' # First few predictions
#' tail(b1$ans)
#' 
#' @author Roger J. Bos, \email{roger.bos@@gmail.com}
#' @export
bt <- function(mydat, fmla, mylag, best_cost, flip = FALSE) {
  
  s <- .calcSVM(mydat, fmla, mylag, best_cost)
  # s$error
  # s$best_cost
  # View(s$ans)
  
  cols <- c('vintage', 'ret')
  ret <- merge(dat[, ..cols], s$ans, by = "vintage")
  if (flip) {
    ret[, Iactual := ifelse(fwdstate == 1, 0, 1)]
    ret[, Iforecast := ifelse(forecast == 1, 0 ,1)]
    ret[is.na(Iforecast), Iforecast := 0]
  } else {
    ret[, Iactual := fwdstate]
    ret[, Iforecast := forecast]
    ret[is.na(Iforecast), Iforecast := 1]
  }
  ret <- ret[!is.na(fwdstate), ]
  ret <- ret[!is.na(Iactual), ]
  ret[, buyhold := cumprod(1 + ret)]
  ret[, actual_ret := ret * Iactual]
  ret[, actual := cumprod(1 + actual_ret)]
  ret[, strategy_ret := ret * Iforecast]
  ret[, strategy := cumprod(1 + strategy_ret)]
  
  ret2 <- as.xts(ret[, c("vintage", "strategy_ret", "actual_ret", "ret")])
  stats <- round(rbind(table.AnnualizedReturns(ret2),
                       SharpeRatio(ret2),
                       maxDrawdown(ret2),
                       SemiDeviation(ret2)), 4)
  
  calstats <- table.CalendarReturns(ret2)
  return(list(error = s$error, best_cost = s$best_cost, mylag = mylag, stats = stats, calstats  = calstats, ans = s$ans))
  
}
