library(PerformanceAnalytics)
library(e1071)

DT_replace_NA <- function(DT) {
  for (j in seq_len(ncol(DT)))
    set(DT, which(is.na(DT[[j]])), j, 0)
}

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

  # Train on data up to 12/31/1972 and use cross validation to find optimal cost
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
    best_cost
  }
  
  #best_cost = 1060
  d0 <- list()
  for (i in (end+1):length(dates)) {
    # i = 737; dates[i]
    trainset <- mydat[mydat$vintage <= dates[i - mylag], -1]
    testset <- mydat[mydat$vintage == dates[i], -c("vintage","fwdstate")]
    
    svm.model <- svm(fmla, 
                     data = trainset, 
                     type = "C-classification",
                     kernel = "radial",
                     cost = best_cost, 
                     gamma = gamma,
                     probability = TRUE)
    svm.pred <- predict(svm.model, testset, probability = TRUE)
    ddate <- as.Date(ifelse(i < length(dates), dates[i + 1], dates[i] %m+% months(1)))
    out <- data.table(vintage = ddate, forecast = as.integer(as.character(svm.pred)), prob = g(attr(svm.pred, "probabilities")[2],2))
    d0[[i]] <- out
    
  }
  out <- rbindlist(d0)
  
  ans <- merge(mydat[, c("vintage","fwdstate")], out, by="vintage")
  ans <- ans[!is.na(ans$forecast), ]
  ans[fwdstate == -1, fwdstate := 0]
  ans[forecast == -1, forecast := 0]
  error <- 1 - sum(ans$fwdstate==ans$forecast) / nrow(ans)
  return(list(error=error, best_cost=best_cost, ans=ans))
  
}

bt <- function(mydat, fmla, mylag, best_cost, flip = FALSE) {
  
  # best_cost <- 260; mylag <- 12
  
  # names(mydat); rm(s)
  s <- .calcSVM(mydat, fmla, mylag, best_cost)
  # s$error
  # s$best_cost
  # View(s$ans)
  
  cols <- c('vintage', 'ret')
  ret <- merge(dat[, ..cols], s$ans, by="vintage")
  if (flip) {
    ret[, Iactual := ifelse(fwdstate == 1, 0, 1)]
    ret[, Iforecast := ifelse(forecast == 1, 0 ,1)]
    #sum(is.na(ret$Iforecast))
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
  ret[, actual := cumprod(1+actual_ret)]
  ret[, strategy_ret := ret * Iforecast]
  ret[, strategy := cumprod(1+strategy_ret)]
  
  ret2 <- as.xts(ret[, c("vintage","strategy_ret","actual_ret","ret")])
  stats <- round(rbind(table.AnnualizedReturns(ret2),
                       SharpeRatio(ret2),
                       maxDrawdown(ret2),
                       SemiDeviation(ret2)), 4)
  stats
  calstats <- table.CalendarReturns(ret2)
  return(list(error = s$error, best_cost = s$best_cost, mylag = mylag, stats = stats, calstats  = calstats, ans = s$ans))
  
}
