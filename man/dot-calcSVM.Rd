% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/backtest_svm.R
\encoding{UTF-8}
\name{.calcSVM}
\alias{.calcSVM}
\title{.calcSVM}
\usage{
.calcSVM(mydat, fmla, mylag, best_cost)
}
\arguments{
\item{mydat}{data.table of rawdata}

\item{fmla}{model formula used to forecast recessions.}

\item{mylag}{integer number of months to lag the data.}

\item{best_cost}{integer best_cost or NULL, in which case the best_cost will be determined using k-fold cross-validation.}
}
\value{
list containing the following outputs:
*error* the percentage of months incorrectly classified, 
*best_cost* integer value of th best_cost as determined by the k-fold cross validation on the first 14 years of data, and
*ans* is the predictions, 1 for recession and -1 for no recession.
}
\description{
Helper function to calculate SVM.
}
\author{
Roger J. Bos, \email{roger.bos@gmail.com}
}
\concept{recession forecasting using SVM and backtesting results}
