#' Data for 742 months capturing 6 periods of recession.
#'
#' A dataset containing the recession indicator and possible predictor variables for 742 months from 1958-01-31 to 2019-10-31.
#' 
#' @format A data frame with 742 rows and 501 variables:
#' \describe{
#'   \item{vintage}{date of the data}
#'   \item{recession}{recession}
#'   \item{ret}{return of the S&P 500 index}
#'   \item{payems}{total nonfarm employees, .1,..,.12 indicates which of the last 12 months}
#'   \item{sp500}{S&P 500 monthly closing price, .1,..,.12 indicates which of the last 12 months}
#'   \item{yld_curve}{(GS10 - FEDFUNDS), .1,..,.12 indicates which of the last 12 months}
#'   \item{man_prod}{Manufacturing Production Index as published by the Institute for Supply Management, .1,..,.12 indicates which of the last 12 months}
#'   
#' }
#' @source \url{https://www.nber.org/cycles.html}
#' @source \url{https://alfred.stlouisfed.org/series?seid=PAYEMS}
#' @source \url{https://www.multpl.com/s-p-500-historical-prices/table/by-month}
#' @source \url{https://fred.stlouisfed.org/series/GS10}
#' @source \url{https://fred.stlouisfed.org/series/FEDFUNDS}
#' @source \url{https://www.quandl.com/data/ISM/MAN_PROD-Manufacturing-Production-Index}
#' 
#' 
