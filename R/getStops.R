#' Generates VALOS stops
#'
#' Generate stops for a given stock using the SMIF's Volatility Adjusted
#' Leg Out Strategy. Stops are generated as of a user given date which, if
#' ommitted, defaults to the current date.
#'
#' Stops can be returned in percentage terms (i.e. \code{c(-0.1, -0.2, -0.3)})
#' or in dollar terms based on the most recent price available at the date the
#' stops were generated (i.e. \code{c(54, 48, 42)}), in US dollars. Stops are
#' calculated using the current official version of VALOS. This allows for the
#' possibility of there being only one stop returned for the stock (as is
#' allowed by our current version of VALOS).
#'
#' @note The SMIF VALOS formula is currently undergoing a review and a finalized
#' change is likely before November 15th. After that, past VALOS stops calculated
#' by this function may or may not return the old version of VALOS or the newer
#' version of VALOS. Post-revision runnings for date >= date(revision) will use
#' the newer VALOS formula.
#'
#' @param ticker Character; ticker of the stock to generate stops for
#' @param date Character (or Date); date of the day to generate the VALOS stops.
#' Defaults to \code{Sys.Date()} (today's date)
#' @param per Logical; should the stops be returned in percentage terms. Defaults
#' to \code{TRUE}
#'
#' @aliases getVALOS
#' @concept VALOS stops stop loss
#' @importFrom quantmod getSymbols dailyReturn Cl
#' @importFrom lubridate days
#' @importFrom stats na.omit sd
#' @examples \dontrun{
#' valos("CELG", per = TRUE)}
#' @export getStops getVALOS
"getStops" <- "getVALOS" <- function(ticker, date = Sys.Date(), per = FALSE){
  start_date <- as.Date(date) - days(155)
  # Data Gathering
  benchmark <- smif.package::smif_aa$benchmark
  benchmark_data <- na.omit(getSymbols(benchmark, auto.assign=FALSE, from=start_date, to=date, src="yahoo"))
  stock_data <- na.omit(getSymbols(ticker, auto.assign=FALSE, from=start_date, to=date, src="yahoo"))
  # Data Cleaning
  benchmark_returns <- dailyReturn(last(benchmark_data, n=100)) #@importFrom xts last
  stock_returns <- dailyReturn(last(stock_data, n=100))
  # Calculations   # @importFrom PerformanceAnalytics StdDev
  stdev_benchmark <- sd(benchmark_returns)
  stdev_stock <- sd(stock_returns)
  # Calculate Average Stop
  AVG_STOP <- 0.1 * (stdev_stock / stdev_benchmark)
  # Calculate Stops
  if (AVG_STOP <= 0.1){
    STOPS <- c(0.1, 0.1, 0.1)
  }else if( AVG_STOP <= 0.2){
    STOPS <- c(0.1, AVG_STOP, (2 * AVG_STOP - 0.1))
  }else{
    STOPS <- c(0.1, 0.2, 0.3)
  }
  # Output Stop Prices
  if (per) {
    return(-STOPS)
  } else{
    LAST <- last(Cl(stock_data), n = 1)[[1]] #last(stock_data, n=1)[[4]]
    STOP_PRICES <- LAST * (1 - STOPS)
    return(round(STOP_PRICES, 2))
  }
}
# STOPS = .1
# #calculate the stop losses and the exact price of the stop loss
# valos <- function(TICKER, CURRDATE = Sys.Date())
# {
#   #internal Variables
#   DATERANGE = 90
#   STARTDATE <- date(CURRDATE) - days(DATERANGE)
#   #want to make sure that there are 100 observations
#   benchmark <- na.omit(getSymbols("SPY", auto.assign = FALSE, from = STARTDATE, to = CURRDATE, src = "google"))
#   tickerData <- na.omit(getSymbols(TICKER, auto.assign = FALSE, from = STARTDATE, to = CURRDATE, src = "google"))
#   lastClose = as.numeric(tickerData[nrow(tickerData),4])
#   #calculate the daily returns
#   benchReturn = dailyReturn(last(benchmark, n = 100))
#   tickReturn = dailyReturn(last(tickerData, n = 100))
#   #calculate the stdev of the returns
#   benchSD = sd(benchReturn)
#   tickSD = sd(tickReturn)
#   #the rule is the ratio times 10%
#   avg_stop = STOPS*(tickSD/benchSD)
#   #avg_stop
#   if (avg_stop <= .1){
#     stops = c(.1, .1, .1)
#   }
#   if (avg_stop > .1 && avg_stop <= .2){
#     stops = c(.1, avg_stop, 2*avg_stop-.10)
#   }
#   if (avg_stop > .2){
#     stops = c(.1, .2, .3)
#   }
#   stopPrices = lastClose*(1-stops)
#   stopsFrame = rbind.data.frame(stops, stopPrices)
#   colnames(stopsFrame) <- c("Lowest", "Middle", "Highest")
#   rownames(stopsFrame) <- c("Stops", "Stop Prices")
#   return(stopsFrame)
# }
