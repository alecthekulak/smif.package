#' Load constituent data for indexes 
#' 
#' Functions to scrape and clean index constituent data from multiple sources for a given index. 
#' Current \code{src} methods available are: default.
#' Data is loaded silently without user assignment by default. 
#' 
#' @param index a character vector specifying the name of the index. Acceptable values are 
#'   \code{c("S&P 500", "NASDAQ 100", "DJIA")}
#' @param env where to create objects. Setting \code{env=NULL} is equal to \code{auto.assign=FALSE}. 
#'   Defaults to \code{.GlobalEnv}
#' @param simple should results include non-ticker data. If \code{TRUE}, returns character vector of \code{index}'s 
#'   constituents. If \code{FALSE} returns a data.frame containing the following columns: stock ticker, last price, 
#'   index weight. Defaults to \code{TRUE}
#' @param src character string specifying the source to retrieve the data from. As of 0.1-1, 
#' the only source available is "default" which changes depending on the selected \code{index}. Defaults to "default"
#' @param auto.assign should results be loaded to \code{env}. If \code{FALSE} return results. Defaults to \code{TRUE}
#' 
#' @return a character vector or data.frame of \code{index}'s constituent tickers (, price, weight)
#'  
#' @author Alec Kulakowski, \email{alecthekulak@gmail.com}
#' @seealso \code{\link{smif.package}} \code{\link{cleanIndex}}
#' @aliases getConstituents
#' 
#' @details 
#' \code{getConstituents} will try to format the \code{index} parameter using \code{\link{cleanIndex}}. 
#' Data is loaded from various sources depending on input arguments: 
#' 
#' For S&P 500 data, the source is the \href{http://us.spdrs.com/}{SPDR website}, price data. For 
#'   \code{SIMPLE=FALSE}, data is also drawn from Yahoo Finance.
#' 
#' For NASDAQ 100 data, the source is the \href{http://www.nasdaq.com/quotes/nasdaq-100-stocks.aspx}{NASDAQ website}. 
#'   For \code{SIMPLE=FALSE}, price data is also taken from NASDAQ. Constituent weightings are not available in a
#'   form scrapable by R. 
#'   
#' For DJIA 30 data, the souce is \href{http://money.cnn.com/data/dow30/}{CNN Money}, which updates its data every few
#'   months. For \code{SIMPLE=FALSE}, up-to-date price data is sourced from Yahoo Finance, and constituent weightings 
#'   are calculated manually. 
#'   
#' Future versions will include \href{http://www.ftse.com/products/indices/russell-us}{Russel indexes} with data sourced 
#'   from \href{https://institutional.vanguard.com/}{Vanguard}
#'
#' @keywords misc
#'  
#' @export
#' @examples 
#' \dontrun{
#' 
#' getConstituents(index='S&P 500')
#' getConstituents(index='NASDAQ 100')
#' getConstituents(index='DJIA', auto.assign=FALSE)
#' 
#' getConstituents.simple(index='S&P 500')
#' }
"getConstituents" <- function(index, env=.GlobalEnv, simple = TRUE, src = "default", auto.assign=TRUE){
  # @references \url{http://us.spdrs.com/}, \url{http://www.nasdaq.com/quotes/nasdaq-100-stocks.aspx}
  # suppressMessages(library(rvest))
  # suppressMessages(library(RCurl))
  # suppressMessages(library(gdata))
  # suppressMessages(library(quantmod))
  # suppressMessages(library(magrittr))
  if(!is.environment(env)){ auto.assign = FALSE }
  # Index text correction
  index = cleanIndex(index)
  if(index == "NDX"){
    # Retrieves NASDAQ-100 tickers from NASDAQ itself 
    url <- "http://www.nasdaq.com/quotes/nasdaq-100-stocks.aspx?render=download"
    indexData <- read.csv( url(url),
                           header=TRUE, strip.white=TRUE, stringsAsFactors = FALSE)
    # Data cleaning 
    indexData <- indexData[, !(names(indexData) %in% c("X"))]
    if(simple){
      RESULT <- indexData$Symbol
    }else{
      # There is no current way to get weights for NASDAQ constituents (w/o advanced formulas)
      # url2 <- "http://business.nasdaq.com/Docs/NDX.pdf"
      indexData$Weight <- 0
      indexData <- indexData[,c('Symbol','lastsale','Weight')]
      colnames(indexData) <- c('Ticker','Price','Weight')
      warning("Weight data is not yet available for the NASDAQ 100")
      RESULT <- indexData
    }
  }else if(index == "SPY"){
    # Retrieves S&P 500 tickers from SPDR site itself 
    url <- "http://us.spdrs.com/site-content/xls/SPY_All_Holdings.xls?fund=SPY&docname=All+Holdings&onyx_code1=&onyx_code2="
    indexData <- gdata::read.xls(url, 
                          header=FALSE,strip.white=TRUE, stringsAsFactors = FALSE)[-1:-4,]
    colnames(indexData) <- c('Company', 'Ticker', 'Weight', 'Sector', 'Shares')
    # indexData <- head(indexData, -10) #should be -11
    # url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
    # web_page <- read_html(url) %>% html_node("table.wikitable") %>% html_table() 
    # sp_tickers <- unique(web_page[,'Ticker symbol'])
    # Data cleaning (removing description lines in the XLS)
    indexData <- indexData[indexData$Ticker != "",]
    if(simple){
      RESULT <- indexData$Ticker
    }else{
      # Retrieve price data for each ticker 
      indexData$Price <- quantmod::getQuote(indexData$Ticker, what = quantmod::yahooQF('Last Trade (Price Only)'))$Last
      RESULT <- indexData[c('Ticker','Price', 'Weight')]
    }
  }else if(index == "DJIA"){
    # Retrieves Dow Jones Industrial Average tickers from CNN 
    url <- "http://money.cnn.com/data/dow30/"
    indexData <- xml2::read_html(url) %>% rvest::html_node('table.wsod_dataTable') %>% rvest::html_table()
    indexData$Ticker <- indexData$Company %>% gsub(pattern='[[:blank:]]+[[:print:]]+', replacement='')
    if(simple){
      RESULT <- indexData$Ticker
    }else{
      # Tries to generate weights for each ticker
      warning("Weights for DJIA constituents may not be accurate. Expect deviation <= 5%")
      indexData$Price <- quantmod::getQuote(indexData$Ticker, what = quantmod::yahooQF('Last Trade (Price Only)'))$Last
      denom <- sum(indexData$Price)
      indexData$Weight <- indexData$Price/denom
      RESULT <- indexData[,c('Ticker','Price','Weight')]
    }
  }else{
    stop("Invalid input")
  }
  if(auto.assign){
    assign(x = index, value = RESULT, envir = env)
    return(index)
  }else{
    return(RESULT)
  }
}

#' @describeIn getConstituents A one-input streamlined version of \code{getConstituents} 
"getConstituents.simple" <- function(index) getConstituents(index, simple=TRUE, src="default", auto.assign=FALSE)
