#' Load Constituent Data for Indexes 
#' 
#' Functions to scrape and clean index constituent data from multiple sources for a given index. 
#' Current \code{src} methods available are: default.
#' Data is loaded silently without user assignment by default. 
#' 
#' @param index character specifying the name of the index
#' @param env where to create objects. Setting env=NULL is equal to auto.assign=FALSE. Defaults to .GlobalEnv
#' @param simple should results include non-ticker data. If \code{TRUE}, returns character vector of \code{index}'s 
#'   constituents. If \code{FALSE} returns an. Defaults to \code{TRUE}
#' @param src character string specifying the source to retrieve the data from. As of 0.1-1, 
#' the only source available is "default" which changes depending on the selected \code{index}. Defaults to "default"
#' @param auto.assign should results be loaded to \code{env}. If \code{FALSE} return results. Defaults to \code{TRUE}
#' 
#' @return if \code{auto.assign==FALSE}:  a character vector of \code{index}'s constituents
#' @return if \code{auto.assign==TRUE}:  the name of the variable holding the data within the specified environment 
#'  
#' @export
#' @examples 
#' ## Not run: 
#' getConstituents(index='S&P500')
#' ## End(Not run)
"getConstituents" <- function(index, env=.GlobalEnv, simple = TRUE, src = "default", auto.assign=TRUE){
  # suppressMessages(library(rvest))
  # suppressMessages(library(RCurl))
  # suppressMessages(library(gdata))
  # suppressMessages(library(quantmod))
  # suppressMessages(library(magrittr))
  index <- tolower(index) %>% gsub(pattern='[ &^]+|index', replacement='')
  if(index %in% c('nasdaq100', 'ndx', 'nasdaq')){
    indexData <- read.csv( url("http://www.nasdaq.com/quotes/nasdaq-100-stocks.aspx?render=download"),
                           header=TRUE, strip.white=TRUE, stringsAsFactors = FALSE)
    if(simple){
      return(indexData$Symbol)
    }else{
      indexData$dollarVal <- indexData$share_volume * indexData $lastsale
      totalVal <- sum(indexData$dollarVal)
      indexData$Weight <- indexData$dollarVal / totalVal
      indexData <- indexData[,c('Symbol','lastsale','Weight')]
      colnames(indexData) <- c('Ticker','Price','Weight')
      return(indexData)
    }
  }else if(index %in% c('sp500','gspc', 'spy', 'sp')){
    url <- "http://us.spdrs.com/site-content/xls/SPY_All_Holdings.xls?fund=SPY&docname=All+Holdings&onyx_code1=&onyx_code2="
    indexData <- gdata::read.xls("http://us.spdrs.com/site-content/xls/SPY_All_Holdings.xls?fund=SPY&docname=All+Holdings&onyx_code1=&onyx_code2=", 
                          header=FALSE,strip.white=TRUE, stringsAsFactors = FALSE)[-1:-4,]
    colnames(indexData) <- c('Company', 'Ticker', 'Weight', 'Sector', 'Shares')
    indexData <- head(indexData, -10)
    # url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
    # web_page <- read_html(url) %>% html_node("table.wikitable") %>% html_table() 
    # sp_tickers <- unique(web_page[,'Ticker symbol'])
    if(simple){
      
      return(indexData$Ticker[indexData$Ticker != ""])
    }else{
      indexData$Price <- quantmod::getQuote(indexData$Ticker, what = yahooQF('Last Trade (Price Only)'))$Last
      return(indexData[c('Ticker','Price', 'Weight')])
    }
  }else if(index %in% c('djia', 'dia', 'dji','dow','dowjones')){
    url <- "http://money.cnn.com/data/dow30/"
    dow_tickers <- read_html(url) %>% html_node('table.wsod_dataTable') %>% html_table()
    dow_tickers$Ticker <- dow_tickers$Company %>% gsub(pattern='[[:blank:]]+[[:print:]]+', replacement='')
    if(simple){
      return(dow_tickers$Ticker)
    }else{
      denom <- sum(dow_tickers$Price)
      dow_tickers$Weight <- dow_tickers$Price/denom
      return(dow_tickers[,c('Ticker','Price','Weight')])
    }
  }
  #} else if(index %in% c('russel3000', 'rusell3000', 'rusel3000', 'russell3000')){
  #   https://institutional.vanguard.com/#
  # }
  
}
