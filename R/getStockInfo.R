#' Retreives info for a stock
#'
#' Functions to retrieve specific info for a given ticker. Based off of the \code{TTR} function \code{stockSymbols}
#' it compensates for different data types and difficulty in accessing specific ticker data.
#'
#' Data retrieval depends on the \code{TTR} package, which retrieves its data from \href{http://www.nasdaq.com/}{NASDAQ}
#' @note Values for \code{Sector} may not be identical to sectors listed in \code{\link{getSectorWeights}}.
#' Values for \code{Sector}, \code{IPO.Year}, and \code{Market.Cap} are occasionally missing and will default to \emph{NA}.
#' @param ticker Character; the ticker for the stock you need info about
#' @param numMarketCap Logical; should the Market.Cap element be returned cleaned. If \code{TRUE}:
#' Market.Cap returns as a numeric. If \code{FALSE}: Market.Cap returns as a character string.
#' Defaults to \code{TRUE}
#' @param auto.assign Logical; should the results be assigned to \code{env}. If \code{FALSE} returns results.
#' Defaults to \code{FALSE}
#' @param env Environment; where to auto.assign objects. Setting \code{env=NULL} is equal to
#' \code{auto.assign=FALSE}. Defaults to \code{.GlobalEnv}.
#' @return \code{getStockInfo} returns a list containing the elements:
#'
#'   \item{Name}{(character) name of the company}
#'   \item{Last.Trade}{(numeric) price of the last trade for the company}
#'   \item{Sector}{(character) sector of the company}
#'   \item{Industry}{(character) descriptive industry name}
#'   \item{IPO.Year}{(integer) year of the company's IPO}
#'   \item{Exchange}{(character) exchange that the company currently trades on. One of: \code{"AMEX"},
#'        \code{"NASDAQ"}, \code{NYSE}.}
#'   \item{Market.Cap}{(numeric or character) current market capitalization for the company}
#'
#' @aliases getStockInfo getStockInfo.Sector getStockInfo.Name getStockInfo.Last.Trade getStockInfo.Industry
#' getStockInfo.Exchange getStockInfo.IPO.Year getStockInfo.Market.Cap
#' @author Alec Kulakowski, \email{alecthekulak@gmail.com}
#' @keywords misc data
#' @examples
#' getStockInfo("NVDA")
#' @rdname getStockInfo
#' @export
"getStockInfo" <- function(ticker, numMarketCap = TRUE, auto.assign=FALSE, env=.GlobalEnv){
  stockList <- TTR::stockSymbols(quiet = TRUE)
  stockInfo <- stockList[which(stockList$Symbol==ticker),]
  if(nrow(stockInfo) == 0){ stop(paste("Could not find data for ticker:",as.character(ticker))) }
  if(numMarketCap){
    mcap <- cleanAccount(stockInfo$MarketCap)
  }else{ mcap <- stockInfo$MarketCap }
  RESULT <- list(Name = stockInfo$Name, Last.Trade = stockInfo$LastSale, Sector = stockInfo$Sector,
                 Industry = stockInfo$Industry, IPO.Year = stockInfo$IPOyear, Exchange = stockInfo$Exchange,
                 Market.Cap = mcap)
  if(!is.environment(env)){ auto.assign = FALSE }
  if(auto.assign){
    assign(x = paste0(ticker,".info"), value = RESULT, envir = env)
    return(paste0(ticker,".info"))
  }else{
    return(RESULT)
  }
}

#' @rdname getStockInfo
#' @title getStockInfo.Name
#' Loads the company name for the given ticker (if available)
#' @examples getStockInfo.Name("NVDA")
#' @export
"getStockInfo.Name" <- "getStockInfo.name" <- function(ticker){ getStockInfo(ticker = ticker, numMarketCap = F)$Name }
#' @rdname getStockInfo
#' @title getStockInfo.Sector
#' Loads the sector for the given ticker
#' @examples \donttest{getStockInfo.Sector("NVDA") }
#' @export
"getStockInfo.Sector" <- "getStockInfo.sector" <- function(ticker){ getStockInfo(ticker, numMarketCap = F)$Sector }
#' @rdname getStockInfo
#' @title getStockInfo.Exchange
#' Loads the exchange for the given ticker (if available)
#' @examples \donttest{getStockInfo.Exchange("NVDA") }
#' @export
"getStockInfo.Exchange" <-"getStockInfo.exchange" <-  function(ticker){ getStockInfo(ticker = ticker, numMarketCap = F)$Exchange }
#' @rdname getStockInfo
#' @title getStockInfo.Last.Trade
#' @description Loads the last trade price for the given ticker
#' @examples \donttest{getStockInfo.Last.Trade("NVDA") }
#' @export
"getStockInfo.Last.Trade" <- "getStockInfo.last" <- function(ticker){ getStockInfo(ticker = ticker, numMarketCap = F)$Last.Trade }
#' @rdname getStockInfo
#' @title getStockInfo.Industry
#' Loads the last descriptive industry name for the given ticker
#' @examples \donttest{getStockInfo.Industry("NVDA") }
#' @export
"getStockInfo.Industry" <- "getStockInfo.industry" <- function(ticker){ getStockInfo(ticker = ticker, numMarketCap = F)$Industry }
#' @rdname getStockInfo
#' @title getStockInfo.IPO.Year
#' Loads the IPO year for the given ticker (if available)
#' @examples \donttest{getStockInfo.IPO.Year("NVDA") }
#' @export
"getStockInfo.IPO.Year" <-"getStockInfo.ipoyear" <- function(ticker){ getStockInfo(ticker = ticker, numMarketCap = F)$IPO.Year }
#' @rdname getStockInfo
#' @title getStockInfo.Market.Cap
#' Loads the current market capitalization for the given ticker (if available), allows changing the \code{numMarketCap} variable
#' @examples \donttest{getStockInfo.Market.Cap("NVDA", numMarketCap=FALSE) }
#' @export
"getStockInfo.Market.Cap" <-"getStockInfo.mcap" <- function(ticker, numMarketCap=TRUE){
  getStockInfo(ticker = ticker, numMarketCap = numMarketCap)$Market.Cap
}

