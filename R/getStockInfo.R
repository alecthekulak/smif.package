#' Retreives info for a stock
#'
#' Function family to retrieve specific info for a given ticker.
#'
#' Based off of the \code{TTR} function \code{stockSymbols} it compensates for different data types and difficulty
#' in accessing specific ticker data. Data retrieval depends on the \code{TTR} package, which retrieves its data
#' from \href{http://www.nasdaq.com/}{NASDAQ}
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
#'   \item{Name}{Character; name of the company}
#'   \item{Last.Trade}{Numeric; price of the last trade for the company}
#'   \item{Sector}{Character; sector of the company}
#'   \item{Industry}{Character; descriptive industry name}
#'   \item{IPO.Year}{Integer; year of the company's IPO}
#'   \item{Exchange}{Character; exchange that the company currently trades on. One of: \code{"AMEX"},
#'        \code{"NASDAQ"}, \code{NYSE}.}
#'   \item{Market.Cap}{Numeric (or Character); current market capitalization for the company}
#'
#' @aliases getStockInfo getStockInfo.sector getStockInfo.name getStockInfo.last
#' getStockInfo.industry getStockInfo.exchange getStockInfo.IPO.year getStockInfo.market.cap
#'
#@author Alec Kulakowski, \email{alecthekulak@gmail.com}
#'
#' @family data retrieval functions
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
#' @title getStockInfo.name
#' Loads the company name for the given ticker (if available)
#' @examples getStockInfo.name("NVDA")
#' @export
"getStockInfo.name" <- function(ticker){ getStockInfo(ticker = ticker, numMarketCap = F)$Name }
#' @rdname getStockInfo
#' @title getStockInfo.sector
#' Loads the sector for the given ticker
#' @examples \donttest{getStockInfo.sector("NVDA") }
#' @export
"getStockInfo.sector" <- function(ticker){ getStockInfo(ticker, numMarketCap = F)$Sector }
#' @rdname getStockInfo
#' @title getStockInfo.exchange
#' Loads the exchange for the given ticker (if available)
#' @examples \donttest{getStockInfo.exchange("NVDA") }
#' @export
"getStockInfo.exchange" <-  function(ticker){ getStockInfo(ticker = ticker, numMarketCap = F)$Exchange }
#' @rdname getStockInfo
#' @title getStockInfo.last
#' Loads the last trade price for the given ticker
#' @examples \donttest{getStockInfo.last("NVDA") }
#' @concept getStockInfo.last.trade getStockInfo.price getStockInfo.last.price
#' @export getStockInfo.last
"getStockInfo.last" <- function(ticker){ getStockInfo(ticker = ticker, numMarketCap = F)$Last.Trade }
#' @export getStockInfo.last.trade getStockInfo.price getStockInfo.last.price
"getStockInfo.last.trade" <- "getStockInfo.price"  <- "getStockInfo.last.price" <- getStockInfo.last
#' @rdname getStockInfo
#' @title getStockInfo.industry
#' Loads the last descriptive industry name for the given ticker
#' @examples \donttest{getStockInfo.industry("NVDA") }
#' @export
"getStockInfo.industry" <- function(ticker){ getStockInfo(ticker = ticker, numMarketCap = F)$Industry }
#' @rdname getStockInfo
#' @title getStockInfo.IPO.year
#' Loads the IPO year for the given ticker (if available)
#' @examples \donttest{getStockInfo.IPO.year("NVDA") }
#' @concept getStockInfo.IPO
#' @export getStockInfo.IPO.year
"getStockInfo.IPO.year" <- function(ticker){ getStockInfo(ticker = ticker, numMarketCap = F)$IPO.Year }
#' @export getStockInfo.IPO
"getStockInfo.IPO" <- getStockInfo.IPO.year
#' @rdname getStockInfo
#' @title getStockInfo.market.cap
#' Loads the current market capitalization for the given ticker (if available), allows changing the \code{numMarketCap} variable
#' @examples \donttest{getStockInfo.market.cap("NVDA", numMarketCap=FALSE) }
#' @concept getStockInfo.mcap
#' @export getStockInfo.market.cap
"getStockInfo.market.cap" <- function(ticker, numMarketCap=TRUE){
  getStockInfo(ticker = ticker, numMarketCap = numMarketCap)$Market.Cap
}
#' @export getStockInfo.mcap
"getStockInfo.mcap" <- getStockInfo.market.cap

