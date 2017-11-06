#' Retreives info for a stock
#'
#' Function family to retrieve specific info for a given ticker.
#'
#' Based off of the \code{TTR} function \code{stockSymbols} it compensates for different data types and difficulty
#' in accessing specific ticker data. Data retrieval depends on the \code{TTR} package, which retrieves its data
#' from \href{http://www.nasdaq.com/}{NASDAQ}
#' @note Values for \code{Sector} may not be identical to sectors listed in \code{\link{getSectorWeights}}.
#' Values for \code{Sector}, \code{IPO.Year}, and \code{Market.Cap} are occasionally missing and will default to
#' \emph{NA}. \code{getStockInfo} caches the data for \code{stockSymbols} in a hidden global variable (\code{.ss})
#' to massively increase speed on subsequent uses of the function.
#' @param ticker Character; the ticker for the stock you need info about
#' @param clean.mcap Logical; should the Market.Cap element be returned cleaned. If \code{TRUE}:
#' Market.Cap returns as a numeric. If \code{FALSE}: Market.Cap returns as a character string.
#' Defaults to \code{TRUE}
#' @param clean.sector Logical; should the Sector element be returned cleaned. Sector cleaning is done using
#' \code{\link{cleanSector}}. Defaults to \code{TRUE}
#' @param auto.assign Logical; should the results be assigned to \code{env}. If \code{FALSE} returns results.
#' Defaults to \code{FALSE}
#' @param env Environment; where to auto.assign objects. Setting \code{env=NULL} is equal to
#' \code{auto.assign=FALSE}. Defaults to \code{.GlobalEnv}.
#' @return Returns a list containing the elements:
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
#' getStockInfo.last.trade getStockInfo.price getStockInfo.last.price getStockInfo.mcap
#' getStockInfo.IPO
#'
#@author Alec Kulakowski, \email{alecthekulak@gmail.com}
#'
#' @family data retrieval functions
#' @keywords misc data
#' @examples
#' getStockInfo("NVDA")
#' @rdname getStockInfo
#' @export
"getStockInfo" <- function(ticker, clean.mcap = TRUE, clean.sector = TRUE,
                           auto.assign = FALSE, env = .GlobalEnv){
  # if(is.data.frame(get0(".ss", envir=.GlobalEnv))){
  #   .showUSER("stockSymbols already loaded. Retrieving...")
  #   stockList <- get0(".ss", envir=.GlobalEnv)
  if(exists("stockData", envir = get0(".ss"))){
    stockData <- get("stockData", envir = get0(".ss"))
  }else{
    .showUSER("Loading data from stockSymbols...")
    stockData <- TTR::stockSymbols(quiet = TRUE)
    # assign(".ss", stockList, envir=.GlobalEnv)
    assign("stockData", stockData, envir = get0(".ss"))
  }
  stockInfo <- stockData[which(stockData$Symbol==ticker),]
  if(nrow(stockInfo) == 0){ stop(paste("Could not find data for ticker:",as.character(ticker))) }
  if(clean.mcap){
    stockInfo$MarketCap <- cleanAccount(stockInfo$MarketCap)
  }
  if(clean.sector){
    stockInfo$Sector <- cleanSector(stockInfo$Sector)
  }
  RESULT <- list(Name = stockInfo$Name, Last.Trade = stockInfo$LastSale, Sector = stockInfo$Sector,
                 Industry = stockInfo$Industry, IPO.Year = stockInfo$IPOyear, Exchange = stockInfo$Exchange,
                 Market.Cap = stockInfo$MarketCap)
  if(!is.environment(env)){ auto.assign = FALSE }
  if(auto.assign){
    assign(x = paste0(ticker,".info"), value = RESULT, envir = env)
    return(paste0(ticker,".info"))
  }else{
    return(RESULT)
  }
}

#' @describeIn getStockInfo Loads the company name for the given ticker (if available)
#'
#' @examples getStockInfo.name("NVDA")
#' @keywords internal
#' @export
"getStockInfo.name" <- function(ticker){
  getStockInfo(ticker = ticker, clean.mcap = F, clean.sector = F)$Name
}
#' @describeIn getStockInfo Loads the sector for the given ticker, allows changing the \code{clean.sector} variable
#'
#' @examples \donttest{getStockInfo.sector("NVDA") }
#' @keywords internal
#' @export
"getStockInfo.sector" <- function(ticker, clean.sector = TRUE){
  getStockInfo(ticker, clean.mcap = F, clean.sector = clean.sector)$Sector
}
#' @describeIn getStockInfo Loads the exchange for the given ticker (if available)
#'
#' @examples \donttest{getStockInfo.exchange("NVDA") }
#' @keywords internal
#' @export
"getStockInfo.exchange" <-  function(ticker){
  getStockInfo(ticker = ticker, clean.mcap = F, clean.sector = F)$Exchange
}
#' @describeIn getStockInfo Loads the last trade price for the given ticker
#'
#' @examples \donttest{getStockInfo.last("NVDA") }
#' @concept getStockInfo.last.trade getStockInfo.price getStockInfo.last.price
#' @keywords internal
#' @export getStockInfo.last getStockInfo.last.trade getStockInfo.price getStockInfo.last.price
"getStockInfo.last" <- "getStockInfo.last.trade" <- "getStockInfo.price"  <- "getStockInfo.last.price"  <- function(ticker){
  getStockInfo(ticker = ticker, clean.mcap = F, clean.sector = F)$Last.Trade
}
#' @describeIn getStockInfo Loads the last descriptive industry name for the given ticker
#'
#' @examples \donttest{getStockInfo.industry("NVDA") }
#' @keywords internal
#' @export getStockInfo.industry
"getStockInfo.industry" <- function(ticker){
  getStockInfo(ticker = ticker, clean.mcap = F, clean.sector = F)$Industry
}
#' @describeIn getStockInfo Loads the IPO year for the given ticker (if available)
#'
#' @examples \donttest{getStockInfo.IPO.year("NVDA") }
#' @keywords internal
#' @export getStockInfo.IPO.year getStockInfo.IPO
"getStockInfo.IPO.year" <- "getStockInfo.IPO" <- function(ticker){
  getStockInfo(ticker = ticker, clean.mcap = F, clean.sector = F)$IPO.Year
}
#' @describeIn getStockInfo Loads the current market capitalization for the given ticker (if available), allows changing the \code{clean.mcap} variable
#'
#' @examples \donttest{getStockInfo.market.cap("NVDA", clean.mcap=FALSE) }
#' @keywords internal
#' @export getStockInfo.market.cap getStockInfo.mcap
"getStockInfo.market.cap" <- "getStockInfo.mcap" <- function(ticker, clean.mcap = TRUE){
  getStockInfo(ticker = ticker, clean.mcap = clean.mcap, clean.sector = F)$Market.Cap
}

