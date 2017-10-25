#' Get stock data (optimized)
#'
#' A custom wrapper for getSymbol calls. Switches between different sources depending on server
#' status, and ticker classification. Negates need for local storage of price data between calls.
#' Optimized to increase speed of subsequent calls dramatically.
#'
#' Uses \code{tryCatch} and global options to determine and store current server status for premium
#' data provider Alpha Vantage. Retrieves data based on status of AV's server (if down, data is
#' retrieved from Google). Retrieved data is stored in a private environment \emph{and} returned.
#' This makes subsequent calls to the function with the same or similar arguments practically
#' instantaneous. \code{full} data currently unavailable for \code{tickers} which contain "RFR".
#'
#' @param tickers Character; a string or a vector of strings specifying the data which is required
#' @param full Logical; should function return "full" data? If \code{TRUE}: return data will be a
#' \code{list} of \code{OHLC} objects. If \code{FALSE}: return data will be an \code{xts} of
#' Adjusted/Close price data for each ticker. Defaults to \code{FALSE}
#' @param try.av Logical; should program attempt to get data from Alpha Vantage? Defaults to \code{FALSE}
#' (AV servers are currently slow compared to Google's)
#' @param ret Logical; should results be returned to user. If \code{FALSE} returns \code{tickers} to
#' user. Defaults to \code{TRUE}
#' @return Either a \code{list} of \code{OHLC} objects (if \code{isTRUE(full)}) or a \code{xts} of
#' Adjusted price data for each ticker (if \code{!isTRUE(full)})
#'
#' @examples \dontrun{
#' getSymbols.SMIF(c("NVDA", "SPY", "RFR"), full = FALSE) }
#' getSymbols.SMIF("SPY", ret = FALSE)
#' @family data retrieval functions
#' @seealso \code{\link{.getTo}}, \code{\link{.getFrom}}: used to determine date boundries for data
#' retrieval
#' @importFrom quantmod getSymbols has.Ad has.Cl Ad Cl
#' @import xts
#' @export getSymbols.SMIF
"getSymbols.SMIF" <- function(tickers, full = F, try.av = F, ret = T){
  options("use.av" = try.av)# For now, until it becomes faster
  options("getSymbols.warning4.0" = FALSE)
  # Ensures environment exists
  if(!exists(".stock.data", mode="environment")){
    .showUSER(".stock.data environment does not exist. Creating it.")
    .stock.data <- new.env(parent = globalenv())
  }
  # Ensure all tickers are defined in the environment
  # tickers[which(grepl("rfr", tickers, ignore.case = T))] <- "DGS3MO"
  for(ticker in tickers){
    if(grepl("rfr", ticker, ignore.case = T)){
      if(full){ stop("Cannot return full data for series including: ", ticker) }
      ticker <- "DGS3MO"
    }
    if(!(ticker %in% names(.stock.data$.getSymbols))){
      if(ticker == "DGS3MO"){
        name <- getSymbols('DGS3MO',src = 'FRED', auto.assign = TRUE, env = .stock.data)
        temp <- .stock.data[[name]]
        temp <- na.approx(temp) #zoo na.approx
        temp <- temp[ index(temp) >= .getFrom() & index(temp) <= .getTo() ]
        temp <- temp / 252 #turn yearly rate into daily rate/return
        names(temp) <- c("RFR")
        assign("RFR", temp, envir = .stock.data) #zoo index
        next
      }
      if( getOption("use.av", TRUE) ){
        tryCatch({suppressWarnings({
          name <- getSymbols(Symbols = ticker, src="av",
                             periodicity = "daily", data.type="csv", output.size = "full",
                             adjusted = TRUE, api.key = "NDU8JOJCG3IDQEW7",
                             auto.assign=T, env = .stock.data)
        })}, error = function(e){
          cat("Alpha Vantage server is unaccessible. Switching to Google.")
          options("use.av" = FALSE)
        })
      }
      if( !getOption("use.av", TRUE) ){
        name <- getSymbols(Symbols = ticker, src="google",
                           from = .getFrom(), to = .getTo(),
                           auto.assign=T, env = .stock.data)
      }
      temp <- na.approx(.stock.data[[name]])
      assign(name, (temp[ index(temp) >= .getFrom() & index(temp) <= .getTo() ]), envir = .stock.data)
    }
  }
  # "full" returns list of OHLC data
  if( !ret ){
    .showUSER("Symbols assigned. ret == FALSE, returning tickers")
    return( tickers )
  }
  if( full ){
    # Single argument
    if(length(tickers) == 1){
      return( .stock.data[[tickers]] )
    }
    # Multiple arguments
    results <- vector(mode = "list", length = length(tickers))
    for(num in 1:length(tickers)){
      results[[num]] <- .stock.data[[tickers[num]]]
    }
    return( results )
  }else{
    # Single argument
    if(length(tickers) == 1){
      #For RFR argument
      if(grepl("rfr", tickers, ignore.case = T)){
        return( .stock.data[["RFR"]] )
      }
      if(has.Ad(.stock.data[[tickers]])){
        return( Ad(.stock.data[[tickers]]) )
      }else{
        return( Cl(.stock.data[[tickers]]) )
      }
    }
    # Multiple arguments
    results <- xts::xts()
    for(ticker in tickers){
      #For RFR argument
      if(grepl("rfr", ticker, ignore.case = T)){
        rfr <- .stock.data[["RFR"]]
        results <- merge( rfr[index(getSymbols.SMIF("SPY",full=F))], results)
      }else if( has.Ad(.stock.data[[ticker]]) ){
        results <- merge( Ad(.stock.data[[ticker]]), results )
      }else{
        results <- merge( Cl(.stock.data[[ticker]]), results )
      }
    }
    colnames(results) <- rev(tickers)
    return(results[, tickers])
  }
}



