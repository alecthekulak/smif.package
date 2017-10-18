#' Identify ETFs that correspond to a given sector
#'
#' Uses \code{getStockInfo.sector} and/or \code{cleanSector} to
#' retrieve uniform sector name for input. One of either
#' \code{ticker} or \code{sector} must be supplied.
#'
#' @note
#' \code{getSectorETF} is a wrapper of various sub-functions.
#' If both \code{ticker} and \code{sector} are supplied,
#' \code{sector} will take priority.
#'
#' @param ticker Character; string of a stock's ticker
#' @param sector Character; string of a sector
#' @param ... additional parameters
#'
#' @family data retrieval functions
#' @return Character; string of sector ETF's ticker
#' @export getSectorETF
#' @seealso \code{\link{getStockInfo.sector}}: to retrieve sector
#' that corresponds to input ticker. \code{\link{cleanSector}}: to
#' coerce sector into uniform name.
#'
#' @examples
#' getSectorETF(ticker = "NVDA", verbose=FALSE)
#' getSectorETF(sector = "Consumer Non-Durables")
getSectorETF <- function(ticker, sector, ...){
  if(missing(ticker) & missing(sector)){
    stop("One of arguments 'ticker' or 'sector' must be supplied.")
  }else if(missing(ticker)){
    return( getSectorETF.sector(sector = sector, ...) )
  }else if(missing(sector)){
    newSector <- getStockInfo.sector(ticker)
    return( getSectorETF.sector(sector = newSector, ...) )
  }
}
#' @rdname getSectorETF
#' @title Retrieve ETF for specified sector
#'
#' @param verbose Logical; should messages be shown. Defaults to getOption("verbose")
#'
#' @export getSectorETF.sector
#'
#' @examples
#' getSectorETF.sector("Telecom")
getSectorETF.sector <- function(sector, verbose = getOption("verbose", FALSE)){
  # Clean input string
  if(verbose){
    sector <- cleanSector(sector)
  }else{
    sector <- suppressMessages( cleanSector(sector) )
  }
  if(sector %in% smif_aa$sectors$sectorName){
    return( smif_aa$sectors[smif_aa$sectors$sectorName == sector,]$sectorETF )
  }else if(grepl("Misc|Other", sector)){
    if(verbose) message("Sector is Miscellaneous/Other. Returning market ETF.")
    return("SPY")
  }
  stop(paste0("Sector ETF could not be found for ",sector))
}
