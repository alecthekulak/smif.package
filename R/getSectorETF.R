

# sectors = c("Consumer Discretionary", "Consumer Staples", "Energy", "Financials", "Health Care",
#             "Industrials", "Technology", "Utilities", "Telecomunications", "Materials")
#
# sectorETFs = c("XLY", "XLP", "XLE", "XLF", "XLV",
#                "XLI", "XLK", "XLU", "XTL", "XLB")
#
# aas <- as.data.frame(sectors, sectorETFs)
# df <- data.frame(sectors, sectorETFs)

# library(smif.package)
#'getSectorETF
getSectorETF <- function(ticker, ...){
  sector <- getStockInfo.sector()
}
#'GetSectorETF.sector
getSectorETF.sector <- function(sector, verbose = getOption("verbose", FALSE)){
  # Clean input string
  sector <- suppressMessages( cleanSector(sector) )
  if(sector %in% smif_aa$sectors$sectorName){
    return( smif_aa$sectors[smif_aa$sectors$sectorName == sector,]$sectorETF )
  }else if(grepl("Misc|Other", sector)){
    if(verbose) message("Sector is Miscellaneous/Other. Returning market ETF.")
    return("SPY")
  }
  stop(paste0("Sector ETF could not be found for ",sector))
}
