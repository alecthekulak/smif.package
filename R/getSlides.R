#' Get data for SMIF risk slides
#'
#' Produces the data currently required for pitch risk slides. Returns the default risk slide table with
#' filled-in values (except for implied volatility, which is omitted completely).
#'
#' @param ticker Character; the ticker for the stock
#' @param sector Character; the sector of the \code{ticker}. If ommited, will use \code{getStockInfo.sector}.
#' @param use.rfr Logical; whether to use the risk-free rate in the beta calculations. Defaults to \code{TRUE}.
#' @return Data.frame; risk slide table with filled in values.
#'
#' @importFrom quantmod getSymbols ClCl
#' @importFrom stats var cov sd
#' @importFrom zoo na.approx index
#' @export getSlides
"getSlides" <- function(ticker, sector = getStockInfo.sector(ticker = ticker), use.rfr = TRUE){
  # Generates slide data for stock pitches
  #
  # Args:
  #   ticker: The ticker of the stock you want data for
  #   sector: The sector (for sector beta calculations)
  #   use.rfr
  #
  # Returns:
  #   (data.frame) risk slide data
  #
  # Load data -------------------------------------------------------------------------------------
  sectorETF = getSectorETF.sector(sector = sector)
  ticker_list <- c(ticker, sectorETF, smif.package::smif_aa$benchmark)     # Uses smif.package::smif_aa
  raw_data_list <- lapply(ticker_list, function(ticker){
    prc <- getSymbols(ticker, src = 'google', auto.assign = F,
                      from = Sys.Date() - 1 - getTimeFrame.months(12L),
                      to = Sys.Date() - 1)
    return( na.omit(ClCl(prc)) )
  })
  raw_data <- do.call(merge, raw_data_list)
  if(use.rfr){
    rfr <- getSymbols('DGS3MO',src = 'FRED', auto.assign=FALSE) %>% na.approx()  #zoo::na.approx
    rfr <- rfr[as.Date(index(rfr)) >= Sys.Date() - getTimeFrame.months(12L)][-1] / 252    #zoo::index
    raw_data <- cbind(raw_data, rfr[index(raw_data)])
  }else{
    raw_data$"rfr" <- 0
  }
  colnames(raw_data) <- c("ticker", "sector", "market", "rfr")

  # This is now Standard Deviation
  daily_vol <- sd(raw_data$ticker)                      # stats::sd
  vol_6m <- daily_vol * sqrt(252/2) * 100
  vol_12m <- daily_vol * sqrt(252) * 100

  data_6m <- raw_data[ paste0(Sys.Date() - months(6), "/") ]
  data_12m <- raw_data[ paste0(Sys.Date() - months(12), "/") ]

  data_6m <- data_6m - rep(data_6m$rfr,4)
  data_12m <- data_12m - rep(data_12m$rfr,4)

  sector_6m_beta <- cov( data_6m$ticker, data_6m$sector )/var( data_6m$sector )
  sector_12m_beta <- cov( data_12m$ticker, data_12m$sector )/var( data_12m$sector )
  market_6m_beta <- cov( data_6m$ticker, data_6m$market )/var( data_6m$market )
  market_12m_beta <- cov( data_12m$ticker, data_12m$market )/var( data_12m$market )

  res <- data.frame(matrix(NA,nrow=2,ncol = 3))
  colnames(res) <- c('Vol','Market Beta','Sector Beta')
  rownames(res) <- c('6 Month','12 Month')
  res[1,] <- c(vol_6m, market_6m_beta, sector_6m_beta)
  res[2,] <- c(vol_12m, market_12m_beta, sector_12m_beta)
  return(res)
}

