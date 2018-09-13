#' Get VaR and ES metrics
#'
#' Retrieves metrics related to risk for a proposed addition. Additions specified by
#' tickers of new positions and corresponding sizes (in USD).
#'
#' Retrieves current holdings in SMIF portfolio, determines current portfolio Value at
#' Risk and Expected Shortfall metrics. Determines portfolio metrics for new "proposed"
#' portfolio (with additions), returns comparisons.
#'
#' All metrics are calculated with a 95% confidence interval and for monthly data.
#' If length of \code{amt} is less than length of \code{ticker.new}, the function will
#' attempt to expand \code{amt} to match number of tickers.
#'
#' @param ticker.new Character; a string or vector of strings specifying tickers
#' @param amt Numeric; a numeric or vector of numerics specifying a dollar amount of the
#' ticker to add.
#' @param ... additional parameters (passed to \code{getHoldings.SMIF})
#'
#' @return
#' A \code{list} of following objects:
#' \itemize{
#'   \item{Data.frame; VaR/ES data, in USD and percentages, before and after addition}
#'   \item{Numeric; percentage change in size of portfolio caused by additions}
#'   \item{Numeric; percentage of total portfolio comprised by additions}
#' }
#' @examples
#' \donttest{
#' getRiskMetrics(c("NVDA", "T", "VZ"), amt = c(10000, 4500, 7500))
#' }
#' @seealso \code{\link{getHoldings.SMIF}}
#' @family data processing functions
#' @import lubridate
#' @importFrom PerformanceAnalytics VaR ETL
#' @importFrom quantmod getSymbols Cl monthlyReturn
#' @importFrom stats setNames
#' @export getRiskMetrics
"getRiskMetrics" <- function(ticker.new, amt = 12500L, ...) {
  #.setAdmin()
  # Get position's potential impact on portfolio VaR
  #
  # Args:
  #   ticker.new: The ticker of the stock you want data for (can be vector)
  #   amt: Dollar value of the stock proposed to be added (can be vector [matching ^])
  #   ...: additional parameters:
  #      sector: The sector (for sector AA calculations)
  #
  # Returns:
  #   (data.frame) VaR info, in USD
  #
  # Input verification -------------------------------------------------------------------------------------
  if(length(ticker.new) > length(amt)){
    amt <- rep(amt, length(ticker.new))
  }
  # Load data -------------------------------------------------------------------------------------
  # options("verbose" = FALSE)
  # options("getSymbols.yahoo.warning"=FALSE)
  .showUSER("Fetching current holdings from SMIF server...")
  .adminState = .getAdmin()
  .setAdmin(TRUE)
  positions <- getServerData(what = "holdings", adv = F) #remove
  .setAdmin(.adminState)
  # remove \/
  # if(is.data.frame(get0(".holdings.adv", envir=globalenv()))){
  #   positions <- get0(".holdings.adv", envir=globalenv()) #get0("holdings.advanced", envir=globalenv())
  # }else{
  #   .adminState = .getAdmin()
  #   .setAdmin(TRUE)
  #   positions <- getServerData(what = "holdings", adv = F) #remove
  #   # positions <- getHoldings.SMIF(auto.assign=F, ...)
  #   # assign(".holdings.adv", positions, envir=globalenv())
  #   .setAdmin(.adminState)
  # }
  #remove /\
  # positions$sector <- sapply(X=positions$sector, FUN=cleanSector) #sectors do not matter for this
  # Retrieve prices -----------------------------------------------------------------------------
  .showUSER("Retrieving price data for current holdings...")
  price.data <- getSymbols.SMIF(positions$"Ticker")
  # Retrieve returns ------------------------------------------------------------------------------
  return.data.list <- lapply(1:ncol(price.data),function(i){
    monthlyReturn(price.data[,i]) #this is monthly
  })
  return.data <- do.call(merge, return.data.list)
  return.data <- setNames(return.data, nm=positions$"Ticker")
  # Current position data  ------------------------------------------------------------------------------
  positions$Price <- as.numeric(price.data[nrow(price.data),])
  positions$Value <- positions$Price * positions$"Shares"
  portfolio.size <- sum(positions$Value)
  positions$Weight <- positions$Value / sum(positions$Value)
  # Old portfolio metrics  ------------------------------------------------------------------------------
  .showUSER("Calculating portfolio metrics...")
  portfolio.returns <- xts(return.data %*% positions$Weight, order.by=index(return.data))
  portfolio.VaR <- VaR(R=portfolio.returns, p=0.95, method="historical")[1]
  portfolio.VaR.usd <- portfolio.VaR * sum(positions$Value)
  portfolio.CVaR <- ETL(R=portfolio.returns, p=0.95, method="historical")[1]
  portfolio.CVaR.usd <- portfolio.CVaR * sum(positions$Value)
  # New ticker data  ------------------------------------------------------------------------------
  new.positions <- positions
  new.return.data <- return.data #can do away with these both soon, turn all into w/o "new."
  .showUSER("Fetching data for proposed additions...")
  for(i in 1:length(ticker.new)){
    TICKER <- ticker.new[i]
    AMT <- amt[i]
    ticker.price <- Cl(getSymbols(TICKER,src = 'yahoo',auto.assign = F,
                                  from = Sys.Date() - 1 - getTimeFrame.months(12L),
                                  to = Sys.Date() - 1))
    ticker.returns <- monthlyReturn(ticker.price)
    colnames(ticker.returns) <- c(TICKER)
    ticker.sector <- getStockInfo.sector(TICKER) #try putting this in the rbind statement
    last.price <- round( ticker.price[[ length(ticker.price) ]], digits=2)
    quantity <- floor(AMT/last.price)
    new.positions <- rbind(new.positions, c(TICKER, quantity, last.price,
                                            quantity*last.price, 0))
    # new.positions <- rbind(new.positions, c(TICKER, quantity, ticker.sector,
    #                                         as.character(Sys.Date()), last.price,
    #                                         quantity*last.price, 0))
    new.return.data <- merge(new.return.data, ticker.returns)
  }
  # New portfolio construction  ------------------------------------------------------------------------------
  .showUSER("Calculating portfolio after additions...")
  new.portfolio.size <- sum(as.numeric(new.positions$Value))
  new.positions$Weight <- as.numeric(new.positions$Value) / new.portfolio.size
  new.return.data <- na.approx( new.return.data )
  new.return.data <- na.omit( new.return.data )
  # New portfolio metrics  ------------------------------------------------------------------------------
  new.portfolio.returns <- xts(new.return.data %*% new.positions$Weight,
                               order.by=index(new.return.data))
  new.portfolio.VaR <- VaR(R=new.portfolio.returns, p=0.95, method="historical")[1]
  new.portfolio.VaR.usd <- new.portfolio.VaR * sum(as.numeric(new.positions$Value))
  new.portfolio.CVaR <- ETL(R=new.portfolio.returns, p=0.95, method="historical")[1]
  new.portfolio.CVaR.usd <- new.portfolio.CVaR * sum(as.numeric(new.positions$Value))
  # Comparison ------------------------------------------------------------------------------
  marginal.VaR.usd <- portfolio.VaR.usd - new.portfolio.VaR.usd #marginal VaR = incremental VaR?
  marginal.CVaR.usd <- portfolio.CVaR.usd - new.portfolio.CVaR.usd #marginal VaR = incremental VaR?
  # Results ------------------------------------------------------------------------------
  .showUSER("Framing results, cleaning up...\n")
  VaR.data <- data.frame(matrix(NA,nrow=2,ncol = 6), stringsAsFactors = FALSE)
  names(VaR.data) <- c('Before Addition','(Before %)','After Addition','(After %)','Change', 'Change (%)')
  row.names(VaR.data) <- c('Value at Risk','Expected Shortfall')
  VaR.data[1,] <- c(portfolio.VaR.usd, 100*portfolio.VaR,  new.portfolio.VaR.usd, 100*new.portfolio.VaR, marginal.VaR.usd, 100*marginal.VaR.usd/portfolio.VaR.usd)
  VaR.data[2,] <- c(portfolio.CVaR.usd, 100*portfolio.CVaR, new.portfolio.CVaR.usd, 100*new.portfolio.CVaR, marginal.CVaR.usd, 100*marginal.CVaR.usd/portfolio.CVaR.usd)
  # Clean and return ------------------------------------------------------------------------------
  VaR.data <- round(VaR.data,2)
  p.s.c <- round(100*(new.portfolio.size - portfolio.size)/portfolio.size , 2)
  a.p <- round(100*(new.portfolio.size-portfolio.size)/new.portfolio.size , 2)
  return(list("VaR.data" = VaR.data, "portfolio.size.change" = p.s.c, "addition.percentage" = a.p))
}
