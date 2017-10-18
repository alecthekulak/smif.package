#
#
# "getSlides" <- function(newTicker){
#   # newTicker <- "NVDA"
#   # library(smif.package)
#
#   sector <- getStockInfo.Sector(newTicker)
#   sector_ticker <- getSectorETF()
#   benchmark_ticker <- smif_aa$benchmark
#
#
#   vol6 <- vol * sqrt(252/2)
#   vol12 <- vol * sqrt(252)
#
#   lookback6 <- paste0(Sys.Date() - (365/2),'/')
#   lookback12 <- paste0(Sys.Date() - (365),'/')
#
#   beta6 <- allData[lookback6]
#   beta12 <- allData[lookback12]
#
#   sectorBeta6 <- cov(beta6[,1],beta6[,2])/var(beta6[,2])
#   marketBeta6 <- cov(beta6[,1],beta6[,3])/var(beta6[,3])
#
#   sectorBeta12 <- cov(beta12[,1],beta12[,2])/var(beta12[,2])
#   marketBeta12 <- cov(beta12[,1],beta12[,3])/var(beta12[,3])
#
#   BetaData <- data.frame(matrix(NA,nrow=2,ncol = 3))
#   colnames(BetaData) <- c('Vol','Market Beta','Sector Beta')
#   row.names(BetaData) <- c('6 Month','12 Month')
#   BetaData[1,] <- c(vol6,marketBeta6,sectorBeta6)
#   BetaData[2,] <- c(vol12,marketBeta12,sectorBeta12)
#   return(BetaData)
# }
# # "getSlides" <- function(newTicker, amt = 10000, silent = FALSE, ...){#maybe different name for amt
# #   advanced <- TRUE
# #   dataDate <- Sys.Date() - lubridate::years(3)
# #   positions <- getHoldings.SMIF(auto.assign=F, "goldmansachs", "")
# #   # positions <- getHoldings.SMIF(auto.assign=F, ...)
# #   if(!get0(silent)) print("Retrieving ")
# #   # Retrieve price data for every ticker
# #   rawDataList <- lapply(positions$Ticker,function(ticker){#chagne to positions$ticker
# #     Cl(getSymbols(ticker, src = 'google', auto.assign = F, from = dataDate)) #years is from lubridate
# #   })
# #   priceData <- do.call(merge, rawDataList) %>% setNames(., positions$ticker)
# #   # Retrieve return data for every ticker
# #   returnDataList <- lapply(1:ncol(priceData),function(i){
# #     monthlyReturn(priceData[,i])
# #   })
# #   returnData <- do.call(merge, returnDataList) %>% setNames(., positions$ticker)
# #   # Create results
# #   positionData <- tail(priceData, 1) %>% as.data.frame # %>% `rownames<-`(c("Price"))
# #   positionData[2,] <- positionData[1,] * positions$Shares #should be positions$quantity
# #   portfolioValue <- sum(positionData[2,])
# #   positionData[3,] <- positionData[2,] / portfolioValue
# #   rownames(positionData) <- c("Price", "Value", "PctWeight")
# #   # Calculate portfolio data
# #   portfolioReturns <- xts(returnData %*% as.numeric(positionData[3,]), order.by=index(returnData))
# #   portVaR <- VaR(R=portfolioReturns, p=0.95, method="historical")
# #   ##############################
# #   # Now with added ticker
# #   newPrices <- Cl(getSymbols(newTicker, src = 'google',auto.assign = F, from = dataDate))
# #   newRets <- monthlyReturn(newPrices) %>% `colnames<-`(c(newTicker))
# #   positionData <- cbind(positionData, c(as.numeric(tail(newPrices,1)), amt, 0))
# #   colnames(positionData)[ncol(positionData)] <- c(newTicker)
# #   positionData[3,] <- positionData[2,]/sum(positionData[2,])
# #   newReturnData <- merge(returnData, newRets)
# #   # New portfolio statistics
# #   newPortfolioReturns <- xts(newReturnData %*% as.numeric(positionData[3,]), order.by=index(newReturnData))
# #   newPortVaR <- VaR(R=newPortfolioReturns, p=0.95, method="historical")
# #
# #
# #
# # }
