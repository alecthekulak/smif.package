#' Collection of data relevent to SMIF Asset Allocation
#'
#' Currently contains limited sub-datasets. More will be added in
#' future iterations. Contact package administrator to suggest an addition.
#'
#' @name smif_aa
#' @docType data
#' @usage data(smif_aa)
#' @keywords datasets
#'
#' @format A list of various datasets:
#' \describe{
#'   \item{\code{sectors}}{A data.frame containing the following columns:
#'              \code{sectorName} (sector names),
#'              \code{sectorETF} (corresponding sector ETFs)}
#'   \item{\code{benchmark}}{Character vector the ticker of our benchmark }
#' }
#' @details
#' Sector ETFs as provided by SPDR are \href{http://www.sectorspdr.com/sectorspdr/sectors}{Select Sector SPDR ETFs}
#' with the exception of Telecommunications (see \code{Note}) which is sourced from
#' \href{https://us.spdrs.com/en/etf/spdr-sp-telecom-etf-XTL}{SPDR S&P ETFs}.
#'
#' Data in \code{sectors} are character vectors, not factors.
#' @note
#' The \code{sectors} dataset does not include any non-equity sectors, such as REITs, nor any
#' "miscellaneous" sector categorization such as "other." \code{sectors} and \code{sectorETFs}
#' do contain data on the "Materials" sector, which we do not currently invest in.
#'
#' Sector definitions conflict slightly with those specified by SPDR. They include "Real Estate" which we omit,
#' while they do not have an entry for Telecommunications.
#'
#' @source Marco Gancitano (\email{mgancita@@stevens.edu}), \url{http://www.sectorspdr.com/sectorspdr/sectors}, Telecom:
#' \url{https://us.spdrs.com/en/etf/spdr-sp-telecom-etf-XTL}
#' @examples
#' data(smif_aa)
#' \donttest{
#' smif_aa$sectors
#' smif_aa$benchmark
#' }
"smif_aa"
#DATASET SMIF_AA
# library(devtools)
# sectorName <- c("Consumer Discretionary", "Consumer Staples", "Energy", "Financials", "Health Care",
#                 "Industrials", "Technology", "Utilities", "Telecomunications", "Materials")
# sectorETF <- c("XLY", "XLP", "XLE", "XLF", "XLV",
#                "XLI", "XLK", "XLU", "XTL", "XLB")
# sectors <- data.frame(sectorName, sectorETF, stringsAsFactors=FALSE)
# smif_aa <- list(sectors = sectors,
#                 benchmark = "SPY")
# devtools::use_data(smif_aa, compress = "bzip2", overwrite=TRUE)
