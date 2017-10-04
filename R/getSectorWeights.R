#' Load sector weightings for indexes
#' 
#' Function to scrape, clean, and format sector weightings for a given index or ETF. 
#' 
#' @param index a character vector specifying the name of the index. Values currently accepted are: \code{c("S&P 500")}
#' 
#' @return a data.frame containing \code{index}'s sector weightings 
#' 
#' @author Alec Kulakowski, \email{alecthekulak@gmail.com}
#' @seealso \code{\link{smif.package}}\code{\link{cleanIndex}}
#' @aliases getSectorWeightings
#' 
#' @details 
#' \code{getSectorWeights} will try to coerce the value of the \code{index} argument into a recognizable ticker using 
#' \code{\link{cleanIndex}} before it attempts to access sector weightings. Data will be loaded from different sources 
#' depending on input arguments:
#' 
#' For S&P 500 data, data will be retreived from the official 
#' \href{"http://us.spindices.com/documents/additional-material/500-sector-representation.xlsx?force_download=true"}{S&P Dow Jones website}
#' 
#' @keywords misc
#' @export
#' @examples 
#' \dontrun{
#' 
#' getSectorWeights("S&P 500")
#' }
"getSectorWeights" <- function(index = "SPY"){
  # Index text correction
  index = cleanIndex(index)
  if(index == "SPY"){
    url <- "http://us.spindices.com/documents/additional-material/500-sector-representation.xlsx?force_download=true"
  }else{
    stop("Invalid input")
  }
  # Imports the raw data from the source
  raw_data <- gdata::read.xls(url, header=TRUE, strip.white=TRUE, 
                              blank.lines.skip=TRUE, pattern="MARKET REPRESENTATION", stringsAsFactors = FALSE)
  new_data <- raw_data[1:11,1:2]
  # Fix formatting errors from souce within sector names
  sector_list <- unlist(lapply(new_data[,1],  function(SECTOR_STR){ paste(unlist(strsplit( SECTOR_STR, split="[*]?[ ]{1,}|[*]+")),collapse=" ") } ))
  # Force percentages into decimal terms 
  PercentComp <- unlist(lapply(new_data[,2], function(percent){as.numeric(sub("%", "", percent))/100}))
  # Arrange final results in formatted dataframe 
  RESULT <- as.data.frame(PercentComp, row.names = sector_list)
  return(RESULT)
}
"getSectorWeightings" <- getSectorWeights

#' @describeIn getSectorWeights A no-input version of \code{getSectorWeights}. Runs \code{getSectorWeights} for the S&P 500. 
#' This cannot be stored as a constant dataset as getSectorWeights is not a pure function when taken over long periods of time, as weights are variable.
"getSectorWeights.SPY" <- function(){ getSectorWeights("SPY") }
"getSectorWeightings.SPY" <- getSectorWeights.SPY