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
#' @aliases getSectorWeightings getSectorWeightings.SPY
#' @import magrittr
#' @importFrom utils download.file
#'
#' @details
#' \code{getSectorWeights} will try to coerce the value of the \code{index} argument into a recognizable ticker using
#' \code{\link{cleanIndex}} before it attempts to access sector weightings. Data will be loaded from different sources
#' depending on input arguments:
#'
#' For S&P 500 data, data will be retreived from the official
#' \href{"http://us.spindices.com/documents/additional-material/500-sector-representation.xlsx?force_download=true"}{S&P Dow Jones website}
#'
#' @keywords misc data
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
    #http://us.spindices.com/documents/additional-material/500-sector-representation.xlsx?force_download=true
    the_url = "http://us.spindices.com/documents/additional-material/500-sector-representation.xlsx"
  }else{
    stop("Invalid input")
    return(NULL)
  }
  # Imports the raw data from the source
  # raw_data <- gdata::read.xls(the_url, header=TRUE, strip.white=TRUE,
  #                             blank.lines.skip=TRUE, pattern="MARKET REPRESENTATION", stringsAsFactors = FALSE)
  # library(xlsx); library(readxl); library(XLConnect)
  # temp_dir <- tempdir()
  # temp_file <- tempfile(tmpdir = temp_dir, pattern = "temp", fileext=".xlsx")
  temp_file = tempfile()
  download.file(url = the_url, destfile = temp_file, mode="wb", cacheOK = FALSE, quiet=TRUE)

  raw_data = xlsx::read.xlsx(temp_file, sheetIndex = 1, startRow=4, header=TRUE, stringsAsFactors=FALSE)
  unlink(temp_file)
  # raw_data <- XLConnect::loadWorkbook(temp_file)
  # aa <- XLConnect::readWorksheet(raw_data,)

  # raw_data<- xlsx::read.xlsx2(temp_file, sheetIndex=1) #, sheetIndex = 1, startRow=4, header=TRUE, stringsAsFactors=FALSE)
  # raw_data <- readxl::read_xlsx(temp_file)
  # raw_data <- readxl::read_excel(temp_file)
  # raw_data <- XLConnect::loadWorkbook(temp_file)
  # unlink(temp_dir)
  #onwards
  new_data <- raw_data[1:11,1:2] %>% na.omit

  # Fix formatting errors from souce within sector names
  sector_list <- unlist(lapply(new_data[,1],  function(SECTOR_STR){ paste(unlist(strsplit( SECTOR_STR, split="[*]?[ ]{1,}|[*]+")),collapse=" ") } ))
  # Force percentages into decimal terms
  #  PercentComp <- unlist(lapply(new_data[,2], function(percent){as.numeric(sub("%", "", percent))}))
  PercentComp <- unlist(lapply(new_data[,2], FUN=as.numeric))
  # Arrange final results in formatted dataframe
  RESULT <- as.data.frame(PercentComp, row.names = sector_list)
  return(RESULT)
}
"getSectorWeightings" <- getSectorWeights

#' @describeIn getSectorWeights A no-input version of \code{getSectorWeights}. Runs \code{getSectorWeights} for the S&P 500. This cannot be stored as a constant dataset as getSectorWeights is not a pure function when taken over long periods of time, as weights are variable.
"getSectorWeights.SPY" <- function(){ getSectorWeights("SPY") }
"getSectorWeightings.SPY" <- getSectorWeights.SPY
