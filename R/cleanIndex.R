#' Clean index inputs
#'
#' This function uses input parsing and replacement to coerce the character vector input
#' into the index that the user was referencing. Primary useage is found in other
#' \code{smif.package} functions which require uniform index identifiers.
#'
#' @param index Character; string that must be coerced into an index identifier
#' @param stops Logical; whether the function should stop and throw an error message if the ticker
#' cannot be coerced
#'
#' @return Character; string representation of the given index
#'
#' @details \code{cleanIndex} will attempt to remove all non alphanumeric characters from \code{index} and
#' will coerce all alphabetic characters to lowercase. It then compares them to possible formatted spellings
#' of various indexes from \code{indexIdentifier} until a match is found. If no match is found, returns an error.
#'
#@author Alec Kulakowski, \email{alecthekulak@gmail.com}
#'
#' @family data cleaning functions
#' @seealso \code{\link{indexIdentifier}}: internal dataset used to identify trivial cases
#'
#' @concept clean index
#' @export
#' @examples
#' cleanIndex("S&P 500")
"cleanIndex" <- function(index, stops = TRUE){
  indexes <- smif.package::indexIdentifier()              #smif.package::indexIdentifier
  if(index %in% indexes) return(index)
  # index <- tolower(index) %>% gsub(pattern='[ &^]+|index', replacement='')
  index <- tolower(index) %>% gsub(pattern='[[:blank:][:punct:]]+|index', replacement='')
  if(index %in% tolower(indexes))  return( indexes[which(tolower(indexes) == index)] )
  if(index %in% c('nasdaq100', 'ndx', 'nasdaq')){
    return("NDX")
  }else if(index %in% c('sp500','gspc', 'spy', 'sp')){
    return("SPY")
  }else if(index %in% c('djia', 'dia', 'dji','dow', 'thedow', 'dowjones', 'dowjonesindustrial', 'dowjonesindustrialaverage')){
    return("DJIA")
  }else if(index %in% c('russel3000', 'rusell3000', 'rusel3000', 'russell3000')){
    return("IWV")
    #https://institutional.vanguard.com/
  }else{
    if(stops){
      stop("Could not coerce 'index' to uniform identifier")
    }else{
      print("Could not coerce 'index' to uniform identifier")
      return(index)
    }
  }
}
#' Valid identifiers for common indexes
#'
#' A dataset containing index identifiers in a uniform manner. This dataset is currently a stub.
#' More values will be added as \code{cleanIndex} functionality is expanded. All elements of
#' \code{indexIdentifiers} currently work as explicit or rough inputs to \code{cleanIndex}.
#'
#' @format Character; vector of index identifiers as strings. (Current length = 4)
#' @keywords internal
#' @source \link{indexIdentifier}
#' @examples
#' "SPY" %in% indexIdentifier()
#' @export
"indexIdentifier" <- function(){ c("NDX", "SPY", "DJIA", "IWV") }
#' Clean accounting data
#'
#' Accepts formatted accounting data (as would be found in data imported from \code{.csv},
#' \code{.xls}, or \code{xlsx} files, as well as from webscraping) with dollar signs and
#' character notations for trillion, billion, and million. Converts the input into a numeric
#' that can be used in operations that require type as.numeric.
#'
#' @note Data with a suffix of a single "m" or "M" character will not be considered "1000"
#' but will be interpreted as one million (1000000).
#'
#' @param acc Character; string of a number in accounting format (i.e. "$792.76B")
#' @return Numeric; non-formatted version of the input argument
#' @family data cleaning functions
#' @examples
#' cleanAccount("$792.76B")
#' cleanAccount("435.5 mn")
#' @export
"cleanAccount" <- function(acc){
  temp <- gsub("[$|[:alpha:]|[:blank:]]+", "", acc) %>% as.numeric
  if(grepl("[Mm]{4}|[Tt]", acc)){
    #Multiply by one trillion
    temp = temp*1000000000000
  }else if(grepl("[Mm]{3}|[Bb]", acc)){
    #Multiply by one billion
    temp = temp*1000000000
  }else if(grepl("[Mm]{1,2}", acc)){
    temp = temp*1000000
  }
  return(temp)
}

#' Clean sector names
#'
#' This function takes a character vector input and parses through the SMIF Asset Allocation
#' \code{sectors} dataset and attempts to coerce the input into a sector compliant with SMIF AA
#' sector formatting guidelines.
#'
#' @param sector Character; string to be transformed into a uniform sector
#' @param verbose Logical; whether the function should throw messages. Defaults to getOption("verbose")
#'
#' @return Character; string representation of a sector
#' @family data cleaning functions
#' @concept clean index
#'
#' @examples
#' cleanSector("Consumer Non-Durables")
#' @export cleanSector
"cleanSector" <- function(sector, verbose = getOption("verbose", FALSE)){
  # Variable cleaning
  inputVal <- sector
  sectorData <- smif.package::smif_aa$sectors
  sector <- tolower(gsub("[[:blank:]-]+", "", sector))
  choices <- tolower(gsub("[[:blank:]-]+", "", sectorData$sectorName))
  # Checks for trivial case
  if(!is.na(  pmatch(sector, choices)  )){
    return( sectorData$sectorName[ pmatch(sector, choices) ])
  }
  # If in Consumer Discretionary or Consumer Staples
  if(grepl("nondurable|staple", sector)){
    return( sectorData$sectorName[ 2 ])
  }else if(grepl("durable|service|discretion", sector)){
    return( sectorData$sectorName[ 1 ])
  }
  # Utilities
  if(grepl("util", sector)){
    return( sectorData$sectorName[ 8 ] )
  }
  # Industrials
  if(grepl("indust|goods|transport", sector)){
    return( sectorData$sectorName[ 6 ] )
  }
  # Last attempt at coersion
  if(!is.na(  pmatch(substr(sector,1,3), substr(choices,1,3)))){
    return( sectorData$sectorName[ pmatch(substr(sector,1,3), substr(choices,1,3)) ])
  }
  # Other/Miscellaneous
  if(verbose) message(paste0("Argument (",inputVal,") could not be coerced to a valid AA-compliant sector. Returning 'Misc'."))
  return("Misc")
}

