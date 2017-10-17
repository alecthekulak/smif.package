
#' Clean index input
#'
#' This function takes a character vector input and uses input parsing and replacement to coerce the
#' text into the index that the user was most likely attempting to refer to. This is done to ensure that
#' other \code{smif.package} functions are able to refer to input indexes in a uniform way.
#'
#' @param index Character; string that must be coerced into an index identifier
#' @param stops Logical; whether the function should stop and throw an error message if the ticker
#' cannot be coerced
#'
#' @return A character vector that corresponds to a given index
#'
#' @details \code{cleanIndex} will attempt to remove all non alphanumeric characters from \code{index} and
#' will coerce all alphabetic characters to lowercase. It then compares them to possible formatted spellings
#' of various indexes from \code{indexIdentifier} until a match is found. If no match is found, returns an error.
#'
#' @author Alec Kulakowski, \email{alecthekulak@gmail.com}
#' @seealso \code{\link{smif.package}} \code{\link{indexIdentifier}}
#'
#'
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' cleanIndex("s & p 500")
#' }
"cleanIndex" <- function(index, stops = TRUE){
  if(index %in% indexIdentifier){ return(index) }
  index <- tolower(index) %>% gsub(pattern='[ &^]+|index', replacement='')
  if(index %in% c('nasdaq100', 'ndx', 'nasdaq')){
    return("NDX")
  }else if(index %in% c('sp500','gspc', 'spy', 'sp')){
    return("SPY")
  }else if(index %in% c('djia', 'dia', 'dji','dow','dowjones')){
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
#' A dataset containing index identifiers in a uniform manner
#'
#' @format A character vector of possible identifiers. Length = 4
#' @keywords internal
#' @source \link{indexIdentifier}
"indexIdentifier" <- c("NDX", "SPY", "DJIA", "IWV")
#' Clean accounting data
#'
#' Accepts formatted accounting data (as would be found in data imported from \code{.csv},
#' \code{.xls}, or \code{xlsx} files, as well as from webscraping) with dollar signs and
#' character notations for trillion, billion, and million. Converts the input into a numeric
#' that can be used in operations that require type as.numeric.
#'
#' @note Data with a suffix of a single "m" or "M" character will not be considered "1000"
#' but will be interpreted as one million (1000000).
#' @param acc Character; string of a number in accounting format (i.e. "$792.76B")
#' @return a numeric representation of the input string
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


