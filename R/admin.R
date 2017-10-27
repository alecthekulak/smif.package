#' Administration functions for smif.package
#'
#' Used by various functions to enable or disable advanced functionality.
#' Functions are generally internal, as are advanced variables.
#'
#' \code{.setAdmin} also controls the values of the global \code{".advanced"} variable,
#' as well as the global settings for a reserved version of option("verbose").
#' \code{.getAdmin} should be used to retrieve current statis of the \code{"smif.smif.admin"}
#' global option.
#'
#' TimeFrame is number of years used for import code.
#' @import lubridate
#' @aliases admin timeframe time.frame showUSER
#' @name smif.admin
NULL
#' @rdname smif.admin
#' @keywords internal
#' @export
".setAdmin" <- function(x = TRUE){
  options("smif.admin" = x)
  options("smif.verbose" = x)
  # assign(".advanced", x, envir=.GlobalEnv)
}
#' @rdname smif.admin
#' @keywords internal
#' @export
".getAdmin" <- function(){
  getOption("smif.admin", default = FALSE)
}
#' @rdname smif.admin
#' @export
"setTimeFrame" <- function(x = 5L){
  options("smif.time" = x)
  if( !is.null(sys.call(-1L)) ){
    if( sys.call(-1L) != "getTimeFrame()" &
        getTimeFrame() != x ){
      .stock.data <- new.env(parent = globalenv())
    }
  }
}
#' @rdname smif.admin
#' @export
"getTimeFrame" <- function(x = 1L){
  if(is.null(getOption("smif.time"))) setTimeFrame()
  getOption("smif.time") * x
}
#' @rdname smif.admin
#' @export
"getTimeFrame.months" <- function(x = 12L){
  base::months(getTimeFrame(x))
}
#' @rdname smif.admin
#' @keywords internal
#' @export
".getFrom" <- function(x = 12L){
  Sys.Date() - 1 - base::months(getTimeFrame(x))
}
#' @rdname smif.admin
#' @keywords internal
#' @export
".getTo" <- function(){
  Sys.Date() - 1
}
#' @rdname smif.admin
#' @keywords internal
#' @export
".showUSER" <- function(...){
  if(getOption("smif.verbose",F)){
    cat(...,"\n", sep="")
  }
}

