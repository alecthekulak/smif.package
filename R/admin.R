#' Administration functions for smif.package
#'
#' Used by various functions to enable or disable advanced functionality.
#' Functions are generally internal, as are advanced variables.
#'
#' \code{.setAdmin} also controls the values of the global \code{".advanced"} variable,
#' as well as the global settings for a reserved version of option("verbose").
#' \code{.getAdmin} should be used to retrieve current statis of the \code{"smif.admin"}
#' global option.
#'
#' TimeFrame is number of years used for import code.
#' @name admin
NULL
#' @rdname admin
#' @keywords internal
#' @export
".setAdmin" <- function(x = TRUE){
  options("smif.admin" = x)
  options("smif.verbose" = x)
  assign(".advanced", x, envir=.GlobalEnv)
}
#' @rdname admin
#' @keywords internal
#' @export
".getAdmin" <- function(){
  getOption("smif.admin", default = FALSE)
}
#' @rdname admin
#' @export
"setTimeFrame" <- function(x = 5L){
  options("smif.time" = x)
}
#' @rdname admin
#' @export
"getTimeFrame" <- function(x = 1L){
  if(is.null(getOption("smif.time"))) setTimeFrame()
  getOption("smif.time") * x
}
#' @rdname admin
#' @export
"getTimeFrame.months" <- function(x = 1L){
  base::months(getTimeFrame(x))
}
#' @rdname admin
#' @export
".showUSER" <- function(...){
  if(getOption("smif.verbose",F)){
    cat(..., sep="")
  }
}
