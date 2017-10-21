#' Administration functions for smif.package
#'
#' Used by various functions to hide advanced functionality
#' @name admin
NULL
#' @rdname admin
#' @keywords internal
#' @export
".setAdmin" <- function(x = TRUE){
  options("smif.package.admin" = x)
  options("verbose" = x)
  .advanced <- TRUE
}
#' @rdname admin
#' @keywords internal
#' @export
".getAdmin" <- function(){
  getOption("smif.package.admin", default = FALSE)
}
