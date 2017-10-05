#' SMIF Function and Documentation Package
#'
#' The \code{smif.package} allows members of the SMIF Risk and Asset Allocation teams to store commonly used functions and
#' datasets for use accross multiple devices and within RShiny projects.
#'
#' @details
#' \tabular{ll}{
#'   Package: \tab smif.package \cr
#'   Type: \tab Package \cr
#'   Version: \tab 0.1-7 \cr
#'   Date: \tab 2017-10-05 \cr
#'   License: \tab GPL-3 + file LICENSE \cr
#'   Depends: \tab magrittr(>=1.5) \cr
#'   URL: \tab https://github.com/alec25/smif.package \cr
#' }
#'
#' The package will be hosted on github, and the latest version can be downloaded with the following code:
#' \code{devtools::install_github('alec25/smif.package', dependencies=TRUE, upgrade_dependencies=TRUE)}
#'
#' @section Functions:
#'
#' The current master functions held by \code{smif.package} are as follows:
#' \describe{
#'   \item{\code{\link{getConstituents}}}{Returns the current constituents for a given index}
#'   \item{\code{\link{getSectorWeights}}}{Returns the sector weightings for a given index}
#'   \item{\code{\link{getStockInfo}}}{Returns info for a specified stock}
#' }
#'
#' @docType package
#' @name smif.package
"_PACKAGE"
