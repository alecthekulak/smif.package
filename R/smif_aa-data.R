#' Collection of data relevent to SMIF Asset Allocation
#'
#' Currently only contains one sub-dataset. More will be added in
#' future. Contact package administrator to suggest an additoin.
#'
#' @docType data
#' @usage data(smif_aa)
#' @keywords datasets
#'
#' @format A list of various datasets:
#' \describe{
#'   \item{\code{sectors}}{Character vector of all possible sector names}
#' }
#' @note
#' The \code{sectors} dataset does not include any non-equity sectors, such as REITs, nor any
#' "miscellaneous" sector categorization such as "other."
#'
#' @source Marco Gancitano (\url{mgancita@stevens.edu})
#' @examples \donttest{
#' data(smif_aa)
#' smif_aa$sectors
#' }
"smif_aa"
