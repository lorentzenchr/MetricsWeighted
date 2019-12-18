#' Weighted Mean
#'
#' Returns weighted mean of a numeric vector.
#'
#' @importFrom matrixStats weightedMean
#' @param x Numeric vector.
#' @param w Optional non-negative, non-missing case weights.
#' @param ... Further arguments passed to \code{mean} or \code{matrixStats::weightedMean}.
#' @return A length-one numeric vector.
#' @export
#' @examples
#' weighted_mean(1:10)
#' weighted_mean(1:10, w = NULL)
#' weighted_mean(1:10, w = 1:10)
#' @seealso \code{\link{weighted_quantile}}.
weighted_mean <- function(x, w = NULL, ...) {
  if (is.null(w)) {
    return(mean(x, ...))
  }
  if (is.logical(x)) {
    x <- as.numeric(x)
  }
  weightedMean(x, w = w, ...)
}
