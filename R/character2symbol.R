#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
character2symbol <- function(x) {
  x <- as.character(x)
  lapply(x, as.symbol)}