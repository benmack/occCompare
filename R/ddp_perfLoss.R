#' Title
#'
#' @param x 
#' @param metric 
#' @param best A string giving the benchmarck, i.e. the 
#' accuracy from which to substract the accuracy realized with 
#' another approach. This must be a character combined by a 
#' value of x$ms and x$ts separated by a dot, e.g. the default 
#' \code{star.k.k.}.
#' the realized accuracy 
#' @param others 
#'
#' @return
#' @export
ddp_perfLoss <- function(x, metric="mxK", best="star.k.k", 
                         others=NULL) {
  
  if (!(metric%in%colnames(x)))
    stop("'metric' must be in 'colnames(x)'.")
  
  x$tempCol <- paste(x$method, x$ms, sep=".")
  
  if (!(best%in%unique(x$tempCol)))
    stop("'best' must be a combination of 'paste(x$method, x$ms, sep='.')'.")
  
  if (is.null(others)) {
    others <- unique(x[, "tempCol"])
    others <- others[others!=best]
  }
  mx <- x[x[, "tempCol"]==best, metric]
  out <- data.frame()
  for (ot in others)
    out <- rbind(
      out, cbind(x[x[, "tempCol"]==ot, ], 
                 PL=mx-x[x[, "tempCol"]==ot, metric]))
  out <- out[, -which(colnames(out)=="tempCol")]
  out
}
