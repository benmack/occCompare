#' Title
#'
#' @param tr 
#' @param method 
#' @param param 
#' @param seed 
#'
#' @return
#' @export
#'
#' @examples
get_tuneGrid <- function(tr, method, param, seed) {
  if (method=="bsvm" & is.character(param$sigma))
    if (is.character(param$sigma)) { # =="sigmaFromSigest"
      tmp <- strsplit(param$sigma, "_")[[1]]
      param$sigma <- NULL
      if (length(tmp)==2)
        param$sigma <- eval(parse(text=tmp[2]))
      
      require(kernlab)
      
      sigmaFromSigest <- get_sigmasFromSigest(tr, seed)
      
      ## abline(h=sigmaFromSigest)
      
      param$sigma <- sort(c(sigmaFromSigest, param$sigma))
      names(param$sigma) <- NULL
    }
  tuneGrid <- expand.grid(param, stringsAsFactors=F)
  return(tuneGrid)
}
