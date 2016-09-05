#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
get_sigmasFromSigest <- function(x, seed) {
  sigmasFromSigest <- 
    sapply(1:100, function(i) {
      set.seed(seed*i)
      sigest(as.matrix(x))
    })
  ## boxplot(t(sigmasFromSigest))
  
  # sigmaFromSigest <- apply(sigmasFromSigest, 1, median)
  
  sigmaFromSigest <- c(
    quantile(sigmasFromSigest[3, ], .9),
    quantile(sigmasFromSigest[2, ], .50),
    quantile(sigmasFromSigest[1, ], .1))
  return(sigmaFromSigest)
}

#' Title
#'
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
get_sigmasFromSigest_allExp <- function(verbose=TRUE) {
  parcc <- get_settings()
  
  tasks <- expand.grid(seed=parcc$seed, nP=parcc$nP, 
                       fset=parcc$fset)
  rslts <- expand.grid(seed=parcc$seed, nP=parcc$nP,
                       fset=parcc$fset, idP=parcc$idP)
  
  rslts <- cbind(rslts, "90%"=NA)
  rslts <- cbind(rslts, "50%"=NA)
  rslts <- cbind(rslts, "10%"=NA)
  if (verbose)
    cat("# Tasks: ", nrow(tasks), "\n")
  for(i in 1:nrow(tasks)) {
    if (verbose)
      cat(i, ".")
    rs <- parcc$get_refset(seed=tasks$seed[i], fset=tasks$fset[i], 
                     nP=tasks$nP[i], nU=parcc$nU[1],
                     nTe=parcc$nTe, args.rs=parcc$args.rs, overwrite=F)
    for (idP in parcc$idP) {
      idx4sigest <- c(which(rs$set=="tr" & rs$y==idP))
      ans <- get_sigmasFromSigest(rs[idx4sigest, -c(1,2)], tasks$seed[i])
      rw <- rslts$seed==tasks$seed[i] & rslts$nP==tasks$nP[i] & 
        rslts$fset==tasks$fset[i] & rslts$idP==idP
      sum(rw)
      rslts[rw, (ncol(rslts)-2):ncol(rslts)] <- ans
    }
  }
  return(rslts)
}
