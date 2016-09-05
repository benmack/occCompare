#'@export
get_fnameResBase <- function(basedir, params, checkParams=TRUE) {
  # Parse a basename for the expereiment results.
  # The Function must contain a (one-row) data frame or a 
  # list with the named elements: 
  # seed, fset, method, nP, nU, idP, scl
  
  check <- c("seed", "fset", "method", "nP", "nU", "idP", "scaling")
  
  idx_null <- sapply(params[check], is.null)
  if (any(idx_null) & checkParams)
    stop(paste("Parameter(s) not available:", 
               paste0(check[idx_null], collapse=", ")))

  idx_len <- sapply(params[check], length)!=1
  if (any(idx_len) & checkParams)
    stop(paste("Parameter(s) length is not 1:", 
               paste0(check[idx_len], collapse=", ")))

  bname <- 
    paste0(basedir, "/", "seed", params$seed, "/", 
           params$fset, "/", params$method, "/nP", 
           params$nP, "/nU", params$nU, "/idP", params$idP, 
           "/", params$scaling, "/")

  return(bname)
}

