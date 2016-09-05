register_backend <- function(cl=NULL, nCores=NULL) {
  require(foreach)
  require(doParallel)
  
  if (is.null(nCores))
    nCores <- detectCores()
    try(stopCluster(cl), silent=T)
    cl <- makeCluster(nCores)
    registerDoParallel(cl)
    return(cl)
}
