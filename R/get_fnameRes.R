#'@export
get_fnameRes <- function(bname, param, what) {
  
  paramnames <- list("binsvm"=c("sigma", "C"),
                     "bsvm"=c("sigma", "cNeg", "cMultiplier"),
                     "maxent"=c("fc", "beta"),
                     "ocsvm"=c("sigma", "nu")
                     )

  if (all(paramnames[["binsvm"]] %in% names(param))) {
    method = "binsvm"
    paramlist <- lapply(paramnames[["binsvm"]], 
                       function(nm) param[[which(names(param)==nm)]])
  }
  if (all(paramnames[["ocsvm"]] %in% names(param))) {
    method = "ocsvm"
    paramlist <- lapply(paramnames[["ocsvm"]], 
                       function(nm) param[[which(names(param)==nm)]])
  }
  if (all(paramnames[["bsvm"]] %in% names(param))) {
    method = "bsvm"
    paramlist <- lapply(paramnames[["bsvm"]], 
                       function(nm) param[[which(names(param)==nm)]])
  }
  if (all(paramnames[["maxent"]] %in% names(param))) {
    method = "maxent"
    paramlist <- lapply(paramnames[["maxent"]], 
                       function(nm) param[[which(names(param)==nm)]])
  }
  names(paramlist) <- paramnames[[method]]

  
  addAttrSigma <- FALSE
  if (method=="bsvm") {
    if (is.factor(paramlist$sigma)) {
      dir_cache=paste0(bname, "cache_sigma")
      dir.create(dir_cache, showWarnings=FALSE)
      pat1 <- paste0(bname, "sigma")
      pat2 <- paste0("_cNeg", paramlist$cNeg[1],
                    "_cMultiplier", paramlist$cMultiplier[1],
                    "_fitUpred.rds")
      # cache the results for this experiment
      file_cache <- paste0(dir_cache, "/", basename(pat1), ".rds")
      if (!file.exists(file_cache)) {
        pats <- paste0(pat1, "*", pat2)
        fnames <- Sys.glob(pats)
        if (length(fnames)==0)
          stop(cat(sprintf(
            "Cannot find files for pattern:\n%s\n", pats)))
        sigmas <- gsub(pat1, "", fnames)
        sigmas <- gsub(pat2, "", sigmas)
        sigmas.num <- sort(as.numeric(sigmas), index.return=TRUE)
        sigmas <- sigmas[sigmas.num$ix]
        names(sigmas) <- letters[1:length(sigmas)]
        saveRDS(sigmas, file=file_cache)
      } else {
        sigmas <- readRDS(file_cache)
      }
      new.sigma <- sigmas[as.character(paramlist$sigma)]
      paramlist["sigma"] <- new.sigma
      addAttrSigma <- TRUE
      }
    }
  
  fname <- 
    paste0(bname, 
           paste(names(paramlist), sapply(paramlist, as.character),
                 sep="", collapse="_"), "_",
           what, ".rds")
  
    if (!file.exists(dirname(fname[1])))
      dir.create(dirname(fname[1]), showWarnings=FALSE, recursive=TRUE)
  
  if (addAttrSigma)
    attr(fname, "sigma") <- new.sigma
  
  return(fname)
  
}