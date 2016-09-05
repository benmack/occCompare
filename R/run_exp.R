#' Title
#'
#' @param rs
#' @param looppar 
#' @param tuneGrid 
#' @param fun 
#' @param doPar 
#' @param loadOnly 
#' @param overwrite 
#' @param rm_ifNotPuResampl 
#'
#' @return
#' @export
#'
#' @examples
run_exp <- function(rs, looppar, tuneGrid, fun=".trainUtest", 
                    doPar=TRUE, loadOnly=FALSE, overwrite=FALSE,
                    rm_ifNotPuResampl=NULL) {
  
  if (looppar$method %in% c("binsvm"))
    looppar$nU <- 0
  
  if (!fun %in% c(".trainUtest", ".resampling"))
    stop("\"fun\" must be \"trainUtest\" or \".resampling\".")
  # looppar must contain:
  # $method:  the name of the current classifier
  # $fset:   the name of the feature set
  paramList <- lapply(1:nrow(tuneGrid), function(i) 
    droplevels(tuneGrid[i,,drop=FALSE]))
  
  bname <- get_fnameResBase(looppar$dn.res, looppar)
  summaryFile <- get_summaryFile(looppar$dn.res, looppar, fun) 
  
  if (any(search()%in%"package:foreach") & doPar) {
    # DOESNT WORK:
    #     foreach(param=paramList, 
    #             .packages=c("oneClass", "dismo", "occCompare")) %dopar%
    #       do.call(paste0("occCompare:::", fun), 
    #               args=list(rs=rs, method=looppar$method,
    #                         param=param, bname=bname))
    if (fun==".trainUtest") {
      res = foreach(param=paramList, 
              .packages=c("oneClass", "dismo")) %dopar%
        occCompare:::.trainUtest(rs, method=looppar$method,
                                 param=param, bname=bname, 
                                 seed=looppar$seed,
                                 overwrite=overwrite)
      
    } else if (fun==".resampling") {
      res = foreach(param=paramList, 
              .packages=c("oneClass", "dismo")) %dopar%
        occCompare:::.resampling(rs, method=looppar$method,
                                 param=param, bname=bname,
                                 resampling=looppar$resampling, 
                                 seed=looppar$seed,
                                 overwrite=overwrite,
                                 rm_ifNotPuResampl=rm_ifNotPuResampl)
    } else {
      stop(cat(sprintf("%s not implemented for parallel processing.", fun)))
    }
    for (i in 1:length(paramList)) {
      addLine2csv(cbind(paramList[[i]], res[[i]]), summaryFile)
    }
  } else {
    todo <- length(paramList)
    percentage_of_todo <- seq(0, 100, by=10)
    milestones <- round(todo*percentage_of_todo/100, 0)
    counter <- 0
    cat(paste0("From 100% (n=", todo, ") done [%]: 0."))
    for (param in paramList) {
      counter <- counter + 1
      if (counter %in% milestones)
        cat(paste0(percentage_of_todo[counter==milestones], "."))
      if (fun==".trainUtest") {
        res = .trainUtest(rs, method=looppar$method,
                          param=param, bname=bname,
                          seed=looppar$seed,
                          overwrite=overwrite)
      } else if (fun==".resampling")  {
        res = .resampling(rs, method=looppar$method,
                          param=param, bname=bname,
                          resampling=looppar$resampling, 
                          seed=looppar$seed,
                          overwrite=overwrite,
                          rm_ifNotPuResampl=rm_ifNotPuResampl)
      }
      #       do.call(fun, args=list(rs=rs, method=looppar$method,
      #                               param=param, bname=bname))
      addLine2csv(cbind(param, res), summaryFile)
    }
    cat("\n")
  }
}
