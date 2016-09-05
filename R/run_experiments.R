################################################################################
#'run_experiments
#' 
#' @description Run the experiments and save the data to disk.
#'
#' @details 
#' \strong{\code{parcc}} must be a list, 
#' which (at least) contains the following elements: \cr
#' \describe{
#'  \item{\code{dn.res}}{A directory in which to write the raw results.}
#'  \item{\code{seed}}{Vector with seed points 
#'  (single element passed to \code{get_refset})}
#'  \item{\code{nP}}{Vector with the number of positive samples 
#'  (single element passed to \code{get_refset})}
#'  \item{\code{nU}}{Vector with the number of unlabeled samples 
#'  (single element passed to \code{get_refset})}
#'  \item{\code{fset}}{Vector with the predictor/feature set names 
#'  (single element passed to \code{get_refset})}
#'  \item{\code{scaling}}{Vector with scaling type name, currently 
#'  only \code{ccs01}}
#'  \item{\code{method}}{Vector of OCC method names that can be 
#'  passed to \code{\link[oneClass]{trainOcc}}.}
#'  \item{\code{resampling}}{Vector of resampling types. (So far: 
#'  \code{val}, \code{cv10}, \code{cv10_pu})}
#'  \item{\code{param}}{Named list (names as \code{parcc$method}) 
#'  each of which containing the parameters for the respective method.}
#' }
#' \strong{\code{get_refset}} must be a function, which 
#' takes the arguments
#' \describe{
#'  \item{\code{seed}}{Any element of \code{parcc$seed}}
#'  \item{\code{nP}}{Any element of \code{parcc$nP}}
#'  \item{\code{nU}}{Any element of \code{parcc$nU}}
#'  \item{\code{fset}}{Any element of \code{parcc$fset}}
#'  \item{\code{args.rs}}{Optional, but usually a list which is available as 
#'  \code{args.rs} in the user defined function \code{get_refset()}}
#'  } 
#' The function must returns a \code{data.frame} with the columns
#' \describe{
#'  \item{\code{tr}}{logical, TRUE for training pixels}
#'  \item{\code{y}}{the class labels \{0, 1, ..., nClasses\}, with 0: unlabeled)}
#'  \item{\code{...}}{feature columns at positions 3:(nFeatures+2) with any names.}
#'  } 
#'  
#' @param doPar If \code{TRUE} and if possible uses parallel processing. 
#' @param debug ...
#' @param overwrite Set to \code{TRUE} if all the already available results should be overwritten.  
#' @param overwrite_resTable Delete the resTables before collecting the results.
#' @param loadOnly ...
#' @return \code{NULL}
#' @export
run_experiments <- function(parcc=NULL,
                            fun=".trainUtest",
                            # loadOnly=FALSE, 
                            doPar=TRUE, 
                            # nCores=NULL, # used from parcc$nCores 
                            debug=FALSE, 
                            overwrite=FALSE,
                            overwrite_resTable=FALSE,
                            loadOnly=FALSE, ...) {
# @param  parcc The parameter list for the experiments (see details).
# @param  get_refset A function, which returns a reference set. 
# @param args.rs List of additional things that might be required in 
# \code{get_refset}.
# @param loadOnly Currently passed to \code{\link{train}} but is not used there (?).
  if (is.null(parcc))
    parcc <- get_settings()
  get_refset <- parcc[["get_resfet"]]
  args.rs <- parcc[["args.rs"]]
  
  valid_funs <- c(".trainUtest", ".resampling-val",
                   # ".resampling-cv10", ".resampling-cv10_pu", 
                   ".binsvm")
  if (!all(fun %in% valid_funs))
    stop(cat("\"fun\" must match", paste0("\n-", valid_funs)))
  
  # "...": passed to get_refset()
  loopElements <- .TASK_PARAMETER
  loopElements <- c("seed", "nP", "nU", "fset", 
                    "idP", "method")
  
  get_nModels <- function(param) {
    prod(sapply(param, length))
  }  
  get_nExperiments <- function(parcc) {
    nExp <- sapply(loopElements, function(i) 
      length(parcc[[i]]))
    return(nExp)
  }  
  print.fun <- function(namedvect){
    cat(paste("\t", paste(names(namedvect), namedvect, 
                          sep=": "), collapse="\n"))
  }
  nExpComp <- get_nExperiments(parcc)
  nExp <- prod(nExpComp)
  # SOME EXPECTATIONS
  cat(sprintf("# experiments: %d \n", nExp))
  print.fun(nExpComp)
  
  cat("\n\nStarting loops...\n")
  
  print.fun <- function(loopElements, task, parcc) {
    atFrom <- function(pm) {
      at=which(parcc[[pm]]==task[[pm]])
      from=length(parcc[[pm]])
      sprintf("%s/%s", at, from) }
    cat(paste(paste(loopElements, 
                    sapply(loopElements, atFrom), 
                    sep=":", collapse=" | "), "\n"))
  }
  
  print.done <- function(fun, at, from) {
    cat(sprintf("%.1f / %d %s completed (%.1f %%).\n", at, 
                from, fun, round(at/from*100, 1)))
  }
  
  nModelsDone <- 0
  nTasksDone <- 0
  
  if (!exists("cl"))
    cl <- NULL
  if (doPar)
    cl <- register_backend(cl=cl, nCores=parcc$nCores)
  
  fun.bak = fun # in order to ignore wrong .trainUtest 
  # computations du to the different refsets 
  # for .resampling...
  
  nSubTasks = length(fun)
  #   nSubTasks = nSubTasks + 
  #     ifelse(".resampling" %in% fun, length(parcc$resampling)-1, 0) 
  
  # --------------------------------------------------------
  # start loops
  task <- parcc
  for (seed in parcc$seed) {
    task$seed <- seed
    for(nP in parcc$nP) {
      task$nP <- nP
      for(nU in parcc$nU) {
        task$nU <- nU
        for (fset in parcc$fset) {
          task$fset <- fset
          # REFERENCE SET
          rs_allTr <- get_refset(seed=task$seed,
                                 fset=task$fset,
                                 nP=task$nP, nU=task$nU,
                                 args.rs)
          if (debug)
            print(check_refset(rs_allTr))
          
          for(idP in parcc$idP) {
            task$idP <- idP
            for(scl_type in parcc$scaling) {
              task$scaling <- scl_type
              # remove the training samples of the non-idP classes 
              rs <- rs_allTr[rs_allTr$set=="te" | rs_allTr$y %in% c(0, task$idP), ]
              if (debug)
                print(check_refset(rs))
              # 2016-01-10: class-specific scaling using preProcess
              if (task$scaling == "ccs01") {
                idx_scale <- rs$set=="tr" & rs$y==task$idP
                pp <- preProcess(rs[idx_scale, -(1:2)], method="range")
                # This is a fast solution...
                if ("binsvm" %in% parcc$method) {
                  rs_sup <- predict(pp, rs_allTr[rs_allTr$y!=0, ])
                  # print(check_refset(rs_sup))
                  rs_sup$y <- puFactor(rs_sup$y==task$idP, TRUE)
                }
                rs[, -(1:2)] <- predict(pp, rs[, -(1:2)])
              } else {
                stop("Currently only ccs01 scaling is supported.")
              }
              
              # table(rs$y)
              idx4sigest <- c(which(rs$set=="tr" & rs$y==task$idP))
              # table(rs$y[idx4sigest])
              # set.seed(task$seed)                         # rm TVT
              # rm_ifNotPuResampl <- sample(which(rs$y==0), task$nU)
              rs$y <- puFactor(rs$y==task$idP, TRUE)
              for (method in parcc$method) {
                task$method <- method
                cat("\n\n*********************************\n")
                print.fun(loopElements, task, parcc)
                tuneGrid.bak <- 
                  get_tuneGrid(rs[idx4sigest, -c(1,2)],
                               method=task$method,
                               param=task$param[[task$method]],
                               seed=task$seed)
                
                for (ff in fun) {
                  cat("------------------------------------\n")
                  if(ff!=".trainUtest" & task$method%in%c("binsvm")) {
                    next
                    cat(paste0("Skipping ", ff, " for method ", task$method, ".\n"))
                  }
                  idx_rs <- !logical(nrow(rs))
                  stsp <- strsplit(ff, "-")[[1]]
                  ff <- stsp[1]
                  if (length(stsp)==1) { # => .trainUtest
                    # idx_rs[rm_ifNotPuResampl] <- F
                    cat("PREV. removing PU - samples. Now not anymore!\n")
                  } else {
                    task$resampling <- stsp[2]
                    stsp.r <- strsplit(task$resampling, "_")[[1]]
                    if (length(stsp.r)==1) { # => normal resampling
                      #idx_rs[rm_ifNotPuResampl] <- F
                      cat("PREV. removing PU - samples. Now not anymore!\n")
                    }
                  }  
                  #                 if (debug) {
                  #                   add = ifelse(ff==".resampling", task$resampling, "")
                  #                   loggerfile <-
                  #                     paste0(parcc$dn.res, 
                  #                            "logger_sigest_",
                  #                            task2str(task[.TASK_PARAMETER]), ".txt")
                  #                   cat(paste(paste(unique(tuneGrid.bak$sigma), 
                  #                                   collapse=", "), 
                  #                             "-", ff, add, "\n"), 
                  #                       file=loggerfile, append=TRUE)
                  #                 }
                  summaryFile <- get_summaryFile(task$dn.res, task, ff)
                  if (overwrite_resTable)
                    unlink(summaryFile)
                  if (file.exists(summaryFile) & !overwrite & !loadOnly) {
                    tasks_done <- try(read.csv(summaryFile, header=TRUE))  # was header=F!
                    # colnames(tasks_done)[1:length(task$param[[task$method]])] <- 
                    #   names(task$param[[task$method]])
                    idx = matchDf(tuneGrid.bak, tasks_done)
                    if (!all(is.na(idx))) {
                      tuneGrid <- 
                        tuneGrid.bak[-as.numeric(rownames(
                          tuneGrid.bak[!is.na(idx), ])), ]
                    } else {
                      tuneGrid <- tuneGrid.bak
                    }
                  } else {
                    tuneGrid <- tuneGrid.bak
                  }
                  cat(ff, ":", basename(summaryFile), "\n")
                  cat("Number of models - all/to do:", 
                      nrow(tuneGrid.bak), " / ", nrow(tuneGrid), "\n")
                  if (nrow(tuneGrid)>0) {
                    if (substr(task$method, 1, 3) == "bin") {
                      rs_run_exp <- rs_sup 
                    } else { 
                      rs_run_exp <- rs[idx_rs, ]
                    }
                    re <- 
                      run_exp(rs_run_exp,
                              looppar=task,
                              tuneGrid=tuneGrid,
                              fun=ff, 
                              doPar=doPar, 
                              loadOnly=loadOnly,
                              overwrite=overwrite,
                              rm_ifNotPuResampl=rm_ifNotPuResampl)
                    if (class(re)=="try-error")  {
                      print(">>>>>> ERROR IN trainUtest!")
                    }
                  }
                  nTasksDone <- nTasksDone + (1/nSubTasks)
                  print.done("tasks", nTasksDone, nExp)
                }
              }# method
            }# scaling
          }# idP
        }# nU
      }# nP
    }# fset
  }# seed
  if (any(class(cl)=="cluster"))
    stopCluster(cl)
  return(NULL)
}# fun