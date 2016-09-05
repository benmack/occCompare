#'@export
check_rdf <- function(rdf) {
  # Ignore these rows when searching for rows that need to be fixed
  .ignore <- list(maxent=c("P.eeotaod", "P.eeotaod.SD"),
                  bsvm=c("P.at0", "P.at0.SD", "P.at0.AG"))
  na.rows <- list()
  for (method in names(rdf)) {
    ignore <- .ignore[[method]]
    cat("----------------------------------------------\n")
    cat("----", method, " => ")
    
    # Check for NA elements
    # get_na_cols(rdf[[method]])  # ok
    na.rows[[method]] <- get_na_rows(rdf[[method]], ignore_cols=ignore)
  }
  invisible(na.rows)
}

#'@name load_results
#'@title Load results written by \code{run_experiments}.
#'@description ...
#'@param bdir The base directory with the summary tables written by run_experiments/run_exp.
#'@param fullResults if \code{TRUE} the full results table is returned, 
#'i.e. all eperiments/models defined in \code{get_settings()} even if they are not 
#'found in the results stored on disk (=> \code{NA}).
#'@export
load_results <- function(what, bdir=NULL, fullResults=T, 
                         maxentDef=TRUE, verbosity=1,
                         file=NULL, overwrite=FALSE) {
  
  if (is.null(file))
    file = get_settings()$get_fn_ressum("results")
  if (file.exists(file))
    cat("Loading results from", file, "\n")
  resDf <-   doOrReadRDS({
    parcc <- get_settings()
    if (is.null(bdir)) 
      bdir <- parcc$dn.res
    bdir=parcc$dn.res
    
    parccDf <- expand.grid(parcc[.TASK_PARAMETER], stringsAsFactors=F)
    
    nTasks <- nrow(parccDf)
    
    taskColsAdd <- .TASK_PARAMETER[# sapply(parcc[.TASK_PARAMETER], length) > 1 &
      !.TASK_PARAMETER=="method"]
    
    resList <- list()
    resDf <- vector("list", length(parcc$method))
    names(resDf) <- parcc$method
    
    addNewRows <- FALSE # this was TRUE before but now we collect only what we want
    
    for (i in 1:nrow(parccDf)) {
      task <- parccDf[i, ]
      taskstr <- task2str(task)
      resList[[i]] <- NA
      names(resList[[i]]) <- taskstr
      if (task$method == "binsvm") {
        modparams <- c("sigma", "C")
      } else if (task$method == "bsvm") {
        modparams <- c("sigma", "cNeg", "cMultiplier")
      } else if (task$method == "maxent") {
        modparams <- c("fc", "beta")
      } else if (task$method == "ocsvm") {
        modparams <- c("sigma", "nu")
      } else {
        stop(paste0("Unknown method: ", sQuote(task$method)))
      }
      expected_models <- expand.grid(parcc$param[[task$method]])
      if (verbosity > 0)
        cat(sprintf("%d/%d - %s\n", i, nTasks, taskstr))
      
      # df.i <- data.frame()
      # Init with expected_models
      df.i <- expected_models
      
      for (ww in what) {
        if (ww == ".resampling") {
          for (resamp in parcc$resampling) {
            if (verbosity > 0)
              cat("\t-", ww, ", ", resamp, "\n")
            task$resampling = resamp
            summaryFile <- get_summaryFile(bdir, task, ww)
            if (!file.exists(summaryFile)) {
              warning(paste("File does not exists:", summaryFile))
              # tasks_done <- as.data.frame(as.list(modparams))[-1, ]
              # names(tasks_done) <- modparams
              next
            } else {
              tasks_done <- read.csv(summaryFile, header=TRUE)
            }
            if (nrow(tasks_done) < nrow(expected_models)) {
              warning(paste("Missing results:", summaryFile))
            }
            # summaryFile <- joinpath(bdir, get_summaryFile(bdir, task, ww))
            tasks_done <- read.csv(summaryFile, header=TRUE)
            tasks_done <- unique(tasks_done)
            if (length(df.i)==0) {
              df.i <- tasks_done
            } else {
              df=df.i
              df.i <- combineDf(tasks_done, df, 
                                commonCols=modparams,
                                addNewRows=addNewRows,
                                verbose=verbosity>1)
            }
          }
        } else {
          if (verbosity > 0)
            cat("\t-", ww, "\n")
          summaryFile <- get_summaryFile(bdir, task, ww)
          if (!file.exists(summaryFile)) {
            warning(paste("File does not exists:", summaryFile))
            # tasks_done <- as.data.frame(as.list(modparams))[-1, ]
            # names(tasks_done) <- modparams
            next
          } else {
            tasks_done <- read.csv(summaryFile, header=TRUE)
          }
          if (nrow(tasks_done) < nrow(expected_models)) {
            warning(paste("Missing results:", summaryFile))
          }
          
          tasks_done <- unique(tasks_done)
          if (length(df.i)==0) {
            df.i <- tasks_done
          } else {
            df.i <- combineDf(tasks_done, df.i, 
                              commonCols=modparams,
                              addNewRows=addNewRows,
                              verbose=verbosity>1)
          }
        }
      }
      
      # CHECK:
      #     df.i <- df.i[c(6:7, 1:3), ]
      #     combineDf(df.i, expected_models,
      #               commonCols=modparams,
      #               addNewRows=TRUE)[1:10,]
      #     if (fullResults) {
      #       if (nrow(df.i)==0) {
      #         df.i <- expected_models
      #         ans <- colnames(resDf[[task$method]])
      #         ans <- ans[!ans %in% c(taskColsAdd,
      #                                colnames(expected_models))]
      #         df.i <- cbind(df.i, lapply(ans, function(x) as.numeric(NA)))
      #         colnames(df.i) <- c(colnames(expected_models), ans)
      #       } else {
      #         ans <- try(combineDf(df.i, expected_models,
      #                              commonCols=modparams,
      #                              addNewRows=TRUE))
      #         if (class(ans)=="try-error") {
      #           stop("Cannot generate the full experiments table.")
      #         } else {
      #           df.i <- ans; rm(ans); gc()
      #         }
      #       }
      #     }
      
      # Add the task variables before the collected data
      tmp <- list()
      for (add in taskColsAdd) {
        tmp[[add]] <- factor(rep(task[1, add], nrow(df.i)), 
                             levels=parcc[[add]])
      }
      ans <- cbind(tmp, df.i)
      
      # Add eventuall missing variables behind the collected data
      if (!is.null(resDf[[task$method]])) {
        cnames_exist <- colnames(resDf[[task$method]]) %in% colnames(ans)
        if (!all(cnames_exist)) {
          tmp <- list()
          for (add in colnames(resDf[[task$method]])[!cnames_exist]) {
            tmp[[add]] <- as.numeric(NA)
          }
          ans <- cbind(ans, tmp)
        }
      }
      
      resDf[[task$method]] <- rbind(resDf[[task$method]], ans)
      
      #     temp <- try(rbind(resDf[[task$method]], ans))
      #     if (class(temp)=="try-error") {
      #       class(temp)
      #       head(resDf[[task$method]])
      #       head(ans)  
      #     } else {
      #       resDf[[task$method]] <- temp
      #     }
      
      
      
      
      #     if (is.null(resDf[[task$method]])) {
      #       resDf[[task$method]] <- cbind(tmp, df.i)
      #     } else {
      #       resDf[[task$method]] <- combineDf(cbind(tmp, df.i),
      #                                         resDf[[task$method]],
      #                                         commonCols=c(taskColsAdd,
      #                                                      modparams),
      #                                         addNewRows=T)      
      #
    }
    
    if (maxentDef) {
      resDf$maxentDef <- resDf$maxent %>% 
        mutate(defModel=maxent_is_default(nP, beta, fc)) %>% 
        filter(defModel) %>% dplyr::select(-defModel)
    }
    
    for (method in names(resDf))
      resDf[[method]] <- 
      cbind(method=factor(method, levels=names(resDf)), 
            resDf[[method]]) %>%
      group_by(seed, fset, nP, nU, idP)
    
    resDf}, file=file, overwrite=overwrite)
    
    return(resDf)
}