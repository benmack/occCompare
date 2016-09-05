#' Add accuracies at specific thresholds to summary file
#' 
#' @details This function loads (with \code{\link{get_summaryFile}}) 
#' the previously with  \code{\link{run_experiments}} calculated
#' results and adds the accuracies for the thresholds minTP_T, 
#' tenTP_T, mxSSSP_T, eqSSSP_T which are derived from the 
#' PU validation data.
#' 
#' @param parcc The experiments settings. If NULL they are 
#' derived with \code{get_settings()}.
#' @param verbose Print progress 
#'
#' @return NULL, adds to summary file
#' @export
add_thVals2resTable <- function(parcc=NULL, verbose=FALSE) {
  if (is.null(parcc))
      parcc <- get_settings()
  
  task.grid <- expand.grid(parcc[c(get_col("TA"), 
                                   "method", "scaling", 
                                   "resampling")],
                           stringsAsFactors = F) %>%
    filter(!method%in%c("binsvm"))
  
  # stop("THE BACKUP CREATES A FOLDER+FILE NAMED IDENTICALLY!")
  backupstring <- paste0("BEFORE_add_thVals2resTable_",  
                         gsub(":", "_", gsub(" ", "_", date())))

  for (i.ta in 1:nrow(task.grid)) {
    if (verbose)
      cat("TASK", i.ta, "/", nrow(task.grid), "\n")
    
    # Load the summary file
    task <- task.grid[i.ta, , drop=F]
    summaryFile <- get_summaryFile(parcc$dn.res, task, ".resampling")
    if (!file.exists(summaryFile))
      next
    summaryFileBAk <- gsub("resTables", paste0("resTables_", backupstring), summaryFile)
    dir.create(dirname(summaryFileBAk), recursive = T, showWarnings = F)
    file.copy(summaryFile, summaryFileBAk)
    
    resTab <- read.csv(summaryFile, header=TRUE)
    
    # Get names of the (eventually) new columns
    names_param <- colnames(resTab)[colnames(resTab)%in%get_col("PA")]
    cnames <- colnames(resTab)
    tnames_all <- c("minTP_T", "tenTP_T", "mxSSSP_T", "eqSSSP_T")
    tnames <- tnames_all[!tnames_all %in% colnames(resTab)]
    if (length(tnames)==0)
      next
    
    bname <- get_fnameResBase(parcc$dn.res, task)
    
    for (i in 1:length(tnames)) {
      resTab <- cbind(resTab, NA)
    }
    colnames(resTab) <- c(cnames, tnames)
    
    # cat("DF #", m, "(", nrow(resTab), "): ")
    for (m in 1:nrow(resTab)) {
      fname.suffix <- paste0("hops_", task$resampling)
      fname <- get_fnameRes(bname, resTab[m, names_param], fname.suffix)
      pred <- readRDS(fname)
      
      if (is.na(resTab$minTP_T[m]))
        resTab$minTP_T[m] <- round(min(pred$pos[pred$obs=="pos"]), 5)
      if (is.na(resTab$tenTP_T[m]))
        resTab$tenTP_T[m] <- round(as.numeric(quantile(
          pred$pos[pred$obs=="pos"], 0.1)), 5)
      tr <- sort(pred$pos[pred$obs=="pos"]) # tr=sort(pred$pos)
      tmp <- dismo:::evaluate(p=pred$pos[pred$obs=="pos"],
                              a=pred$pos[pred$obs=="un"],
                              tr=tr)
      if (is.na(resTab$mxSSSP_T[m]))
        resTab$mxSSSP_T[m] <- round(tr[which.max(tmp@TPR + tmp@TNR)], 5)
      if (is.na(resTab$eqSSSP_T[m]))
        resTab$eqSSSP_T[m] <- round(tr[which.min(abs(tmp@TPR - tmp@TNR))], 5)
    }
    write.csv(resTab, summaryFile, quote=FALSE, row.names=FALSE)
  }
}