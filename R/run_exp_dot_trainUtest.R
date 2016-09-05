#' Title
#'
#' @param rs 
#' @param method 
#' @param param 
#' @param bname 
#' @param seed 
#' @param overwrite 
#' @param debug 
#'
#' @return
#' @export
#'
#' @examples
.trainUtest <- function(rs, method, param, bname, seed,
                        overwrite=FALSE, debug=TRUE) {
  
  fnames.suffix <- c("fitUpred", "ev")
  fnames <- get_fnameRes(bname, param, fnames.suffix)
  names(fnames) <- fnames.suffix
  
  fit <- doOrReadRDS({
    nTr_expected <- table(rs$y[rs$set=="tr"])
    
    #     index=list(Fold1=which(rs$set=="tr"))
    #     summaryFunction <- twoClassSummary
    #     fit <- trainOcc(rs[, -c(1:2)], rs$y, method=method,
    #                     tuneGrid=param, index=index, 
    #                     summaryFunction=summaryFunction,
    #                     allowParallel=FALSE, verboseIter=FALSE)
    
    trControl <- trainControl(method="none",
                              classProbs=TRUE, 
                              savePredictions = TRUE,
                              returnResamp = "all",
                              verboseIter = FALSE, 
                              allowParallel = F)
    
    if (method == "binsvm") {
      fit <- train(rs[rs$set=="tr", -c(1:2)], rs$y[rs$set=="tr"], 
                      method="svmRadial", tuneGrid=param,
                      trControl=trControl,
                      allowParallel=FALSE, verboseIter=FALSE)
    } else {
      fit <- trainOcc(rs[rs$set=="tr", -c(1:2)], rs$y[rs$set=="tr"], 
                      method=method, tuneGrid=param,
                      trControl=trControl,
                      allowParallel=FALSE, verboseIter=FALSE)
    }
    
    # There was a serious bug before such that training data 
    # of the final model contained data from val/te!
    if (method %in% c("binsvm", "bsvm")) {
      # table(rs$y[index$Fold1])
      nTr <- table(fit$finalModel@ymatrix)
      if(!all(nTr %in% nTr_expected))
        stop("Final model does not contains the expected training data.")
    } else if (method=="ocsvm") {
      nTr <- length(fit$finalModel@fitted)
      if(!all(nTr %in% nTr_expected["pos"]))
        stop("Final model does not contains the expected training data.")
    } else if (method=="maxent") {
      nTr <- "Where is it???" # table(fit$finalModel@ymatrix)
      if (!all(nrow(fit$finalModel@presence)==nTr_expected["pos"],
               nrow(fit$finalModel@absence)==nTr_expected["un"]))
        stop("Final model does not contains the expected training data.")
    } else stop("Method not implemented.")
    
    # Moving the validations samples to another list element
    #     set.wo.tr <- rs$set[rs$set!="tr"]
    #     fit$predVa <- fit$pred[set.wo.tr=="va", 
    #                            c("pred", "obs", "pos", 
    #                              "rowIndex", "Resample")]
    #     fit$pred <- fit$pred[set.wo.tr=="te", c("obs", "pos")]
    #     # Removing Training data in order to save disc space
    #     fit$trainingData <- fit$trainingData[NULL, , drop=FALSE] 
    #     fit$trainingDataValUn <- NA
    # NO! now make the predicted data here!
    x <- rs[rs$set!="tr", ]
    idx.va <- x$set=="va"
    idx.te <- x$set=="te"
    pred <- predict(fit, x[, -c(1,2)], type="prob")
    if (!is.null(dim(pred)))
      pred <- pred[, "pos"]
    
    fit$predVa <- data.frame(obs=x$y[idx.va],
                             pos=pred[idx.va],
                             rowIndex=as.integer(rownames(x)[idx.va]))
    fit$pred <- data.frame(obs=x$y[idx.te],
                           pos=pred[idx.te],
                           rowIndex=as.integer(rownames(x)[idx.te]))
    
    fit$trainingData <- fit$trainingData[NULL, , drop=FALSE]
    fit$results <- param
    fit
  }, fnames["fitUpred"], overwrite=overwrite)
  
  if (debug & !method %in% c("binsvm")) {
    # cat("!")
    ans <- strsplit(bname, "/")[[1]]
    n.filename <- 
      c(nP=as.numeric(gsub("nP", "", ans[grep("nP", ans)])),
        nU=as.numeric(gsub("nU", "", ans[grep("nU", ans)])))
    n.index <- 
      c(nP=length(rs$y[rs$set=="tr" & rs$y=="pos"]),
        nU=length(rs$y[rs$set=="tr" & rs$y=="un"]))
    # tbl = table(rs$y[fit$control$index$Fold1])
    # n.fit <- c(nP=tbl["pos"][[1]], nU=tbl["un"][[1]])
    if (any(n.filename != n.index) # | 
        # any(n.filename != n.fit) | 
        # any(n.index != n.fit)
    ) {
      stop("Error - Data not correct!")
    }
  }
  
  # Validation with the hold out samples
  if (sum(rs$set=="va") > 0) {
    # Save the hold out samples such that they can be found in 
    # .resampling
    fnames.rspl <- get_fnameRes(bname, param, "hops_val")
    names(fnames.rspl) <- "hops"
    if (!file.exists(fnames.rspl) | overwrite) {
      pred <- fit$predVa
      saveRDS(pred, fnames.rspl["hops"])
    }
    #     results <- .resampling(rs, method, param, bname,
    #                            resampling="val", seed=seed,
    #                            overwrite=overwrite, debug=FALSE,
    #                            rm_ifNotPuResampl=FALSE)
  }
  
  # Evaluation using the test set
  ev <- doOrReadRDS({
    rng <- range(fit$pred$pos, na.rm=TRUE)
    tr <- seq(rng[1], rng[2], length.out=100)
    ev <- evaluate(p=fit$pred$pos[fit$pred$obs=="pos"], 
                   a=fit$pred$pos[fit$pred$obs=="un"], tr=tr)
    ev@presence <- NA
    ev@absence <- NA
    ev
  }, fnames["ev"], overwrite=overwrite)
  
  out <- .trainUtest_return(fit, ev)
  out <- round(out, 4)
  return(out)
}


#' Title
#'
#' @param fit 
#' @param ev 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
.trainUtest_return <- function(fit, ev, ...) {
  
  F1 <- calc_F1(precision=ev@PPP, recall=ev@TPR)
  mxF1 <- max(F1, na.rm=T)
  idxF1 <- which(F1 == mxF1)[1]
  
  mxK <- max(ev@kappa, na.rm=T)
  idxK <- which(ev@kappa == mxK)[1]
  
  mxCCR <- max(ev@CCR, na.rm=T)
  idxCCR <- which(ev@CCR == mxCCR)[1]
  
  out <- data.frame(auc=ev@auc, mxK=mxK, mxF1=mxF1, mxCCR=mxCCR)
  
  out <- cbind(out,
               get_tprUppp(ev, idxK, prefix="K"),
               get_tprUppp(ev, idxF1, prefix="F1"),
               get_tprUppp(ev, idxCCR, prefix="CCR"),
               as.list(attr(fit, "doOrReadRDS_doTime")[1:3]))
  out
}

#' Title
#'
#' @param precision 
#' @param recall 
#'
#' @return
#' @export
#'
#' @examples
calc_F1 <- function(precision, recall) {
  2*((precision*recall)/(precision+recall)) 
}

#' Title
#'
#' @param ev 
#' @param idx 
#' @param prefix 
#' @param sep 
#'
#' @return
#' @export
#'
#' @examples
get_tprUppp <- function(ev, idx, prefix, sep="_") {
  tpr <- ev@TPR[idx]
  ppp <- ev@PPP[idx]
  df <- data.frame(tpr=tpr, ppp=ppp)
  colnames(df) <- paste(prefix, c("TPR", "PPP"), sep=sep)
  df
}
