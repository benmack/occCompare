#'@export
.resampling <- function(rs, method, param, bname, 
                        resampling="cv10_pu", seed=NULL, 
                        overwrite=FALSE, debug=TRUE,
                        rm_ifNotPuResampl=NULL,
                        pred=NULL # 
) { 
  use_OptimalCutpoints <- FALSE
  fnames.suffix <- paste0("hops_", resampling)
  fnames <- get_fnameRes(bname, param, fnames.suffix)
  names(fnames) <- "hops"
  
  dir_thresholds <- gsub(".rds", "", 
                         get_fnameRes(bname, param, 
                                      paste0('th_', resampling)))
  
  resampl.param <- resampl_param_from_str(resampling)
  
  #   if (all(file.exists(fnames)))
  #     return(NULL)
  
  pred <- doOrReadRDS({
    rs.tr <- rs[rs$set=="tr", -1]
    if (resampl.param$pu) {
      np <- sum(rs.tr$y=="pos")
      nu <- sum(rs.tr$y=="un")
      # indepUn <- (1+np+nu/2):(np+nu)  # old
      indepUn <- rm_ifNotPuResampl      # new
      # table(rs.tr$y[indepUn])
      set.seed(seed)
      index <- createFoldsPu(rs.tr$y, resampl.param$n, indepUn=indepUn, seed=seed)
      fullSet <- paste0(rownames(rs.tr)[1:(np+nu/2)], collapse="")
      # lapply(1:resampl.param$n, function(i) table(rs.tr$y[index[[i]]]))
    } else {
      set.seed(seed)
      if (is.na(resampl.param$n)) {# val case
        index <- list(Fold1=1:nrow(rs.tr))
      }else {
        index <- createFolds(rs.tr$y, resampl.param$n, returnTrain=T)
      }
      fullSet <- paste0(rownames(rs.tr), collapse="")
      # lapply(1:k, function(i) table(rs.tr$y[index[[i]]]))
    }
    if (method=="maxent") {
      method4trainOcc = occCompare:::maxent_methList
      head(method4trainOcc$fit)
      resamplIdentifier <- sapply(index, function(i)
        paste0(rownames(rs.tr)[i], collapse=""))
      resamplIdentifier <- c(resamplIdentifier, 
                             fullSet=fullSet)
    } else {
      method4trainOcc = method
      resamplIdentifier <- NULL
    }
    fit <- trainOcc(rs.tr[, -1], rs.tr$y, method=method4trainOcc, 
                    tuneGrid=param, index=index, 
                    allowParallel=FALSE, verboseIter=FALSE,
                    path=dir_thresholds, 
                    deleteMaxentOutput=TRUE, 
                    resamplIdentifier=resamplIdentifier)
    # rn_finalModel <- rownames(fit$trainingData)
    # rn_resampl <- lapply(index, function(i) rownames(rs.tr)[i])
    # lapply(rn_resampl, function(i) (rn_finalModel %in% i))
    
    pred <- fit$pred[, c("pred", "obs", "pos", "rowIndex", "Resample")]
    pred
    # How to re-produce the fit$results:
    #   results.k <- t(sapply(unique(pred$Resample), 
    #     function(set) summaryFunction(subset(pred, Resample==set))))
    #   colMeans(results.k)
    #   fit$results
    rm(fit)
  }, fnames["hops"], overwrite=overwrite & !resampling=="val")  # returns pred
  
  # -------------------------------------------------------
  # debug
  if (debug) {
    # cat("!")
    ans <- strsplit(bname, "/")[[1]]
    n.filename <- 
      c(nP=as.numeric(gsub("nP", "", ans[grep("nP", ans)])),
        nU=as.numeric(gsub("nU", "", ans[grep("nU", ans)])))
    tbl = table(pred$obs)
    if (resampl.param$pu)
      tbl["un"] <- tbl["un"] / resampl.param$n
    n.index <- c(nP=tbl["pos"][[1]], nU=tbl["un"][[1]])
    if (any(n.filename != n.index)) {
      stop("Error - Data not correct!")
    }
  }
  
  # -------------------------------------------------------
  uResampl <- unique(pred$Resample)
  if (is.null(uResampl)) { # val case!
    valCase <- TRUE
    uResampl <- "val"
    pred$Resample <- "val"
  } else {
    valCase <- FALSE
    }
  # -------------------------------------------------------
  # threshold dependent metrics
  
  # the thresholds
  if (valCase) {
    # thresholds (in SDM often used, 
    # see Guillera-Arroita et al. 2015: 
    # Is my species distribution fit for purpose?)
    minTP <- min(pred$pos[pred$obs=="pos"])
    tenTP <- as.numeric(quantile(pred$pos[pred$obs=="pos"], 0.1))
    tr <- sort(pred$pos[pred$obs=="pos"]) # tr=sort(pred$pos)
    tmp <- dismo:::evaluate(p=pred$pos[pred$obs=="pos"],
                            a=pred$pos[pred$obs=="un"],
                            tr=tr)
    mxSSSP <- tr[which.max(tmp@TPR + tmp@TNR)]
    eqSSSP <- tr[which.min(abs(tmp@TPR - tmp@TNR))]
    
    th4modsel <- data.frame(Resample=uResampl, minTP=minTP, 
                            tenTP=tenTP, mxSSSP=mxSSSP, 
                            eqSSSP=eqSSSP, at0=0)
    th4modsel.median <- c(minTP=minTP, tenTP=tenTP, 
                          mxSSSP=mxSSSP, eqSSSP=eqSSSP, at0=0)
  } else {
    if (method == "maxent") {
      if (valCase) {
        fn_resampling_ths <- get_fnameRes(bname, param, paste0("ths_", resampling))
        th4modsel <- doOrReadRDS(maxent_thresh4modsel(dir_thresholds),
                                 fn_resampling_ths, overwrite=overwrite)
        th4modsel.median <- apply(th4modsel[, -1], 2, median)
      } else {
        
      }
    } else {
      th4modsel <- data.frame(Resample=uResampl, "at0"=0)
      th4modsel.median <- c(at0=0)
    }
  }
  results <- NULL
  # Calc for each fold then combine
  for (th in colnames(th4modsel)[-1]) {
    ans <- subset(pred, Resample==uResampl[1])
    th.val <-th4modsel[th4modsel$Resample==uResampl[1], th]
    smmry <- as.data.frame(as.list(puSummary(ans, th=th.val)))
    for (ur in uResampl[-1]) {
      th.val <- as.numeric(th4modsel[th4modsel$Resample==ur, th])
      smmry <- rbind(smmry, 
                     occCompare::puSummary(subset(pred, Resample==ur),
                                           th=th.val))
    }
    results.m <- colMeans(smmry)
    names(results.m) <- paste0(names(results.m), ".", th)
    
    if (!valCase) { # Not in the val (no cv) case
      results.sd <- apply(smmry, 2, sd)
      names(results.sd) <- paste0(names(results.sd), ".", th, ".SD")
      results.th <- c(results.m, results.sd)
    } else {
      results.th <- results.m
    }
    results <- c(results, results.th) 
  }
  
#   ans <- th4modsel.median
#   names(ans) <- paste(names(ans), "th", sep=".")
#   results <- c(results, ans)

  # Combine preds of all folds and calc metric once
  if (!valCase) { # Not in the val (no cv) case
    for (th in names(th4modsel.median)) {
      results.ag <- puSummary(pred, th4modsel.median[th])
      names(results.ag) <- paste0(names(results.ag), ".", th, ".AG")
      results <- c(results, results.ag)
    }
  }
  
  # -------------------------------------------------------
  # threshold independent metrics
  # calc for each fold then combine
  smmry <- NULL
  if (!("pred" %in% colnames(pred)))
    pred$pred <- puFactor(c(rep("un", nrow(pred)-1), "pos"), positive="pos")
  for (ur in uResampl) {
    smmry <- 
      rbind(smmry,
            occCompare:::puSummary_thIndep(subset(pred, Resample==ur),
                                           use_OptimalCutpoints=
                                             use_OptimalCutpoints))
  }
  results.m <- colMeans(smmry)
  # names(results.m) <- paste0(names(results.m), ".", th)
  if (!valCase) { # Not in the val (no cv) case
    results.sd <- apply(smmry, 2, sd)
    names(results.sd) <- paste0(names(results.sd), ".SD")
    results.thIndep <- c(results.m, results.sd)
  } else {
    results.thIndep <- results.m
  }
  
  results <- c(results, results.thIndep) 
  
  # combine preds of all folds and calc metric once
  if (!valCase) { # Not in the val (no cv) case
    results.ag <- 
      occCompare:::puSummary_thIndep(
        pred, use_OptimalCutpoints=use_OptimalCutpoints)
    names(results.ag) <- paste0(names(results.ag), ".AG")
    results <- c(results, results.ag)
  }
  
  results <- data.frame(as.list(results))
  results <- round(results, 5)

  
  # remove stuff
  results <- dplyr::select(results, -starts_with("TPR"))
  results <- dplyr::select(results, -starts_with("PPP"))
  results <- dplyr::select(results, -starts_with("P."))
  results <- dplyr::select(results, -starts_with("PN."))
  results <- dplyr::select(results, -starts_with("FB."))
  if (length(results)==0)
    stop("No more results left...")
  # cbind(unlist(results))
  
  # change the column names according to the following convention:
  # the PU-metric value of a threshold independent approach: 
  #   <PU-metric>
  # (if available) the threshold returned by the threshold independent approach: 
  #   <PU-metric>.th
  # PU-metric estimated at a specific threshold: 
  #   <PU-metric>.<threshold approach>
  # (if available) the threshold of <PU-metric><threshold approach>: 
  #   <PU-metric>.<threshold approach>_T
  nr <- names(results)
  nr <- gsub("FLEE.FLEE", "FLEE", nr)
  nr <- gsub("FLI.FLI", "FLI", nr)
  nr <- gsub("FLEEC.FLEEC", "FLEEC", nr)
  nr <- gsub("FLIC.FLIC", "FLIC", nr)
  nr <- gsub(".th", "_T", nr)
  
  names(results) <- nr
  
  rm(fnames, fnames.suffix); gc()
  return(results)
}