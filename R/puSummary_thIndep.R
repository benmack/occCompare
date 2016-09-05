#'@export
puSummary_thIndep <- function(data, lev = NULL, model = NULL, th=NULL, 
                              thresholds=NULL, use_OptimalCutpoints=TRUE
                              ) {
  
  # What is that for ??? >>>
  if (!is.null(th)) {
    data$pred <- "un"
    data$pred[data$pos >= th] <- "pos"
  }
  # <<<
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  
  out <- NULL
  if (use_OptimalCutpoints) {
    # ---
    # check if the package exists
    ans <- try(OptimalCutpoints::optimal.cutpoints)
    if (class(ans)=="try-error")
      stop("Package OptimalCutpoints is not installed.")
    oc_methods <- c(
      # Criteria based on sensitivity and specificity measures
      "SpEqualSe", "MaxSpSe", "Youden", "MaxProdSpSe", "Minimax",
      "ROC01", "MaxDOR", "MaxEfficiency", "MaxKappa",
      # Criteria based on predictive values
      "NPVEqualPPV", 
      # "MaxNPVPPV", 
      "MaxSumNPVPPV", "MaxProdNPVPPV", 
      "PROC01",
      # Criteria based on diagnostic likelihood ratios
      # - 
      # Criteria based on cost-benefit analysis of the diagnosis
      "CB", "MCT",
      # Maximum 2 or minimum p value criterion
      "MinPvalue"
      # Prevalence-based methods
      # - 
    )
    
    ocp = try(
      OptimalCutpoints::optimal.cutpoints(
        X=pos~obs, data=data,tag.healthy="pos", direction=">", 
        methods=oc_methods) 
    )
    
    if (class(ocp) == "try-error") {
      nms <- c(oc_methods, "AUC", paste0(oc_methods, ".th"))
      out <- rep(NA, length(nms))
      names(out) <- nms
    } else {
      optcrit = sapply(oc_methods, function(m) ocp[[m]][[1]]$optimal.criterion)
      names(optcrit) <- oc_methods
      optcrit["AUC"]=ocp$Youden[[1]]$measures.acc$AUC[1]
      optth = sapply(oc_methods, function(m) ocp[[m]][[1]]$optimal.cutoff[[1]][1])
      names(optth) <- paste0(oc_methods, ".th")
      out <- c(optcrit, optth)
    }
  } 
  
  # Get the tresholds for puSummaryThLoop
  # similar if not the same thresholds would also be created 
  # in the function... 
#   if (is.null(thresholds)) {
#     # thresholds <- generateThresholds(data, q_rng=c(.5, .8), nTh=100)
#   }
  # The threshold is maximized over the full threshold range 
  # and a constraint range 
  # First get the full range
  thresholds <- seq(min(data$pos), max(data$pos), length.out=100)
  maxPu_full <- puSummaryThLoop(data=data, thresholds=thresholds,
                                calcAUC=!use_OptimalCutpoints)
  thresholds <- generateThresholds(data, q_rng=c(.5, .8), nTh=100)
  maxPu_const <- puSummaryThLoop(data=data, thresholds=thresholds,
                                calcAUC=!use_OptimalCutpoints) 
  # !use_OptimalCutpoints is there since AUC is calculated also there 
  
  LeeAndLi <- c(.format_LeeAndLi(maxPu_full),
                .format_LeeAndLi(maxPu_const, "C"))
  if ("AUC" %in% names(maxPu_full))
    LeeAndLi <- c(LeeAndLi, AUC=as.numeric(maxPu_full["AUC"]))
#   # see wwhat happens
#   boxplot(list(P=data$pos[data$obs=="pos"], 
#                U=data$pos[data$obs=="un"])) # , ylim=range(thresholds))
#   abline(h=min(thresholds))
#   abline(h=max(thresholds))
    
#   # >>>
#   # EVENTUALLY CALCULATE METRICS ONCE AND THEN CONSTRAIN ON THE RESULTS
#   # FASTER...
#   #, returnAll=T)
#   thDep <- thDep[, c("th", "puF", "puF1")]
#   max_fullRng <- maximize_puMetric(thDep, c("puF", "puF1"))
#   # remove the value of the one metric at which the other one is maximized
#   max_fullRng <- 
#     max_fullRng[, !colnames(max_fullRng)%in%c("puF.puF1", "puF1.puF")]
#   # Get the approximate thresholds of the constraint   
#   # ...
#   # <<<

  out <- c(LeeAndLi, out)
  
  return(out)
}

.format_LeeAndLi <- function(x, suffix="") {
  sel <- c("puF.puF", "puF1.puF1", "puF.th", "puF1.th")
  LeeAndLi <- as.numeric(x[sel])
  sel <- gsub("puF1", paste0("FLI", suffix), sel)
  sel <- gsub("puF", paste0("FLEE", suffix), sel)
  names(LeeAndLi) <- sel
  LeeAndLi}