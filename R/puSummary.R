#'@export
puSummary <- function(data, lev = NULL, model = NULL, th=NULL) {
  
  if (!is.null(th)) {
    data$pred <- "un"
    data$pred[data$pos >= th] <- "pos"
  }
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")

  tp <- sum(data[,"pred"]=="pos" & data[,"obs"]=="pos")
  fn <- sum(data[,"pred"]!="pos" & data[,"obs"]=="pos")
  fpPu <- sum(data[,"pred"]=="pos" & data[,"obs"]!="pos")
  
  tpr <- tp/(tp+fn)
  ppp <- (sum(data[,"pred"]=="pos")/length(data[,"pred"]))
  puP <- tp/(tp+fpPu)
  FLEE <- (tpr^2)/ppp
  FLI <- (2*tp)/(tp+fn+fpPu)
  FB <- 2 * ( (puP*tpr) / (puP+tpr) )
  
  if (is.na(FLEE))
    FLEE[1] <- 0
  if (is.na(FLI))
    FLI[1] <- 0
  if (is.na(FB))
    FB[1] <- 0
  
  FLEE <- FLEE[[1]]
  FLI <- FLI[[1]]
  FB <- FB[[1]]
  
  pn <- min(data[ data[, "obs"]=="un" ,"pos"]) <  0 &
    max(data[ data[, "obs"]=="un" ,"pos"]) >  0
  
  out <- signif(c(TPR=tpr, P=puP, PPP=ppp, FB=FB, FLEE=FLEE, FLI=FLI, PN=pn), 3)
  return(out)
}