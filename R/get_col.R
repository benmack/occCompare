#' Title
#'
#' @param groups 
#' @param do_unlist 
#'
#' @return
#' @export
get_col <- function(groups, do_unlist=TRUE) {
  gr <- list( # GRoup list
    # "TAsk"
    TA=c('seed','fset','nP','nU','idP'), 
    # TIme
    TI=c('user.self','sys.self','elapsed'), 
    # CLassifer
    CL=c('method'), 
    # PArameter
    PA=list(bsvm=c('sigma', 'cNeg', 'cMultiplier'), 
            maxent=c('fc', 'beta'),
            maxentDef=c('fc', 'beta'),
            ocsvm=c('sigma', 'nu')),
    # Metrics derived from Test data
    TM=c('auc','mxK','mxF1','mxCCR','K_TPR','K_PPP','F1_TPR',
         'F1_PPP','CCR_TPR','CCR_PPP')
  )
  ans <- (groups %in% names(gr))
  if (!all(ans)) {
    valid <- paste0(names(gr), sep=", ")
    invalid <- paste0(groups[!ans], sep=", ")
    stop(cat("Invalid groups:", invalid,  "\nValid:", valid))
  }
  out <- gr[groups]
  if (do_unlist)
    out <- as.character(unlist(unlist(out)))
  
  return(out)
}
