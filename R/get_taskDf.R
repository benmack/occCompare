#'@export
get_taskDf = function(parlist, as.string=FALSE){
  df = as.data.frame(parlist[.TASK_PARAMETER], stringsAsFactors=F)
  if (as.string) {
    paste0(.TASK_PARAMETER, df, collapse="_")
  } else {
  df
  }
}
