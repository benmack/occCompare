#'@export
resampl_param_from_str <- function(x) {
  # understands: 'cvX', 'cvX_pu', with x = number of folds
  splt = strsplit(x, "_")[[1]]
  if (splt[1]=="val") {
    n <- NA
  } else {
    n = as.numeric(gsub("cv", "", splt[1]))
  }
  pu = length(splt)==2
  return(list(n=n, pu=pu))
}
