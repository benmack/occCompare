#' Get settings required for using package occCompare  
#' 
#' @description It is assumed that the settings restored as
#' option \code{occc_sttings}, thus callable via 
#' \code{getOption("occc_settings")}. 
#'
#' @return
#' @export
get_settings <- function() {
  
  occc_settings <- c("bdir", "seed", "fset", "nP", "nU", "idP", 
                     "method", "param", "resampling", "scaling",
                     "dn.res", "dn.ressum", "get_fn_ressum", 
                     "dset", "get_refset", "args.rs")
  
  settings <- getOption("occc_settings")
  
  miss <- c()
  for (os in occc_settings) {
    if (is.null(settings[[os]]))
      miss <- c(miss, os)
  }
  if (length(miss > 0)) {
    str <- paste0(paste0(
      "The settings listed below are missing.\n",
      "They need to be found via \n", 
      "   getOption('occc_settings')[[<setting>]].\n",
      "Missing settings:\n   - "), 
      paste0(miss, collapse="\n   - "), "\n")
    cat(str)
    stop("See above for missing settings.")
  }
  # Add checks here, ...
  
  # Keep only occc_settings
  settings <- settings[occc_settings]
  
  settings <- structure(settings,
                        class="occc_settings")
  settings
}
