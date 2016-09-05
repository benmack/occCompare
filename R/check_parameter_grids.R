#' Title
#'
#' @param method 
#' @param x 
#' @param y 
#' @param facet_x 
#' @param res 
#' @param tasks 
#' @param grid_boder_cases_only 
#' @param optcrit 
#' @param ignore 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
check_parameter_grids <- function(method, res=NULL, tasks=NULL,
                                  optcrit="mxK",
                                  ignore=NULL) {
  
  if (is.null(tasks))
    tasks <- get_tasks(method=method)
  
  tuneparams <- names(get_settings()$param[[method]])

  # data frame used for plotting
  if (is.null(res))
    res <- load_results(c(".trainUtest", ".resampling"), verbosity=1, overwrite=FALSE)
  
  rdf <- res[[method]]

  for (tp in tuneparams)
    if (is.factor(rdf[[tp]]))
      if (!tp %in% ignore) {
        ans = list(c("lower", "upper"))
        names(ans) <- tp
        ignore = c(ignore, ans)
        cat(tp, " (factor) added to ignore.\n")
      }

  
  # remove rows that are not in tasks
  for (tk in names(tasks))
    rdf <- rdf[rdf[[tk]] %in% unique(tasks[[tk]]), ]
  
  params_at_border <- NULL
  
  # grid_boder_cases_only 
    nrow_all <- nrow(tasks)  
    keep <- logical(nrow(tasks))
    for (i in 1:nrow(tasks)) {
      params_at_border_i <- as.data.frame(matrix(NA, 0, length(tuneparams)))
      names(params_at_border_i) <- tuneparams
      task <- tasks[i, ]
      taskstr <- sprintf("seed:%d - fset:%s - idP:%d",
                         task$seed, task$fset, task$idP)
      #get_params_if_best_at_gridborder
      tmp = filter(rdf, 
                   seed==task$seed, 
                   fset==task$fset,
                   nP==task$nP,
                   nU==task$nU,
                   idP==task$idP)
      ans <- get_params_if_best_at_gridborder(tmp, params=tuneparams, 
                                              optcrit=optcrit,
                                              ignore=ignore)
    for (j in seq_along(ans))
      if (is.null(ans[[j]]))
        ans[[j]] <- NA 
    params_at_border <- rbind(params_at_border, 
                              as.data.frame(ans))

  }
  return(params_at_border)
}
