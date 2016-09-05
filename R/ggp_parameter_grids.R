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
ggp_parameter_grids <- function(method, x, y, facet_x=NULL,
                                res=NULL, tasks=NULL,
                                grid_boder_cases_only=TRUE,
                                optcrit="mxK",
                                ignore=NULL,
                                print_single_grids=FALSE, 
                                verbose=FALSE) {
  
  if (is.null(tasks))
    tasks <- get_tasks(method=method)
  
  tuneparams <- names(get_settings()$param[[method]])
  
  # data frame used for plotting
  if (is.null(res))
    res <- load_results(c(".trainUtest", ".resampling"), verbosity=1, overwrite=FALSE)
  rdf <- res[[method]]
  # remove rows that are not in tasks
  for (tk in names(tasks))
    rdf <- rdf[rdf[[tk]] %in% unique(tasks[[tk]]), ]
  
  # plot grid_boder_cases_only 
  if (grid_boder_cases_only) {
    nrow_all <- nrow(tasks)  
    keep <- logical(nrow(tasks))
    for (i in 1:nrow(tasks)) {
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
      keep[i] <- !all(sapply(ans, is.null))
      # which.max(tmp[, optcrit][[1]])
      
      if (verbose) {
        l4p <- unlist(ans)
        prnt <- paste0(# "keep =", keep[i], "\t",
            taskstr, " >>> ",
            paste(names(l4p), unlist(l4p), sep="="), "\n")
        
        if (print_single_grids & keep[i]) {
          for (tunecol in tuneparams)
            tmp[, tunecol] <- as.factor(tmp[, tunecol][[1]])
          
          ggp <- ggplot(tmp, aes_string(x, y)) +
            geom_tile(aes(fill=mxK)) + 
            scale_fill_gradient2(low="red", high="blue", mid="white", 
                                 midpoint=.5, limit=c(0,1)) +
            ggtitle(prnt) + 
            theme(
              axis.text.x=element_text(angle = 90, vjust=0.5, hjust = 1),
              plot.title = element_text(size=12, face="bold", vjust=1, 
                                        lineheight=0.6))
          if (!is.null(facet_x))
            ggp <- ggp + facet_grid(as.formula(paste(". ~ ", facet_x)))
          print(ggp)
        } else if (!print_single_grids  & keep[i]) {
          cat(prnt)
        }
      }
    }
    tasks <- tasks[keep, ]
    cat(sprintf("%d of %d tasks with optimum at the parameter grid border.\n", 
                nrow(tasks), nrow_all))
  }
  
  facet_grid_y <- c()
  for (tk in names(tasks))
    if (length(unique(rdf[[tk]]))>1)
      if (tk != "method")
        facet_grid_y <- c(facet_grid_y, tk)
  cat("'facet_grid' formula: ", paste(paste0(facet_grid_y, collapse="+"), "~ ."))
  
  # convert to factor such that geom_tile turns out pretty
  for (tunecol in tuneparams)
    rdf[, tunecol] <- as.factor(rdf[, tunecol][[1]])
  
  df = rdf[, c(.TASK_PARAMETER[-6], tuneparams, optcrit)]
  
  if (is.null(facet_x))
    fg <- facet_grid(as.formula(paste(paste0(facet_grid_y, collapse="+"), " ~ .")))
  else
    fg <- facet_grid(as.formula(paste(paste0(facet_grid_y, collapse="+"), " ~ ", facet_x)))
  
  ggp <- ggplot(df, aes_string(x, y)) +
    geom_tile(aes(fill=mxK)) + 
    fg + 
    scale_fill_gradient2(low="red", high="blue", mid="white", 
                         midpoint=.7, limit=c(0.4,1)) +
    theme(
      axis.text.x=element_text(angle = 90, vjust=0.5, hjust = 1),
      plot.title = element_text(size=20, face="bold", vjust=1, lineheight=0.6))
  return(ggp)
}
