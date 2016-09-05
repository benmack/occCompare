#' Get all tasks (of a method) in a data frame
#'
#' @param method If NULL tasks are given for each method. Else specify the given only the tasks of a method.
#'
#' @return dta frame
#' @export
#'
#' @examples
get_tasks <- function(method=NULL) {
  tasks <- expand.grid(get_settings()[.TASK_PARAMETER])
  if (!is.null(method))
      tasks <- tasks[tasks$method%in%method, , drop=T]
  return(tasks)
}