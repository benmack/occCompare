# x <- task
# x <- as.list(task)
# x$seed = c(1,2)

#'@export
task2str <- function(x) {
  if (any(sapply(x, length)>1))
    stop("Task must contain single values.")
  paste(.TASK_PARAMETER, x[.TASK_PARAMETER], sep="-", collapse="_")
}

#'@export
str2task <- function(x) {
  dummy <- strsplit(x, "_")[[1]]
  dummy <- strsplit(dummy, "-")
  nms <- sapply(dummy, function(x) x[1])
  vals <- sapply(dummy, function(x) x[2])
  task <- as.data.frame(as.list(vals), stringsAsFactors=F)
  colnames(task) <- nms
  for (i in 1:length(task)) {
    dummy <- try(as.numeric(task[i]))
    if (!is.na(dummy))
      task[i] <- dummy
  }
  task
}
