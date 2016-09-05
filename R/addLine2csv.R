#' Add a result line to the csv file 
#'
#' @param line the line to add
#' @param file the file to add to
#'
#' @return
addLine2csv <- function(line, file) {
  if (!file.exists(file)) {
    if (!file.exists(dirname(file)))
      dir.create(dirname(file), recursive=T)
    write.csv(line, file=file, quote=F, row.names=F)
  } else {
    cnames <- colnames(read.csv(file=file, header=TRUE, nrows=1))
    if (all(colnames(line) %in% cnames)) {
      add = paste0(paste0(line[, cnames], collapse=","), "\n")
      cat(add, file=file, append=TRUE)
    } else {
      stop("Columnnames of new line do not match the table.")
    }
  }
}
