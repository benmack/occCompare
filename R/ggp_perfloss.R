#' Title
#'
#' @param x 
#' @param ignore_methods 
#' @param factorVals4plot 
#' @param cols4plot 
#' @param box_lwd 
#' @param box_outlier_size 
#' @param axis.text.size 
#' @param strip.text.size 
#' @param rotate_axis.title.y 
#' @param panel.margin.y 
#' @param panel.margin.x 
#'
#' @return
#' @export
#'
#' @examples
ggp_perfloss <- function(x,
                         ignore_methods="star",
                         factorVals4plot=get_fVals4plot(),
                         cols4plot=get_cols4plot(),
                         box_lwd=.2, 
                         box_outlier_size=.2,
                         axis.text.size=10,
                         strip.text.size=10,
                         rotate_axis.title.y=TRUE,
                         panel.margin.y=0,
                         panel.margin.x=0,
                         hlines=c(0,0.1)
) {
  # PREPARE DATA FRAME
  # ignore
  x <- x[!x$method%in%ignore_methods, ]
  # rename and reorder according
  msgA = "'%s': Levels will be removed:\n  %s\n"
  msgB = "'%s': Levels required for plot not found in data:\n  %s\n"
  for (var in names(factorVals4plot)) {
    in_data_not_4plot <- 
      !unique(x[, var]) %in% names(factorVals4plot[[var]])
    in_data_not_4plot <- unique(x[, var])[in_data_not_4plot]
    if (length(in_data_not_4plot)>0)
      cat(sprintf(msgA, var, paste(in_data_not_4plot, collapse=",")))
    
    req4plot_not_in_data <- 
      !names(factorVals4plot[[var]]) %in% unique(x[, var])
    req4plot_not_in_data <- names(factorVals4plot[[var]])[req4plot_not_in_data]
    if (length(req4plot_not_in_data)>0)
      cat(sprintf(msgB, var, paste(req4plot_not_in_data, collapse=",")))
    
    x[, var] <- revalue(x[, var], factorVals4plot[[var]])
    x[, var] <- factor(x[, var],
                       levels=factorVals4plot[[var]], 
                       ordered = TRUE)
  }
  
  # GGPLOT2 PARMS AND THEME
  if (rotate_axis.title.y) {
    axis.text.y <- element_text(angle = 90, vjust=0.5,
                                hjust = .5, size=axis.text.size)
  } else {
    axis.text.y <- element_text(size=axis.text.size)
  }
  myTheme <-
    theme_bw() + 
    theme(# plot.title=element_text(titles[fset]),
      axis.text.x=element_text(angle = 90, vjust=0.5,
                               hjust = 1, size=axis.text.size),
      axis.text.y = axis.text.y,
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      strip.text = element_text(lineheight=5),
      strip.text.x = element_text(size=strip.text.size, angle=0),
      strip.text.y = element_text(size=strip.text.size, angle=90),
      panel.margin.y = unit(panel.margin.y, "lines"),
      panel.margin.x = unit(panel.margin.x, "lines"),
      # strip.switch.pad.grid = ,
      legend.position="none",
      panel.margin=unit(0.05, "lines")) 
  names(cols4plot$ts) <- levels(x$ts)
  # PLOT
  gg <- ggplot(x, aes(x=ts, y=PL, color=ts)) 
  if (!is.null(hlines))
    gg <- gg + geom_hline(yintercept=hlines)
    gg <- gg + geom_boxplot(lwd=box_lwd, 
                 outlier.size = box_outlier_size) +
    facet_grid(. ~ method+ps,
               scale="free_x", space="free") + 
    myTheme + 
    scale_color_manual(breaks = levels(x$ts),
                       values=cols4plot$ts)
  return(gg)
}