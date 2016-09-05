#' Title
#'
#' @param x list with the results returned by \code{load_results}
#' @param ms data frame with the columns 
#' \code{ps}, \code{ts} and \code{cl}.
#' @param acc_metric Defaults: mxK. Accuracy metric.  
#' 
#' @return
#' @export
#'
#' @examples
model_selection <- function(x, ms=NULL, acc_metric="mxK", 
                            sort_ps_first=TRUE,  verbose=FALSE,
                            file=NULL, overwrite=FALSE) {
  

  if (is.null(file))
    file = get_settings()$get_fn_ressum("modsel")
  
  if (acc_metric == "mxK")
    acc_metric_noMX <- "k"
  else
    stop("acc_metric not supported yet...")
  if (file.exists(file))
    cat("Loading results from:", file, "\n")
  out <- doOrReadRDS({
    
    x.pu <- x[!names(x) %in% "binsvm"]
    
    # best achievable accuracy over all methods
    bapK <- .model_selection(x, ps=acc_metric, ts=acc_metric,
                             maxOverCl=TRUE)
    bapK$method <- "star"
    # best achievable accuracy over all methods given a classifier
    bapK <- rbind(bapK, .model_selection(x, ps=acc_metric, ts=acc_metric))
    
    # the realized accuracy of a particular model selection approach 
    out <- bapK[, c("method", get_col("TA"), 
                    paste(acc_metric, acc_metric, sep="."))]
    out$K <- out$mxK.mxK
    out$ms=out$method
    out$ms <- paste(acc_metric_noMX, acc_metric_noMX, sep=".")
    
    for (i in 1:nrow(ms)) {
      cat(i, "/", nrow(ms), ":", 
          paste(ms$ps[i], ms$ts[i], sep="-"), "\n")
      out.i <- .model_selection(x.pu, ps=ms$ps[i], ts=ms$ts[i],
                                cl=ms$cl[i])
      out <- rbind(out, out.i)
    }
    
    
    out.ms <- out$ms
    out <- dplyr::select(out, -ms)
    out <- cbind(out, psUts_from_ms(out.ms)) 
    
    # Introducing maxentDef as a pseudo classifier:
    out$ps[out$method=="maxentDef"] <- "DEF"
    
    # The following is done to have the right order in the plot
    # I believe...
    # Also k and DEF are added such that Missing factors is 
    # working fine.
    # First add the new levels to ms
    ms$ps <-  factor(ms$ps, levels=c("k", "DEF", levels(ms$ps)))
    ms$ts <- factor(ms$ts, levels=c("k", levels(ms$ts)))
    
    ms <- rbind(ms[1, ], ms)
    ms[1, ] <- c("k", "k", NA)
    ans <- ms[is.na(ms$cl), ]
    ans[, "ps"] <- "DEF"
    ms <- rbind(ms, ans)
    out <- unique(out) # ???
    out$ms <- paste(out$ps, out$ts, sep=".")
    idx_fill <- is.na(out$ms)
    out$ms[idx_fill] <- paste(out$ps, out$ts, sep=".") 
    
    if (sort_ps_first) {
      ans <- expand.grid(ts=levels(ms$ts), ps=levels(ms$ps)) 
    } else {
      ans <- expand.grid(ps=levels(ms$ps), ts=levels(ms$ts)) 
    }
    ms.levels <- paste(ans$ps, ans$ts, sep=".")
    ms.levels <- c(ms.levels, "k.k", "DEF.k")
    miss <- unique(out$ms[!out$ms %in% ms.levels])
    if (length(miss)>0) {
      stop(cat("Missing factors:\n", 
               paste0(miss, collapse=", "), "\n\n"))
    }
    out$ms=factor(out$ms, levels=unique(ms.levels))
    out}, file=file, overwrite=overwrite)
  print(unique(out[, c("method", "ps", "ts", "ms")]))
  return(out)
}

#' Title
#'
#' @param ms 
#'
#' @return
#' @export
#'
#' @examples
psUts_from_ms <- function(ms) {
  ps=sapply(strsplit(as.character(ms), "[.]"), function(x) x[1])
  
  ts=sapply(strsplit(as.character(ms), "[.]"), function(x) x[2])
  
  data.frame(ps=ps, ts=ts, stringsAsFactors = F)
}


#' Title
#'
#' @param x 
#' 
#' @param x list with the results returned by \code{load_results}
#' @param ps model selection
#' @param ts threshold selection
#' @param cl Classifiers as character vectors, i.e. one or more of
#'  \code{names(x)} or 'star', i.e. all classifiers. 
#'  If more than one classifier is selected, and \code{maxOverCl=TRUE}
#'  the maximization is done over the classifiers.
#' @param doUnlist combine the results of the different classifers 
#'  in one data frame. In this case the parameter columns get lost. 
#' @param maxOverCl 
#' @param keepCols The columns to be returned 
#' (added to cols). 
#'
#' @return
#' @export
.model_selection <- function(x, ps="mxK", ts="mxK", cl=NULL, 
                             doUnlist=TRUE, maxOverCl=FALSE,
                             keepCols=c("mxK.mxK", "K")) {
  parcc <- get_settings()
  
  keepCols <- c(get_col("CL"), get_col("TA"), keepCols)
  
  if (is.null(cl))
    cl <- names(x)
  if (is.na(cl)[1])
    cl <- names(x)
  if (length(cl)==1) {
    cl <- strsplit(as.character(cl), ";")[[1]]
  }
  # format such that the selection can be performed 
  # no matter waht is selected.
  # - ...
  
  # When AUC is parameter selection the threshold is irrelevant
  if (ps=="AUC")
    MS <- paste(ps, ps, sep=".")
  else
    MS <- paste(ps, ts, sep=".")
  
  sel <- list()
  for (method in cl) {
    x.cl <- x[[method]]
    cnames <- colnames(x.cl)
    ignore.always <- which(cnames %in% get_col("TI"))
    
    ncols <- ncol(x.cl)
    mcols <-  which(cnames=="method")
    tcols <-  which(cnames %in% get_col("TA"))
    pcols <-  which(cnames %in% get_col("PA"))
    
    # - add threshold information where it is lacking
    # currently all names without "." and "_" 
    # TODO: DOCUMENT THIS RULE !!!
    ignore <- c(mcols, tcols, pcols, 
                grep("[.]", cnames), 
                grep("_", cnames),
                ignore.always)
    idx.rename <- setdiff(1:ncols, ignore)
    colnames(x.cl)[idx.rename] <- 
      paste(cnames[idx.rename], cnames[idx.rename], sep=".")
    
    x.cl.sel <- x.cl %>% arrange_(MS) %>% slice(n()) %>% 
      ungroup() %>% arrange_(.dots=get_col("TA"))
    
    sel[[method]] <- x.cl.sel
    rm(x.cl, x.cl.sel)
  }
  
  MS <- paste(ps, ts, sep=".")
  
  # Here code for accuracy at AT
  # ...
  if (ts!="mxK") {
    for (method in cl) {
      sel.cl <- sel[[method]]
      cnames <- colnames(sel.cl)
      pcols <-  which(cnames %in% get_col("PA"))
      
      sel.cl$K <- NA
      sel.cl$F1 <- NA
      sel.cl$CCR <- NA
      
      for (i in 1:nrow(sel.cl)) {
        sel.cl.i <- sel.cl[i, ]
        bname <- get_fnameResBase(
          parcc$dn.res, cbind(sel.cl.i[, get_col("TA")],
                              method=ifelse(method=="maxentDef", 
                                            "maxent", method), 
                              scaling=parcc$scaling))
        fname.ev <- get_fnameRes(bname, sel.cl.i[pcols], "ev")
        ev <- readRDS(fname.ev)
        
        th.name <- paste0(ts, "_T")
        if (ts=="at0") {
          th <- 0 
        } else if (th.name %in% cnames) {
          th <- as.numeric(sel.cl.i[1, th.name])
        } else {
          stop(cat("Cannot identify threshold: ", th.name, "\n"))
        }
        
        idx.th <- which(abs(ev@t-th)==min(abs(ev@t-th), na.rm=T))[1]
        
        sel.cl$F1[i] <- calc_F1(precision=ev@PPP, recall=ev@TPR)[idx.th]
        sel.cl$K[i] <- ev@kappa[idx.th]
        sel.cl$CCR[i] <- ev@CCR[idx.th]
      }
      sel[[method]] <- sel.cl
    }
  }
  
  for (method in cl) {
    if (doUnlist | maxOverCl) {
      cnames <- colnames(sel[[method]])
      pcols <-  which(cnames %in% get_col("PA"))
      sel[[method]] <- sel[[method]][, -pcols]
    }}
  
  if(doUnlist | maxOverCl) {
    # all_cnames <- unique(unlist(lapply(sel, names)))
    tmp <- sel
    sel <- sel[[1]]
    for (ans in tmp[-1]) {
      sel <- combineDf(ans, sel, addNewRows=TRUE, verbose=FALSE)
    }
    sel <- # do.call(rbind, sel) %>% 
      group_by_(sel, .dots=character2symbol(get_col("TA")))
    if (maxOverCl)
      sel <- 
      arrange_(sel, MS) %>% slice(n()) %>% ungroup() %>%
      arrange_(.dots=get_col("TA"))
    keepCols <- keepCols[keepCols %in% colnames(sel)]
    sel <- dplyr::select_(sel, .dots=keepCols)
    # sel <- cbind(ms=paste0(sel$method, ".", MS), sel)
    sel <- cbind(ms=MS, sel)
  }
  
  return(sel)
}
