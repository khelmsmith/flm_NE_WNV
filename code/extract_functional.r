## open manuscript/figures.Rmd and 'run all' 
## before running these commands. 
# summary(gam.SPEI_test)
# summary(gam.SPEI_1)
# 
# predict(gam.SPEI_1, type="terms")
# 
# grabit <- plot(gam.SPEI_1)
# qplot(grabit[[1]]$x,grabit[[1]]$fit) + geom_rug()
#grabit <- extract_functional(gam.SPEI_1)
extract_functional <- function (x, se = TRUE, select = NULL, scale = -1) 
{
  ## x is a fitted gam() with m >= 1 linear functional terms
  ## se = TRUE says to return the SE estimate as well
  ## select allows restricting which functional term is 
  ## returned; default is all
  ## returns a data.frame with columns term, lag, coef, se 
  m <- length(x$smooth)
  if (is.null(select)){
    select <- 1:m
  } else {
    if (!all(select %in% 1:m)){
      stop("select must be an integer vector less than or equal to the number of smooth terms.")
    }
  }
  if (se) {
    if (is.numeric(se)) 
      se2.mult <- se1.mult <- se
    else {
      se1.mult <- 2
      se2.mult <- 1
    }
    if (se1.mult < 0) 
      se1.mult <- 0
    if (se2.mult < 0) 
      se2.mult <- 0
  }
  else se1.mult <- se2.mult <- 1
  
  if (se && x$Vp[1, 1] < 0) {
    se <- FALSE
    warning("No variance estimates available")
  }
  
  pd <- list()
  i <- 1
  if (length(select) > 0) {
    for (i in select) {
      first <- x$smooth[[i]]$first.para
      last <- x$smooth[[i]]$last.para
      attr(x$smooth[[i]], "coefficients") <- x$coefficients[first:last]
      ## guts of mgcv:::plot.mgcv.smooth start here
      data <- x$model
      xsm <- x$smooth[[i]]
      raw <- data[xsm$term][[1]]
      xx <- min(raw):max(raw) # by always integers
      if (xsm$by != "NA") {
        by <- rep(1, length(xx))
        dat <- data.frame(x = xx, by = by)
        names(dat) <- c(xsm$term, xsm$by)
      }
      else {
        stop(paste(xsm$term,"not a linear functional."))
      }
      X <- mgcv:::PredictMat(xsm, dat)
      label <- paste(xsm$term,xsm$by,sep=":")
      # return() # end of mgcv:::plot.mgcv.smooth
      P <- list(label = label, X = X, x = xx)
      # mgcv:::plot.mgcv.smooth(x$smooth[[i]], P = NULL, data = x$model, 
      #              se = se, se1.mult = se1.mult, 
      #              se2.mult = se2.mult)
      p <- x$coefficients[first:last]
      offset <- attr(P$X, "offset")
      if (is.null(offset)) 
        P$fit <- P$X %*% p
      else P$fit <- P$X %*% p + offset
      if (se) {
        se.fit <- sqrt(pmax(0, rowSums((P$X %*% x$Vp[first:last, first:last, drop = FALSE]) * P$X)))
        P$se <- se.fit * se1.mult
      }
      P$X <- NULL
      pd[[i]] <- P
      rm(P)
    }
  }
  purrr::map_df(pd, as.data.frame, stringsAsFactors=FALSE)
}
