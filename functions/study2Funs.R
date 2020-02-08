errorRate <- function(x) {
  #----------------------------------------------------------------------
  #     *** This function returns three data frame in a list***
  #----------------------------------------------------------------------
  # (1) errDf. The error rate across the subjects and conditons
  # (2) errCond. The error rate shows in each condition across the subjs.
  # (3) errPart. The error rate shows in each subj across the conditions.
  #----------------------------------------------------------------------
  require(plyr)
  
  if( length(unique(x$keyStatus)) != 2) warning('observing too slow or too quick! Check it out')
  
  #--- Calculate error rate and store names across conditions
  cellSizeDf <- ddply(x, .(targetPos, setSize, fVTgt, subj), "nrow", .drop=F)
  cellSize <- subset(cellSizeDf, select = nrow)
  
  tAll <- ddply(x, .(keyStatus, targetPos, setSize, fVTgt, subj), "nrow", .drop=F)
  t1 <- subset(tAll, keyStatus == 1, select = nrow) # correct trials
  t2 <- subset(tAll, keyStatus == 2, select = nrow) # incorrect trials
  
  conditionName <- subset(tAll, keyStatus == 1, select = 2:5) 
  
  nCorIncorRes <- t1+t2
  
  #--- errDf is the error rate across participants and conditions
  errDf <- cbind(conditionName, 100*(t1/nCorIncorRes), 100*(t2/nCorIncorRes))
  colnames(errDf) <- c('targetPos', 'setSize', 'fVTgt', 'subj', 'accuracy', 'error')
  
  #--- Error rate for each condition average across the participants
  errCond <- ddply(errDf, .(targetPos, setSize, fVTgt), numcolwise(mean), na.rm=T)
  errCond <- subset(errCond, select=c(1,2,3,5,6))
  #--- Error rate for each the participants across the conditions
  errPart <- ddply(errDf, .(subj), numcolwise(mean), na.rm=T)
  
  errDf <- errDf[!is.nan(errDf$accuracy),]                # trim off the NaN rows
  errCond <- errCond[!is.nan(errCond$accuracy),]          # trim off the NaN rows
  errPart <- errPart[!is.nan(errPart$accuracy),]          # trim off the NaN rows
  
  return(list(errDf, errCond, errPart))
}


## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This is does the summary; it's not easy to understand...
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean"=measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  require(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "Normed", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL, idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- sapply(data[, c(betweenvars, withinvars), drop=FALSE], FUN=is.factor)
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Norm each subject's data    
  data <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measureNormedVar <- paste(measurevar, "Normed", sep="")
  
  # Replace the original data column with the normed one
  data[,measurevar] <- data[,measureNormedVar]
  
  # Collapse the normed data - now we can treat between and within vars the same
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars), na.rm=na.rm,
                     conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  # Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(sapply(datac[,withinvars, drop=FALSE], FUN=nlevels))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  datac$sd <- datac$sd * correctionFactor
  datac$se <- datac$se * correctionFactor
  datac$ci <- datac$ci * correctionFactor
  
  return(datac)
}

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}


rm.anova <- function(dat=NULL, measurevar, idvar, withinvars=NULL, betweenvars=NULL ) {
  library(car); library(reshape2)
  
  #----------------------------------------------------------
  # Ensure that the betweenvars and withinvars are factors
  #----------------------------------------------------------
  factorvars <- sapply(dat[, c(withinvars), drop=FALSE], FUN=is.factor)
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    dat[nonfactorvars] <- lapply(dat[nonfactorvars], factor)
  }
  
  tmp <- as.formula( paste(idvar, "~", paste(withinvars, collapse="+")))
  datWide <- dcast(dat, tmp, value.var = measurevar)
  
  #--
  nameWithin <- names(datWide)[-1]  # take off subj column
  numberOfWithinFactor <- length(withinvars)
  
  # Each row represents one combination of conditions; each column is one factor
  wf <- matrix( numeric(numberOfWithinFactor*length(nameWithin)), ncol=numberOfWithinFactor)
  
  #-- construct idata
  for(i in 1:length(nameWithin))  {
    t1 <- unlist( strsplit(nameWithin[i], "_", fixed=TRUE) )
    for (j in 1:length(t1)) {
      wf[i,j] <-  t1[j]  
    }
  }
  
  idata <- data.frame(wf);  colnames(idata) <- withinvars
  
  #-- construct idesign
  if( length(names(idata)) == 1) {
    ides <- as.formula( paste("~", names(idata) ) )
  } else {
    ides <- as.formula( paste("~", paste(names(idata), collapse="*")))
  }
  
  #-- extract names of between factor;
  # this part has not been fully tested.
  if( is.null(betweenvars) ) {
    nameBetween <- 1
  } else {
    #nameBetween <- names(datWide)[c(2,3)]
  }
  
  y <- as.matrix(datWide[,2:ncol(datWide)])
  fittedFormula <- as.formula(paste('y ~ ', paste(nameBetween, collapse='*') ) )
  
  #-- print rm anova summary
  mod <- lm(  fittedFormula, data=dat)
  av.mod <- Anova(mod, idata=idata, idesign=ides)
  return(av.mod)
}

# From print.Anova.mlm
xtable.Anova.mlm <- function (x, lab='Label', cap='Caption') {
  require(xtable)
  test <- x$test
  repeated <- x$repeated
  ntests <- length(x$terms)
  tests <- matrix(NA, ntests, 4)
  if (!repeated)
    SSPE.qr <- qr(x$SSPE)
  for (term in 1:ntests) {
    eigs <- Re(eigen(qr.coef(if (repeated) qr(x$SSPE[[term]]) else
      SSPE.qr,
                             x$SSP[[term]]), symmetric = FALSE)$values)
    tests[term, 1:4] <- switch(test, Pillai = stats:::Pillai(eigs,
                                                             x$df[term], x$error.df), Wilks = stats:::Wilks(eigs,
                                                                                                            x$df[term], x$error.df), `Hotelling-Lawley` = stats:::HL(eigs,
                                                                                                                                                                     x$df[term], x$error.df), Roy = stats:::Roy(eigs,
                                                                                                                                                                                                                x$df[term], x$error.df))
  }
  ok <- tests[, 2] >= 0 & tests[, 3] > 0 & tests[, 4] > 0
  ok <- !is.na(ok) & ok
  tests <- cbind(x$df, tests, pf(tests[ok, 2], tests[ok, 3],
                                 tests[ok, 4], lower.tail = FALSE))
  rownames(tests) <- x$terms
  colnames(tests) <- c("Df", "test stat", "approx F", "num Df",
                       "den Df", "Pr(>F)")
  tests <- structure(as.data.frame(tests[-1,]), heading = paste("\nType ",
                                                                x$type, if (repeated)
                                                                  " Repeated Measures", " MANOVA Tests: ", test, " test
							statistic",
					sep = ""), class = c("anova", "data.frame"))
  # print(tests)
  # invisible(x)
  xtable(tests, label=lab, caption=cap)
}

