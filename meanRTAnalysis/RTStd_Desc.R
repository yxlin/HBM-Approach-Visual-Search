#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        18 October, 2013
# Description: Print standard errors
rm(list=ls())
load('./data/myData/avgFC.RData')
load('./data/myData/avgS.RData')
source("./functions/summarise.R")
source("./functions/multiplot.R")
library(ggplot2); library(grid)

## --------------------------------
# Calculating average RT and error rate
avgRT1 <- summarySEwithin(avgFC, measurevar="mean", 
                          withinvars=c("task", "size", "target"),
                          idvar="subj")
avgRT2 <- summarySEwithin(avgS, measurevar="mean", 
                          withinvars=c("size", "target"),
                          betweenvars="task",
                          idvar="subj")
avgErr1 <- summarySEwithin(avgFC, measurevar="errRate", 
                           withinvars=c("task", "size", "target"),
                           idvar="subj")
avgErr2 <- summarySEwithin(avgS, measurevar="errRate", 
                           withinvars=c("size", "target"),
                           betweenvars="task",
                           idvar="subj")

featP <- subset(avgRT1, task == "F" & target == "P")
conjP <- subset(avgRT1, task == "C" & target == "P")
spatP <- subset(avgRT2, task == "S" & target == "P")

descMeanRT <- rbind(featP, conjP, spatP)
(avgAcrossSize <- ddply(descMeanRT, .(task), summarise,
                        meanSE = mean(se)))

(avgAcrossTask <- ddply(descMeanRT, .(task, size), summarise,
                        meanSE = mean(se)))
