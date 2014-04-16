# Disclaimer-------------------------------------------------------
# Author:      Yishin Lin
# Date:        30 March, 2014
# Description: Calculate ANOVA and eta square for miss errors and 
# false alarm
rm(list=ls())
library(plyr); library(reshape2); library(car)

# Load data and functions -------------------------------------------------
load('./data/myData/avgFC.RData')
load('./data/myData/avgS.RData')
source("./functions/study2Funs.r")
source("./functions/eta2Size.R")
source("./functions/eta2.R")

# Organise the data frame -------------------------------------------------
avg1 <- rbind(avgFC, avgS)
avg1$task <- factor(avg1$task, levels=c('F','C','S'),
                    labels=c('F','C','S'))

errMiss <- subset(avg1,  target == "P")
errFA <- subset(avg1,  target == "A")

tmp1 <- ddply(errMiss, .(size,task), summarise,
      miss = mean(miss))
tmp2 <- ddply(errFA, .(size,task), summarise,
      fa = mean(fa))

tmp3 <- cbind(tmp1[,c(1:2)], tmp1$miss, tmp2$fa, tmp1$miss/tmp2$fa)
names(tmp3) <- c("size", "task", "miss", "fa", "ratio")
mean(tmp3$ratio) # show an overall value 

## 2-way (display size and task) at miss errors and false alarm--------
fitMiss <- rm.anova(errMiss, measurevar='miss', idvar='subj', 
                    withinvars=c('size', 'task'))
fitFA <- rm.anova(errFA, measurevar='fa', idvar='subj', 
                   withinvars=c('size', 'task'))
eta2(fitMiss)
eta2(fitFA)

## 1-way (display size )at miss errors and false alarm------------------
errFMiss <- subset(avg1,  task == "F" & target == "P")
errFFA <- subset(avg1,  task == "F" & target == "A")
errCMiss <- subset(avg1,  task == "C" & target == "P")
errCFA <- subset(avg1,  task == "C" & target == "A")
errSMiss <- subset(avg1,  task == "S" & target == "P")
errSFA <- subset(avg1,  task == "S" & target == "A")

fitFMiss <- rm.anova(errFMiss, measurevar='miss', idvar='subj', 
                     withinvars=c('size'))
fitFFA <- rm.anova(errFFA, measurevar='fa', idvar='subj', 
                     withinvars=c('size'))
fitCMiss <- rm.anova(errCMiss, measurevar='miss', idvar='subj', 
                     withinvars=c('size'))
fitCFA <- rm.anova(errCFA, measurevar='fa', idvar='subj', 
                   withinvars=c('size'))
fitSMiss <- rm.anova(errSMiss, measurevar='miss', idvar='subj', 
                     withinvars=c('size'))
fitSFA <- rm.anova(errSFA, measurevar='fa', idvar='subj', 
                   withinvars=c('size'))

eta2Size(fitFMiss)
eta2Size(fitFFA)
eta2Size(fitCMiss)
eta2Size(fitCFA)
eta2Size(fitSMiss)
eta2Size(fitSFA)