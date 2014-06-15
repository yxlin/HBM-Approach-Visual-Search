# Disclaimer-------------------------------------------------------
# Author:      Yishin Lin
# Date:        30 March, 2014
# Description: Calculate ANOVA and eta square for miss errors and 
# false alarm
rm(list=ls())
library(plyr); library(reshape2); library(car)

# Load data and functions ---------------------------------------------
load('./data/myData/avgFC.RData')
load('./data/myData/avgS.RData')
source("./functions/study2Funs.r")
source("./functions/eta2Size.R")
source("./functions/eta2.R")

# Organise the data frame ---------------------------------------------
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

# Save as wide form to allow SPSS to process -------------------------
# errMissWide <- dcast(errMiss, subj+task~size, value.var="miss")
# faWide <- dcast(errFA, subj+task~size, value.var="fa")
# names(errMissWide) <- c("subj", "task", paste0("size", c(3,6,12,18)))
# names(faWide) <- c("subj", "task", paste0("size", c(3,6,12,18)))
#  write.table(errMissWide, file="./data/myData/spss/miss_twoway_wide.txt", row.names=F)
#  write.table(faWide, file="./data/myData/spss/fa_twoway_wide.txt", row.names=F)

## 1-way (display size )at miss errors and false alarm-----------------
errFMiss <- subset(avg1,  task == "F" & target == "P")
errFFA <- subset(avg1,  task == "F" & target == "A")
errCMiss <- subset(avg1,  task == "C" & target == "P")
errCFA <- subset(avg1,  task == "C" & target == "A")
errSMiss <- subset(avg1,  task == "S" & target == "P")
errSFA <- subset(avg1,  task == "S" & target == "A")

# Save as wide form to allow SPSS to process --------------------------
# errMissWideF <- dcast(errFMiss, subj~size, value.var="miss")
# faWideF <- dcast(errFFA, subj~size, value.var="fa")
# errMissWideC <- dcast(errCMiss, subj~size, value.var="miss")
# faWideC <- dcast(errCFA, subj~size, value.var="fa")
# errMissWideS <- dcast(errSMiss, subj~size, value.var="miss")
# faWideS <- dcast(errSFA, subj~size, value.var="fa")
# 
# names(errMissWideF) <- c("subj", paste0("size", c(3,6,12,18)))
# names(faWideF) <- c("subj", paste0("size", c(3,6,12,18)))
# names(errMissWideC) <- c("subj", paste0("size", c(3,6,12,18)))
# names(faWideC) <- c("subj", paste0("size", c(3,6,12,18)))
# names(errMissWideS) <- c("subj", paste0("size", c(3,6,12,18)))
# names(faWideS) <- c("subj", paste0("size", c(3,6,12,18)))

# write.table(errMissWideF, file="./data/myData/spss/miss_onewayF_wide.txt", row.names=F)
# write.table(faWideF, file="./data/myData/spss/fa_onewayF_wide.txt", row.names=F)
# write.table(errMissWideC, file="./data/myData/spss/miss_onewayC_wide.txt", row.names=F)
# write.table(faWideC, file="./data/myData/spss/fa_onewayC_wide.txt", row.names=F)
# write.table(errMissWideS, file="./data/myData/spss/miss_onewayS_wide.txt", row.names=F)
# write.table(faWideS, file="./data/myData/spss/fa_onewayS_wide.txt", row.names=F)


# Run anova ------------------------------------------------------------
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