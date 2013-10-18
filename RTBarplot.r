#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        17 October, 2013
# Description: 
#-------------------------------------------------------  

rm(list=ls())
library(plyr); library(ggplot2)
#----------------------------
# Load package and data 
#----------------------------
# 1. Load the aggregated data which outliers have been trimmed off
load('./data/mydata/ylinrt.RData')

# 2. load complete data set, in order to calculate error rate.
# Note that 2 v 5 used a different group of participants
load('./data/mydata/featureWithSubjInfo.RData');
featAll <- subset(x0, subj != "d11")
load('./data/mydata/conjWithSubjInfo.RData'); 
conjAll <- subset(x0, subj != "d11")
load('./data/mydata/twoFiveWithSubjInfo.RData'); two5All <- x0;rm(x0)

featAll$target <- ifelse(featAll$target == "present", "P", "A") 
featAll <- within(featAll, 
                  target <- factor(target, levels = c("P", "A") ))
conjAll$target <- ifelse(conjAll$target == "present", "P", "A") 
conjAll <- within(conjAll, 
                  target <- factor(target, levels = c("P", "A") ))
two5All$target <- ifelse(two5All$target == "present", "P", "A") 
two5All <- within(two5All, 
                  target <- factor(target, levels = c("P", "A") ))

#----------------------------
# Sub-setting trimmed data 
#----------------------------
# Because of different participant group
featTrimmed <- subset(ylinrt, task == "F")
conjTrimmed <- subset(ylinrt, task == "C")
two5Trimmed <- subset(ylinrt, task == "S")

#----------------------------
# Average RT across trials in 
# the trimmed data set
#----------------------------
avgF <- ddply(featTrimmed, .(size, target, task, subj), .drop=TRUE,
                  summarize, 
                  N = length(rt), 
                  mean = mean(rt), median = median(rt))
avgC <- ddply(conjTrimmed, .(size, target, task, subj), .drop=TRUE,
              summarize, 
              N = length(rt), 
              mean = mean(rt), median = median(rt))
avgS <- ddply(two5Trimmed, .(size, target, task, subj), .drop=TRUE,
              summarize, 
              N = length(rt), 
              mean = mean(rt), median = median(rt))

#----------------------------
# Compute error rate 
#----------------------------
featErr <- ddply(featAll, .(size, target, keyStatus, subj), 
                .drop=FALSE, summarise, 
                errRate = length(rt) )
conjErr <- ddply(conjAll, .(size, target, keyStatus, subj), 
                 .drop=FALSE, summarise, 
                 errRate = length(rt) )
two5Err <- ddply(two5All, .(size, target, keyStatus, subj), 
                 .drop=FALSE, summarise, 
                 errRate = length(rt) )

featCorr <- subset(featErr, keyStatus=="correct",  select=errRate)
featErr <- subset(featErr, keyStatus=="incorrect", select=errRate)
featER <- (featErr/(featErr+featCorr))*100
conjCorr <- subset(conjErr, keyStatus=="correct",  select=errRate)
conjErr <- subset(conjErr, keyStatus=="incorrect", select=errRate)
conjER <- (conjErr/(conjErr+conjCorr))*100
two5Corr <- subset(two5Err, keyStatus=="correct",  select=errRate)
two5Err <- subset(two5Err, keyStatus=="incorrect", select=errRate)
two5ER <- (two5Err/(two5Err+two5Corr))*100

tmpDfF <- cbind(avgF, featER)
tmpDfC <- cbind(avgC, conjER)
tmpDfS <- cbind(avgS, two5ER)

avg0 <- rbind(tmpDfF, tmpDfC, tmpDfS)
save(avg0, file='./data/mydata/acrossTrialAvg.RData')
