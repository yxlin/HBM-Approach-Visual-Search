#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        18 October, 2013
# Description: 
# 1. The file performs trial averaging. It calculates the 
# cell mean by averaging each RT across trials in an 
# experiment conditon.
# 2. We have 4 (display size) x 2 (target present versus 
# absent), 8 conditions.
# 3. The feature and conjunction search should be calculated 
# seprately from the spatial configuration search, because
# the two data sets compose of two different groups of 
# participants.
#-------------------------------------------------------  

rm(list=ls())
library(plyr); 
#----------------------------
# Load package and data 
#----------------------------
# 1. Load the aggregated data which outliers have been trimmed off
load('./data/mydata/ylinrt.RData')

# 2. load complete data set, in order to calculate error rate.
# -- 2 v 5 used a different group of participants
# -- participant d11 in the feature and conjunction search 
# gave chance-level performance, so we excluded her/him.
load('./data/mydata/featureWithSubjInfo.RData');
featAll <- subset(x0, subj != "d11")
load('./data/mydata/conjWithSubjInfo.RData'); 
conjAll <- subset(x0, subj != "d11")
load('./data/mydata/twoFiveWithSubjInfo.RData'); two5All <- x0;rm(x0)

# ------------------------------
# Assign level to target column
# ------------------------------ 
# Because we want to plot firstly present and then absent,
# here we assign P a level coming early than A. By default,
# R uses alphabetical order 
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
featTrimmed <- subset(ylinrt, task == "F")
conjTrimmed <- subset(ylinrt, task == "C")
two5Trimmed <- subset(ylinrt, task == "S")

#----------------------------
# Average RT across trials in 
# the trimmed data set
#----------------------------
# To keep the label of task, I include the task column, which has only
# one level.  By default ddply will print NaN and NA for the levels 
# that do not exist for a specific data set (e.g., featTrimmed won't
# have C and S levels).  So I set .drop option to TRUE to drop those
# NaNs and NAs
avgF <- ddply(featTrimmed, .(size, target, task, subj), .drop=TRUE,
                  summarize, 
                  N = length(rt), 
                  mean = mean(rt), median = median(rt))
avgC <- ddply(conjTrimmed, .(size, target, task, subj), .drop=TRUE,
              summarize, 
              N = length(rt), 
              mean = mean(rt), median = median(rt))
avgS <- ddply(two5Trimmed, .(size, target, task,subj), .drop=TRUE,
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

#----------------------------
# Bind data together, if they come from the 
# same group of participant 
#----------------------------
avgFC <- rbind(tmpDfF, tmpDfC)
avgS <- rbind(tmpDfS)
save(avgFC, file='./data/mydata/avgFC.RData')
save(avgS, file='./data/mydata/avgS.RData')



