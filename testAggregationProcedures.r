#-------------------------------------------------------
# Author:     Yishin Lin
# Date:       17 October, 2013
# Description: Examine two different procedures (different 
# sequence to merge data) reach an identical data set 
#-------------------------------------------------------  

rm(list=ls())
#----------------------------
# Load package and data 
#----------------------------
# 1. Because during data merging, I used always x0 as a container to 
# append data, it is necessary to store each task (feature, 
# conjunction and spatial configuration tasks) separately into
# different data frame.
# 2. These data sets keep the original form without trimming off
# outliers 
load('./data/mydata/featureWithSubjInfo.RData')
featureT2 <- subset(x0, 
                  rt >= 200 &   # rt greater and equal to 200 ms
                  rt <= 4000 &  # less and equal to 4000 ms 
                  keyStatus == "correct" & # correct responses only
                  subj != "d11")# d11 responded at a chance level 

load('./data/mydata/conjWithSubjInfo.RData')
conjT2 <- subset(x0, 
                 rt >= 200 & 
                 rt <= 4000 & 
                 keyStatus == "correct" & 
                 subj != "d11")

# 2 v 5 used a different group of participants
load('./data/mydata/twoFiveWithSubjInfo.RData')
twoFiveT2 <- subset(x0, 
                    rt >= 200 & 
                    rt <= 8000 & 
                    keyStatus == "correct")


# These data sets did trimming at the stage when data from each 
# participant was processed. 
load('./data/mydata/featureT.RData')
load('./data/mydata/conjT.RData')
load('./data/mydata/twoFiveT.RData')
featureT$task <- "F"
conjT$task <- "C"
twoFiveT$task <- "S"
ylinrt <- rbind(featureT, conjT, twoFiveT)

# A few data organisation procedures
ylinrt <- within(ylinrt, 
                  task <- factor(task, levels = c("F", "C", "S") )
)
ylinrt$target <- ifelse(ylinrt$target == "present", "P", "A") 
ylinrt <- within(ylinrt, target <- factor(target, levels = c("P", "A") ))

save(ylinrt, file='./data/mydata/ylinrt.RData')

# test to see if two different ways to organise the data reaches 
# the same data sets.
all(featureT$rt == featureT2$rt)
all(conjT$rt == conjT2$rt)
all(twoFiveT$rt == twoFiveT2$rt)

