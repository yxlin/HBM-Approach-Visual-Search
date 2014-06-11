# Disclaimer --------------------------------------------
# Author:      Yishin Lin
# Date:        24 December, 2013
# Description: This script calculates descriptive 
# statistics in the discussion seciton
rm(list=ls())
load('./data/myData/avgFC.RData')
load('./data/myData/avgS.RData')
library(plyr)

## Re-sequence the task level  ----
avg1 <- rbind(avgFC, avgS)
avg1$task <- factor(avg1$task, levels=c('F','C','S'),
                    labels=c('F','C','S'))

rtP <- subset(avg1, target == "P")
rtPF <- subset(rtP,  task == "F")
rtPC <- subset(rtP,  task == "C")
rtPS <- subset(rtP,  task == "S")

# Print descriptive statistics --------------------------------------
# Shown in the sentence "The display size effect present in feature 
# search ... "
ddply(rtPF, .(size), summarise, M = mean(mean))
