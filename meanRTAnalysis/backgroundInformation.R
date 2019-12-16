#-------------------------------------------------------
# Research Participants background information  
# Author: Yishin Lin
# Date: 17 October, 2013
# Change log:

#----------------------------
# Load package and data 
loadedPackages <-c("plyr","plotrix", "car", "grid") 
suppressPackageStartupMessages(lapply(loadedPackages, require, character.only=TRUE));

rm(list=ls())
load("./data/mydata/featureConjBk.RData")
load("./data/mydata/twoFiveBk.RData")

# Because two groups of participant separately took part in 
# either feature-conjunction task or 2-5 task and I originally coded
# them as d1, d2, d3, ..., I remove column 8, which is 
# participant's unique ID code in a data set. Thus two data sets
# can be merged as one data set to be analysed.

#----------------------------
# Data process  
featureConjbk <- featureConjbk[,-8]
twoFivebk <- twoFivebk[,-8]

featureConjbk$exp <- "featureConj"
twoFivebk$exp <- "twoFive"

bkData <- rbind(featureConjbk, twoFivebk)

# Print out the range, mean and standard error of age 
range(bkData$Age)     # [1] 18 22
mean(bkData$Age)      # [1] 18.86364
std.error(bkData$Age) # [1] 0.1510741


# Print out male/female number of participants as well as 
# right/left handers 
table(bkData$Sex)
# Female   Male 
# 35      9
table(bkData$handedness)
# l  r 
# 6 38 
