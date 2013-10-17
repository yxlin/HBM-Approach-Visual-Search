# Report Background Information of Research Participants
#-------------------------------------------------------
# Author: Yishin Lin
# Date: 17 October, 2013
# Change log:
#-------------------------------------------------------  

#----------------------------
# Load package and data 
#----------------------------
loadedPackages <-c("plyr","plotrix", "car", "grid") 
suppressPackageStartupMessages(lapply(loadedPackages, require, character.only=TRUE));

rm(list=ls())
load("featureConjBk.RData")
load("twoFiveBk.RData")


# Because two groups of participant separately took part in 
# either feature-conjunction task or 2-5 task and I originally coded
# them as d1, d2, d3, ..., I remove column 8, which is 
# participant's unique ID code in a data set. Thus two dat sets
# can be merged as one data set to be analysed.

#----------------------------
# Data process  
#----------------------------
featureConjbk <- featureConjbk[,-8]
twoFivebk <- twoFivebk[,-8]

featureConjbk$exp <- "featureConj"
twoFivebk$exp <- "twoFive"

bkData <- rbind(featureConjbk, twoFivebk)

# Print out the range, mean and standard error of age for 
# the participants
range(bkData$Age)
mean(bkData$Age)
std.error(bkData$Age)

# Print out male/female number of participants as well as 
# right/left handers 
table(bkData$Sex)
table(bkData$handedness)
