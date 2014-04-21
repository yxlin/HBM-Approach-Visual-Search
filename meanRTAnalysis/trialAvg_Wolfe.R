# Disclaimer ---------------------------------------------------
# Author:      Yishin Lin
# Date:        20 October, 2013
# Description: The file calculated average values in each 
# conditons, using data from Wolfe et al. (2010).
rm(list=ls())
load('./data/WolfePalmer/wolfe2010RawDataAllTasks.RData')
library(plyr); library(plotrix)

# Change numeral codes to character --------------------------------
wolfe2010Data$target <- ifelse(
  wolfe2010Data$target == 1, 'P', 
  ifelse(wolfe2010Data$target == 0, 'A', 'unknown'))

wolfe2010Data$error <- ifelse(
  wolfe2010Data$error == 0, 'correct', 
  ifelse(wolfe2010Data$error == 1, 'incorrect', 'unknown'))

wolfe2010Data$task <- ifelse(
  wolfe2010Data$task == "RVvGV", "F", 
  ifelse(wolfe2010Data$task == "RVvRHGV", "C",
  ifelse(wolfe2010Data$task == "2_vs_5", "S",
  'unknown')))

wolfe2010Data$task <- factor(
  wolfe2010Data$task, levels=c('F','C','S'),
  labels=c('F','C','S'))

# Re-level present and absent trial to make present trials
# come firts
wolfe2010Data$target <- factor(wolfe2010Data$target, 
                               levels=c('P','A'),
                               labels=c('P','A'))

wolfe2010Data$slope <- wolfe2010Data$rt / wolfe2010Data$size 
wolfe2010Data$rt.sec <- wolfe2010Data$rt / 1000

# remove of the repeated column of set size  
# vsRawWolfe should be the same as wolfe2010Data 
rawWolfeData <-wolfe2010Data[,-c(9)]

# Trimming data --------------------------------
fRaw <- subset(wolfe2010Data, task == "F")
fRawTrimmed <- subset(wolfe2010Data, task == "F" & rt >= 200 & 
                        rt <= 4000 & error == "correct")
rejGenF <- 100 * signif( 1-nrow(fRawTrimmed) / nrow(fRaw), 2)

cRaw <- subset(wolfe2010Data, task == "C")
cRawTrimmed <- subset(wolfe2010Data,  task == "C" & rt >= 200 & 
                        rt <= 4000 & error == "correct")
rejGenC <- 100 * signif( 1-nrow(cRawTrimmed) / nrow(cRaw), 2)

sRaw <- subset(wolfe2010Data, task == "S")
sRawTrimmed <- subset(wolfe2010Data, task == "S" & rt >= 200 & 
                        rt <= 8000 & error == "correct")
rejGenS <- 100 * signif( 1-nrow(sRawTrimmed) / nrow(sRaw), 2)

trimWolfeData <- rbind(fRawTrimmed, cRawTrimmed, sRawTrimmed)

# Calculate error rate--------------------------------
rawWolfe <- ddply(rawWolfeData, .(size, target, task, subj), 
                  .drop=FALSE,
                  summarize, 
                  N = length(rt), 
                  mean = mean(rt), median = median(rt),
                  sdRT = sd(rt), seRT = std.error(rt))

errTab <- ddply(rawWolfeData, .(size, target, task, error, subj), 
                .drop=FALSE, summarise, 
                errRate = length(rt) )
corr <- subset(errTab, error=="correct",  select=errRate)
err <- subset(errTab, error=="incorrect", select=errRate)
errRate <- (err/(err+corr))*100

avg0 <- cbind(rawWolfe, errRate)
avg1 <- na.exclude(avg0)


# Clearing up and save raw, averaged, trimmed data-------------------
rm(list=setdiff(ls(), c("rawWolfeData", "avg1",
                        "trimWolfeData")))
# save(rawWolfeData, avg1, trimWolfeData, 
#      file='./data/WolfePalmer/trialAvg.RData')
