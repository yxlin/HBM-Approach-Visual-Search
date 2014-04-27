# Disclaimer---------------------------------------------
# Author:      Yishin Lin
# Date:        20 October, 2013
# Description: This file drew qqnorm for each cell at RT mean
rm(list=ls())
library(ggplot2); library(grid)
load('./data/WolfePalmer/trialAvg.RData')
load('./data/mydata/ylinrt.RData')

trimWolfeData$qqnorm <- qqnorm(trimWolfeData$rt, plot=F)$x
trimWolfeData$dataset <- "Wolfe"
wolfe <- subset(trimWolfeData, select=c("size", "target", "task", "rt", "qqnorm", "dataset"))

ylinrt$qqnorm <- qqnorm(ylinrt$rt, plot=F)$x
ylinrt$dataset <- "Lin"
lin <- subset(ylinrt, select=c("size", "target", "task", "rt", "qqnorm", "dataset"))
trialrt <- rbind(lin,wolfe)
rm(list=setdiff(ls(), c("trialrt")))

# Preparing for plotting--------------------------------
source('./functions/themeFuns.R')
source('./functions/plotqq.R')

# Set up for loop  -------------------------------------
size.seq <- sort(unique(trialrt$size))
task.seq <- sort(unique(trialrt$task))
target.seq <- sort(unique(trialrt$target))
dataset.seq <- sort(unique(trialrt$dataset))

# Run loop -----------------------------------------------
for(i in seq(along=size.seq)){
  for(j in seq(along=task.seq)){
    for(k in seq(along=target.seq)){
      for(l in seq(along=dataset.seq)){
        df <- subset(trialrt, size == size.seq[i] & 
                       task == task.seq[j] &
                       target == target.seq[k] & 
                       dataset == dataset.seq[l])
        df$measureVar <- df$rt 
        plotqq(df)
      }
    }
  }
}
