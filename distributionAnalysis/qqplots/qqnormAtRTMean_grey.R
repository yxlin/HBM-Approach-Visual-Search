# Disclaimer---------------------------------------------
# Author:      Yishin Lin
# Date:        20 October, 2013
# Description: This file drew qqnorm for each cell at RT mean
rm(list=ls())
load('./data/WolfePalmer/trialAvg.RData')
avg1$target <- factor(avg1$target, levels=c('P','A'),
                      labels=c('P','A'))
avg1$task <- factor(avg1$task, levels=c('F','C','S'),
                    labels=c('F','C','S'))
avg1$qqnorm <- qqnorm(avg1$mean, plot=F)$x
avg1$dataset <- "Wolfe"
wolfe <- subset(avg1, select=c("size", "target", "task", "mean", "qqnorm", "dataset"))

rm(list=setdiff(ls(), c("wolfe")))
load('./data/mydata/avgFC.RData')
load('./data/mydata/avgS.RData')
library(ggplot2); library(grid)

# Preparing for plotting--------------------------------
lin <- rbind(avgFC, avgS)
lin$task <- factor(lin$task, levels=c('F','C','S'),
                    labels=c('F','C','S'))
lin$qqnorm <- qqnorm(lin$mean, plot=F)$x
lin$dataset <- "Lin"
lin <- subset(lin, select=c("size", "target", "task", "mean", "qqnorm", "dataset"))
avg1 <- rbind(lin,wolfe)

rm(list=setdiff(ls(), c("avg1")))
source('./functions/themeFuns.R')
source('./functions/plotqq.R')

# Set up for loop  -------------------------------------
size.seq <- sort(unique(avg1$size))
task.seq <- sort(unique(avg1$task))
target.seq <- sort(unique(avg1$target))
dataset.seq <- sort(unique(avg1$dataset))

# Run loop -----------------------------------------------
for(i in seq(along=size.seq)){
  for(j in seq(along=task.seq)){
    for(k in seq(along=target.seq)){
      for(l in seq(along=dataset.seq)){
        df <- subset(avg1, size == size.seq[i] & 
                       task == task.seq[j] &
                       target == target.seq[k] & 
                       dataset == dataset.seq[l])
        df$measureVar <- df$mean 
        plotqq(df)
      }
    }
  }
}