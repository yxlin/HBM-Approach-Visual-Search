# Disclaimer---------------------------------------------------
# Author:      Yishin Lin
# Date:        24 December, 2013
# Description: Calculate ANOVA and eta square for the 3 Weibull
# parameter
rm(list=ls())
load('./data/myData/weibull.RData')
source('./functions/rm.anova.R')
source('./functions/eta2.R')
source('./functions/eta2Size.R')
source('./functions/wb.anova.R')

loadedPackages <-c("plyr", "MASS", "timeDate", "timeSeries", "fBasics", "plotrix", "car", "pwr", "ggplot2", "grid", "FAdist", "reshape2") 
suppressPackageStartupMessages(lapply(loadedPackages, require, character.only=TRUE));

# Re-sequence the task level --------------------------------
x1$shift <- x1$shift*1000; 
x1$shape <- x1$shape*1000
x1$scale <- x1$scale*1000
x1$rt <- x1$rt*1000

x1$task <- ifelse(x1$task == "Feature", "F",
                  ifelse(x1$task == "Conj", "C", "S"))
x1 <- within(x1, task <- factor(task, levels = c("F", "C", "S") ))

# Prepare for using Anova in car package ---------------------
size <- factor(c("size3", "size6", "size12", "size18"), 
               levels=c("size3", "size6", "size12", "size18"))
idata <- data.frame(size)

# Results ----------------------------------------------------------
scaleOut <- wb.anova('scale')
shapeOut <- wb.anova('shape')
shiftOut <- wb.anova('shift')
scaleOut[1]
shapeOut[1]
shiftOut[1]