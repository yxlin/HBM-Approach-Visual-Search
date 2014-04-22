# Disclaimer---------------------------------------------------
# Author:      Yishin Lin
# Date:        24 December, 2013
# Description: Calculate ANOVA and eta square for the 3 Weibull
# parameters
rm(list=ls())

# Load data and packages---------------------------------------------
load('./data/WolfePalmer/featureWB.Wolfe.RData')
load('./data/WolfePalmer/conjWB.Wolfe.RData')
load('./data/WolfePalmer/twovFiveWB.Wolfe.RData')
source('./functions/rm.anova.R')
source('./functions/eta2.R')
source('./functions/eta2Size.R')
source('./functions/wb.anova.R')
loadedPackages <-c("plyr", "car", "reshape2") 
suppressPackageStartupMessages(lapply(loadedPackages, require, character.only=TRUE));

# --------------------------------
# Re-sequence the task level
featureWB$task <- "F"; conjWB$task <- "C"; twovFiveWB$task <- "S"
wolfeDataWB <- rbind(featureWB, conjWB, twovFiveWB)

wolfeDataWB$shift <- wolfeDataWB$shift*1000
wolfeDataWB$shape <- wolfeDataWB$shape*1000
wolfeDataWB$scale <- wolfeDataWB$scale*1000
wolfeDataWB$rt <- wolfeDataWB$rt*1000

wolfeDataWB <- within(wolfeDataWB, task <- factor(task, levels = c("F", "C", "S") ))
wolfeDataWB$target <- ifelse(wolfeDataWB$target == "present", "P", "A") 
wolfeDataWB <- within(wolfeDataWB, target <- factor(target, levels = c("P", "A") ))
x1 <- wolfeDataWB

# Prepare for using Anova in car package ---------------------
size <- factor(c("size3", "size6", "size12", "size18"), 
               levels=c("size3", "size6", "size12", "size18"))
idata <- data.frame(size)

# Results ----------------------------------------------------------
scaleOut <- wb.anova('scale', trials='P')
shapeOut <- wb.anova('shape', trials='P')
shiftOut <- wb.anova('shift', trials='P')

scaleOutA <- wb.anova('scale', trials='A')
shapeOutA <- wb.anova('shape', trials='A')
shiftOutA <- wb.anova('shift', trials='A')