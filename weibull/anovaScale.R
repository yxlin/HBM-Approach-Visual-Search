# Disclaimer---------------------------------------------------
# Author:      Yishin Lin
# Date:        24 December, 2013
# Description: Calculate ANOVA and eta square at scale
rm(list=ls())
load('./data/myData/weibull.RData')
source('./functions/rm.anova.R')
source('./functions/eta2.R')
source('./functions/eta2Size.R')
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

## Prepare for using Anova in car package ---------------------
size <- factor(c("size3", "size6", "size12", "size18"), 
               levels=c("size3", "size6", "size12", "size18"))
idata <- data.frame(size)

# Two-way ANOVA (present trial, between task factor) -----------------
# Anova at display size x task anova at scale parameter 
# assuming three levels of task is a between-subject factor
scaleWide <- dcast(x1, subj+task~size, value.var="scale", 
                subset=.(target=="present"))
names(scaleWide) <- c("subj", "task", "size3", "size6", "size12", "size18")
mod.ok <- lm(cbind(size3, size6, size12, size18) ~  task, 
             data=scaleWide)
fit <- Anova(mod.ok, idata=idata, idesign=~size, type="III") 
eta2(fit)

## One-way Anova (display size, scale) ----------------------------
x1PF <- subset(x1, target == "present" & task == "F")
x1PC <- subset(x1, target == "present" & task == "C")
x1PS <- subset(x1, target == "present" & task == "S")
shiftPF <- rm.anova(x1PF, measurevar"scale", idvar="subj", 
                    withinvars=c("size"))
shiftPC <- rm.anova(x1PC, measurevar="scale", idvar="subj", 
                    withinvars=c("size"))
shiftPS <- rm.anova(x1PS, measurevar='scale', idvar="subj", 
                    withinvars=c("size"))
eta2Size(shiftPF)
eta2Size(shiftPC)
eta2Size(shiftPS)

# Two-way ANOVA (present trial, within task factor) -----------------
# only at feature and conjunction
x1PFC <- subset(x1, target == "present" & task != "S")
shiftPFC <- rm.anova(x1PFC, measurevar='scale', idvar='subj', 
                     withinvars=c("size", "task"))
eta2(shiftPFC)