# Disclaimer-------------------------------------------------------
# Author:      Yishin Lin
# Date:        24 December, 2013
# Description: Calculate ANOVA and eta square; for two-way ANOVA
#              SPSS  printout
rm(list=ls())
load('./data/myData/avgFC.RData')
load('./data/myData/avgS.RData')
source("./functions/rm.anova.r")   
source("./functions/eta2Size.r")

## Re-sequence the task level -------------------------------------
avg1 <- rbind(avgFC, avgS)
avg1$task <- factor(avg1$task, levels=c('F','C','S'),
                    labels=c('F','C','S'))

rtP <- subset(avg1, target == "P")
rtPF <- subset(rtP,  task == "F")
rtPC <- subset(rtP,  task == "C")
rtPS <- subset(rtP,  task == "S")

rtA <- subset(avg1, target == "A")
rtAF <- subset(rtA,  task == "F")
rtAC <- subset(rtA,  task == "C")
rtAS <- subset(rtA,  task == "S")

## 1-way (display size) at RT (present trials)-----------------
rtfitPF <- rm.anova(rtPF, measurevar='mean', idvar='subj', withinvars=c('size'))
rtfitPC <- rm.anova(rtPC, measurevar='mean', idvar='subj', withinvars=c('size'))
rtfitPS <- rm.anova(rtPS, measurevar='mean', idvar='subj', withinvars=c('size'))
eta2Size(rtfitPF)
eta2Size(rtfitPC)
eta2Size(rtfitPS)

## 1-way (display size )at overall errors------------------------------
rtfitPFErr <- rm.anova(rtPF, measurevar='errRate', idvar='subj', withinvars=c('size'))
rtfitPCErr <- rm.anova(rtPC, measurevar='errRate', idvar='subj', withinvars=c('size'))
rtfitPSErr <- rm.anova(rtPS, measurevar='errRate', idvar='subj', withinvars=c('size'))
eta2Size(rtfitPFErr)
eta2Size(rtfitPCErr)
eta2Size(rtfitPSErr)

# library(plyr)
# ddply(avg1, .(task), summarise, SD = sd(mean))

## 1-way (display size) at RT (absent trials)-----------------
rtfitAF <- rm.anova(rtAF, measurevar='mean', idvar='subj', withinvars=c('size'))
rtfitAC <- rm.anova(rtAC, measurevar='mean', idvar='subj', withinvars=c('size'))
rtfitAS <- rm.anova(rtAS, measurevar='mean', idvar='subj', withinvars=c('size'))
eta2Size(rtfitAF)
eta2Size(rtfitAC)
eta2Size(rtfitAS)

## 1-way (display size )at overall errors------------------------------
rtfitAFErr <- rm.anova(rtAF, measurevar='errRate', idvar='subj', withinvars=c('size'))
rtfitACErr <- rm.anova(rtAC, measurevar='errRate', idvar='subj', withinvars=c('size'))
rtfitASErr <- rm.anova(rtAS, measurevar='errRate', idvar='subj', withinvars=c('size'))
eta2Size(rtfitAFErr)
eta2Size(rtfitACErr)
eta2Size(rtfitASErr)


