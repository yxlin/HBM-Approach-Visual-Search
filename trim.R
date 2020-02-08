## Feature, Conjunction and Two versus Five Analysis
## ======================================================

## Load the data
## ```{r load data and packages}
rm(list=ls())
setwd('/media/yslin/MERLIN/Documents/HBM-Approach-Visual-Search-master/')
load('data/myData/featureData/feature.RData')
load('data/myData/conjData/conj.RData')
load('data/myData/twoFiveData/twoFive.RData')

source('functions/study2Funs.R')
loadedPackages <-c("plyr", "MASS", "timeDate", "timeSeries", "fBasics", "plotrix", "car", "pwr", "ggplot2", "grid", "FAdist") 
suppressPackageStartupMessages(lapply(loadedPackages, require, character.only=TRUE));
## ```


## Filter assumed outliers as Wolfe et al (2010, VR)
## ```{r filter data}
feature$rt.sec <- feature$rt / 1000
feature$slope <- feature$rt / feature$size
conj$rt.sec <- conj$rt / 1000
conj$slope <- conj$rt / conj$size
twoFive$rt.sec <- twoFive$rt / 1000
twoFive$slope <- twoFive$rt / twoFive$size

featureDf <- ddply(feature, .(target, size, subj), summarise,
                   N = length(rt),
                   Mean = mean(rt))

conjDf <- ddply(conj, .(target, size, subj), summarise,
             N = length(rt),
             Mean = mean(rt))

twoFiveDf <- ddply(twoFive, .(target, size, subj), summarise,
             N = length(rt),
             Mean = mean(rt))


dplyr::tbl_df(featureDf)
dplyr::tbl_df(conjDf)
dplyr::tbl_df(twoFiveDf)

## rt is in ms
hist(feature$rt, breaks=50, col="grey")
hist(conj$rt, breaks=50, col="grey")
hist(twoFive$rt, breaks=50, col="grey")

table(feature$keyStatus)
table(conj$keyStatus)
table(twoFive$keyStatus)

##   correct incorrect 
##     15497       663
##   correct incorrect   unknown 
##     14767      1390         3
##   correct incorrect  outliers 
##     14989      1165         6 


length(table(feature$subj))
## d11 responded at chance level
## prc means practice trials
featureT <- subset(feature, rt >= 200 & rt <= 4000 & keyStatus == "correct" &
                            blockName != "prc" & subj != "d11")
conjT <- subset(conj, rt >= 200 & rt <= 4000 & keyStatus == "correct" &
                      blockName != "prc" & subj != "d11")
## 2 v 5 used different group of participant
twoFiveT <- subset(twoFive, rt >= 200 & rt <= 8000 & keyStatus == "correct" &
                            blockName != "prc")

## In conjunction search, d11's accuracy is lower than 50%.
## table(featureT$subj) / table(feature$subj)
## table(conjT$subj) / table(conj$subj)
table(twoFiveT$subj) / table(twoFive$subj)

save(featureT, file="data/myData/featureT.RData")
save(conjT, file="data/myData/conjT.RData")
save(twoFiveT, file="data/myData/twoFiveT.RData")
