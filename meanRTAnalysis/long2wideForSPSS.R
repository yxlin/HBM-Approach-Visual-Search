#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        24 December, 2013
# Description: Convert long form of data to wide form, so
# SPSS can process. Double-checking if rm.anova is reliable.
rm(list=ls())
load('./data/mydata/avgFC.RData')
load('./data/mydata/avgS.RData')
library(reshape2)

## --------------------------------
# Re-sequence the task level
# The default behaviour is to sort categorical names  as 
# alphabetical order. We wish to sort the task level following
# the presumed task difficulty 
avg1 <- rbind(avgFC, avgS)
avg1$task <- factor(avg1$task, levels=c('F','C','S'),
                    labels=c('F','C','S'))

## --------------------------------
# Analysis is focused on present trial
# RT 
# three tasks are all in an ASCII file
rtP <- subset(avg1, target == "P")
wideDat <- dcast(rtP, subj+task~size, value.var="mean")
# write.table(wideDat, file="./data/myData/spss/meanRT_Present_wide.txt", row.names=F)

# separate three tasks
rtPF <- subset(avg1,  target == "P"& task == "F")
rtPC <- subset(avg1,  target == "P"& task == "C")
rtPS <- subset(avg1,  target == "P"& task == "S")

wideDatF <- dcast(rtPF, subj~size, value.var="mean")
wideDatC <- dcast(rtPC, subj~size, value.var="mean")
wideDatS <- dcast(rtPS, subj~size, value.var="mean")

# write.table(wideDatF, file="./data/myData/spss/meanRT_Present_feature_wide.txt", 
#             row.names=F)
# write.table(wideDatC, file="./data/myData/spss/meanRT_Present_conj_wide.txt", 
#             row.names=F)
# write.table(wideDatS, file="./data/myData/spss/meanRT_Present_spat_wide.txt", 
#             row.names=F)

## --------------------------------
## Error
## --------------------------------
wideDatErr <- dcast(rtP, subj+task~size, value.var="errRate")
write.table(wideDatErr, file="./data/myData/spss/meanRT_PresentErr_wide.txt", 
            row.names=F)
wideDatFErr <- dcast(rtPF, subj~size, value.var="errRate")
wideDatCErr <- dcast(rtPC, subj~size, value.var="errRate")
wideDatSErr <- dcast(rtPS, subj~size, value.var="errRate")

write.table(wideDatFErr, file="./data/myData/spss/meanRT_Present_featureErr_wide.txt", 
            row.names=F)
write.table(wideDatCErr, file="./data/myData/spss/meanRT_Present_conjErr_wide.txt", 
            row.names=F)
write.table(wideDatSErr, file="./data/myData/spss/meanRT_Present_spatErr_wide.txt", 
            row.names=F)
