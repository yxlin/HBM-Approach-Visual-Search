rm(list=ls())
# Disclaimer---------------------------------------------------
# Author: Yishin Lin
# Date: 24 December, 2013
# Description: The script calculated goodness-of-fit statistics
# parameters

# Load data ----------------------------------------------------------
# Load three parameters from BUGS Weibull estimates
load("./data/myData/BayesDataC/exp1WBC.RData")

# Load maximum estimated Weibull estimates separately from 
# spatial configuration task and then from feature and conjunction 
# tasks and 
load("./data/myData/mlewbFC.RData")

# Load the RT from BUGS Weibull estimation
load("./data/myData/BayesDataC/raw1WBC.RData")
raw1WBC <- subset(raw1WBC, target == "present")

# Load raw RT data from the organised RTs 
load('./data/myData/conjT.RData')

# 1 = F; 2 = C
rm(list=setdiff(ls(), c("conjT", "mlewbFC", "exp1WBC", "raw1WBC")))

# Handle conjunction search at vs1---------------------------------
HBM <- subset(exp1WBC, target == "present")
HBM$scale <- HBM$scale*1000
HBM$shift <- HBM$shift*1000
HBM <- HBM[,c(2,1,5,6,4)]

# mlewbFC contains present trials only
#m <- regexpr("[0-9]+", mlewbS$subj)
#tmp <- regmatches(mlewbS$subj, m)

mlewbFC$shift <- mlewbFC$thres
mleC <- subset(mlewbFC, task == 2)
MLE <- mleC[,-c(2,6)]

MLE$method <- "MLE"
HBM$method <- "HBM"

estdf <- data.frame(rbind(MLE, HBM))

rm(list=setdiff(ls(), c("conjT", "raw1WBC", "mleC", 
                        "exp1WBC", "estdf")))

# Start for loop -----------------------------------------------------
source("./functions/gof.R")  
conjPresent <- subset(conjT, target == "present")

size.seq <- sort(unique(conjPresent$size))
subj.seq <- sort(unique(conjPresent$subj))
m.seq <- sort(unique(estdf$method))

x1 <- NULL
x2 <- NULL

for(i in seq(along = size.seq)){
  subdata1 <- subset(conjPresent, size == size.seq[i])
  subdatawb1 <- subset(raw1WBC, size == size.seq[i])
  subest1 <- subset(estdf, size == size.seq[i])
  if (nrow(subdata1)  == 0) next
  for(j in seq(along = subj.seq)){
    subdata2 <- subset(subdata1, subj %in% subj.seq[j] )
    subdatawb2 <- subset(subdatawb1, subj %in% subj.seq[j] )
    subest2 <- subset(subest1, subj %in% subj.seq[j] )
    if (nrow(subdata2)  == 0) next
        for(k in seq(along = m.seq)){
          subest3 <- subset(subest2, method %in% m.seq[k])
    
          #---------------
          if(m.seq[k] == "MLE") {
            dat <-  subdata2$rt - subest3$shift 
          } else {
            dat <-  subdatawb2$rt*1000 - subest3$shift
          }
          est <- subest3[,3:5]
          goftest <- gof(dat=dat, est=est)
          x1 <- rbind(x1, goftest)
          
          #---------------
          nametmp <- c(size.seq[i], subj.seq[j], m.seq[k])
          x2 <- rbind(x2, nametmp)
          
          
      }
    }
  }

rownames(x1) <- 1:nrow(x1)
rownames(x2) <- 1:nrow(x2)

colnames(x2) <- c("size", "subj", "method")
gofstats <- data.frame(x1, x2)
gofstats$ad <- as.numeric(gofstats$ad)
gofstats$ks <- as.numeric(gofstats$ks)
gofstats$cvm <- as.numeric(gofstats$cvm)

save(gofstats, file="./data/gofData/gofstatsC.RData")
