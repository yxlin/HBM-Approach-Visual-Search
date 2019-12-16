rm(list=ls())
# Disclaimer---------------------------------------------------
# Author: Yishin Lin
# Date: 24 December, 2013
# Description: The script calculated goodness-of-fit statistics
# parameters

# Load three parameters from BUGS Weibull estimates
load("./data/myData/BayesDataS/exp1WBS.RData")
load("./data/myData/mlewbS.RData")
load("./data/myData/BayesDataS/raw1WBS.RData")
raw1WBS <- subset(raw1WBS, target == "present")
load('./data/myData/twoFiveT.RData')
rm(list=setdiff(ls(), c("twoFiveT", "mlewbS", "exp1WBS", "raw1WBS")))

# HBM parameters ------------------------------------------------------
HBM <- subset(exp1WBS, target == "present")
HBM$scale <- HBM$scale*1000
HBM$shift <- HBM$shift*1000
HBM <- HBM[,c(2,1,5,6,4)]

mlewbS$shift <- mlewbS$thres
MLE <- mlewbS[,-5]

MLE$method <- "MLE"
HBM$method <- "HBM"

estdf <- data.frame(rbind(MLE, HBM))

rm(list=setdiff(ls(), c("twoFiveT", "raw1WBS", "mlewbS", 
                        "exp1WBS", "estdf")))

# Load function -------------------------------------------------------
source("./functions/gof.R") 
twoFivePresent <- subset(twoFiveT, target == "present")

size.seq <- sort(unique(twoFivePresent$size))
subj.seq <- sort(unique(twoFivePresent$subj))
m.seq <- sort(unique(estdf$method))

# Start for loop ------------------------------------------------------
x1 <- NULL
x2 <- NULL
for(i in seq(along = size.seq)){
  subdata1 <- subset(twoFivePresent, size == size.seq[i])
  subdatawb1 <- subset(raw1WBS, size == size.seq[i])
  subest1 <- subset(estdf, size == size.seq[i])
  if (nrow(subdata1)  == 0) next
  for(j in seq(along = subj.seq)){
    subdata2 <- subset(subdata1, subj %in% subj.seq[j] )
    subdatawb2 <- subset(subdatawb1, subj %in% subj.seq[j] )
    subest2 <- subset(subest1, subj %in% subj.seq[j] )
    if (nrow(subdata2)  == 0) next
        for(k in seq(along = m.seq)){
          subest3 <- subset(subest2, method %in% m.seq[k])
    
          if(m.seq[k] == "MLE") {
            dat <-  subdata2$rt - subest3$shift 
          } else {
            dat <-  subdatawb2$rt*1000 - subest3$shift
          }
          est <- subest3[,3:5]
          goftest <- gof(dat=dat, est=est)
          x1 <- rbind(x1, goftest)
          
          nametmp <- c(size.seq[i], subj.seq[j], m.seq[k])
          x2 <- rbind(x2, nametmp)
      }
    }
  }

# Save the data -------------------------------------------------------
rownames(x1) <- 1:nrow(x1); rownames(x2) <- 1:nrow(x2)
colnames(x2) <- c("size", "subj", "method")
gofstats <- data.frame(x1, x2)
gofstats$ad <- as.numeric(gofstats$ad)
gofstats$ks <- as.numeric(gofstats$ks)
gofstats$cvm <- as.numeric(gofstats$cvm)

save(gofstats, file="./data/gofData/gofstatsS.RData")