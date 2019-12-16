# Load packages and functions----------------------------------------
rm(list=ls())

# Load data ----------------------------------------------------------
# three parameters Weibull data from BUGS 
load("./data/myData/BayesDataF/exp1WBF.RData")

# maximum likelihood estimates for Weibull parameters at 
#feature and conjunction searches 
load("./data/myData/mlewbFC.RData")

# Load the RT from BUGS Weibull estimation
load("./data/myData/BayesDataF/raw1WBF.RData")
raw1WBF <- subset(raw1WBF, target == "present")

# Load raw RT data from the organised RTs 
load('./data/myData/featureT.RData')

# 1 = F; 2 = C
rm(list=setdiff(ls(), c("featureT", "mlewbFC", "exp1WBF", "raw1WBF")))

# Present trial feature search----------------------------------
HBM <- subset(exp1WBF, target == "present")
HBM$scale <- HBM$scale*1000
HBM$shift <- HBM$shift*1000
HBM <- HBM[,c(2,1,5,6,4)]

mlewbFC$shift <- mlewbFC$thres
mleF <- subset(mlewbFC, task == 1)
MLE <- mleF[,-c(2,6)]

MLE$method <- "MLE"
HBM$method <- "HBM"

estdf <- data.frame(rbind(MLE, HBM))

rm(list=setdiff(ls(), c("featureT", "raw1WBF", "mleF", 
                        "exp1WBF", "estdf")))

# Prepare the loop ---------------------------------------------------
source("./functions/gof.R")  
featurePresent <- subset(featureT, target == "present")

size.seq <- sort(unique(featurePresent$size))
subj.seq <- sort(unique(featurePresent$subj))
m.seq <- sort(unique(estdf$method))

# Start the looping  -------------------------------------------------
x1 <- NULL; x2 <- NULL # Data frame containers
for(i in seq(along = size.seq)){
  subdata1 <- subset(featurePresent, size == size.seq[i])
  subdatawb1 <- subset(raw1WBF, size == size.seq[i])
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
          
          nametmp <- c(size.seq[i], subj.seq[j], m.seq[k])
          x2 <- rbind(x2, nametmp)
      }
    }
  }

# Tidy up the data frames ---------------------------------------------
rownames(x1) <- 1:nrow(x1)
rownames(x2) <- 1:nrow(x2)

colnames(x2) <- c("size", "subj", "method")
gofstats <- data.frame(x1, x2)
gofstats$ad <- as.numeric(gofstats$ad)
gofstats$ks <- as.numeric(gofstats$ks)
gofstats$cvm <- as.numeric(gofstats$cvm)

# Save  ---------------------------------------------------------------
# save(gofstats, file="./data/gofData/gofstatsF.RData")