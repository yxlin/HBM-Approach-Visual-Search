rm(list=ls())
# Disclaimer------------------------------------------------------
# Author: Yishin Lin
# Date: 21 May, 2013
# Description: Run HBM

# Load data and functions--------------------------------------
load('./data/myData/featureT.RData')
# Gelman and Hill's (2007) random imputation function
source("./functions/random.imp.R")

# Set up sequences of each factor------------------------------
size.seq <- sort(unique(featureT$size))
target.seq <- sort(unique(featureT$target))
# We should have 19 (out of 20 participants) valid participants 
NSubj <- length(unique(featureT$subj))

# Start the for loop-------------------------------------------
for(i in seq(along = size.seq)){
  subdata1 <- subset(featureT, size == size.seq[i])
  if (nrow(subdata1)  == 0) next
  for(j in seq(along = target.seq)){
    subdata2 <- subset(subdata1, target %in% target.seq[j] )
    if (nrow(subdata2)  == 0) next
    
    # Processing the data now---------------------------------- 
    library(plyr)
    dataPerSubj <- ddply(subdata2, .(subj), summarise,
                         N = length(rt.sec), 
                         MeanRT = mean(rt.sec),
                         MinRT = min(rt.sec))
    minrt  <- dataPerSubj$MinRT
    
    dataPerSubj.ordered <- dataPerSubj[order(dataPerSubj$N,
                                             decreasing=TRUE),]
    dataPerSubj.ordered$subj.o <- dataPerSubj.ordered$subj
    dataPerSubj.ordered$subj <- 1:nrow(dataPerSubj.ordered)
    minrt <- dataPerSubj.ordered$MinRT
    
    rtSample <- NULL
    for(sIdx in dataPerSubj.ordered$subj.o) {
      tmpContainer <- subset(subdata2, subj == sIdx, 
                             select = 'rt.sec')[,1]
      
      if(!is.null(rtSample) && ncol(rtSample) > 
           length(tmpContainer))
        {
        tmp <- ncol(rtSample) - length(tmpContainer)
        tmpContainerNA <- append(tmpContainer, rep(NA, tmp))
        tmpContainer <- random.imp(tmpContainerNA)
        }
      rtSample <- rbind(rtSample, tmpContainer)
    }
    
    # Store data as a list ---------------------------------
    dataList <- list(NSubj = NSubj,
                     NTrials = ncol(rtSample),
                     y = structure(.Data = rtSample, 
                                   .Dim = dim(rtSample)), 
                     minrt = minrt)
    
    # Save data files as JAG format ------------------
    library(BRugs)
    fileNameRoot <- paste('data', size.seq[i], target.seq[j], 
                          sep='')
    BRugsName <- paste('./data/myData/BayesDataF/', 
                       fileNameRoot, 'BRugs.txt', sep='')
    bugsData(dataList, fileName = BRugsName)
    detach(package:BRugs)
    
    library(R2jags)
    JAGSName <- paste('./data/myData/BayesDataF/', 
                      fileNameRoot, 'JAGS.txt', sep='')
    bugs2jags(BRugsName, JAGSName)
    
    # Set initial prior values------------------------------
    # for each participant, set prior beta=0.9;  lambda=12, 
    # theta (scale) = lambda^(-1/beta); 
    # psi = min(each particpant) - min in the group
	  thetaInit <-  runif(NSubj, .3, 4)  
	  betaInit <- runif(NSubj, 0.9, 2)
	  psiInit <- dataPerSubj.ordered$Min -  
      min(dataPerSubj.ordered$Min)


# Initialization ----------------------------------------------
# Rouder et al.'s (2005) initial values
# "to simulate analysis of real data, these should be 
# reasonable, but far from true values"
#******************************************************
  initList <- function(){
    list(beta = betaInit,
         psi = psiInit,
         theta = thetaInit, 
        eta1 = .1 , eta2 = .1, 
        xi1 = .1, xi2 = .1)
  }

# Define parameters to monitor ----------------------------------
parameters <- c('beta', 'psi', 'theta')
nb <-  5000; nbt <-  10
nc <- 3; nct <- 1
ni <-  105000; nit <-  1000 
nt <- 4; ntt <- 1

# RUN THE CHAINS (Create, initialize, and adapt the model:)
# Test run ------------------------------------------------------
# jagsfitT <- jags(data=JAGSName, 
# model.file='./BayesRuns/model.txt', 
# inits=initList, parameters.to.save=parameters, 
# n.iter=nit, n.chain = nct, n.burnin = nbt, 
# n.thin = ntt)

# Real run ------------------------------------------------------
jagsfit <- jags(data=JAGSName, 
                model.file='./BayesRuns/model.txt', 
                inits=initList, parameters.to.save=parameters, 
                n.iter=ni, n.chain = nc, n.burnin = nb, 
                n.thin = nt)

pathWithNameRoot <- paste('./data/myData/BayesDataF/', 
                          fileNameRoot, sep="")
imageName <- paste(pathWithNameRoot, '.RData', sep="")
save.image(file=imageName)
   } 
} 