# Load packages and functions ---------------------------------------
loadedPackages <-c("plyr",  "gamlss", "statmod", "reshape2", "timeSeries", "FAdist") 
suppressPackageStartupMessages(lapply(loadedPackages, require, character.only=TRUE));
rm(list=setdiff(ls(), c()))
source("./functions/maximumLikeWeibull.R")

# True distribution settings ---------------------------------------
# 1. Gaussian 
mu <- 1000; sigma <- 100
exp.rt.norm <- mu 
exp.var.norm <- sigma^2 
NTrials <- seq(20, 500, by=50)               
NSubjs <- 20                    
n <- NTrials*NSubjs               

# Generate simulated data in 10 different situations----------
y0 <- lapply( 1:length(n), function(i) rnorm(n[i], mean=mu, sd=sigma) )

# 2. exGaussian; this step will take some time---------------------- 
mu <- 910.56; tau <- 89.443; sigma <- 44.721
y1 <- lapply( 1:length(n), function(i) rexGAUS(n[i], mu=mu, nu=tau, sigma=sigma) )
exp.rt.exG <- mu + tau
exp.var.exG <- sigma^2 + tau^2
skew.exG <- (2*tau^3) / ((sigma^2+tau^2)^(3/2))

# 3. inverse Gaussian, or called Wald-------------------------------
mu <- 275; lambda <- 2000; kappa <- 725
y2 <- lapply( 1:length(n), function(i) {
              rinvgauss(n[i], mu=mu, lambda=lambda) + kappa
            }
)
exp.rt.wald <- mu + kappa
exp.var.wald <- (mu^3) / lambda
skew.wald <- 3*sqrt(mu/lambda)

# 4. three-parameter Weibull --------------------------------------
# shape = gamma (or a), scale = beta (or b) and thres=shift=alpha (or m)
shape <- 2.0  # gamma
scale <- 220  # beta
shift <- 800  # alpha
y3 <- lapply( 1:length(n), function(i) {
             rweibull3(n[i], shape=shape, scale=scale, thres=shift)
            }
)

exp.rt.weibull <- scale * gamma(1+(1/shape)) + shift
exp.var.weibull <- scale^2 * ( gamma(1+ (2/shape)) - gamma(1 + (1/shape))^2 )

numerator <- 2 * (gamma(1+1/shape))^3 - 3*gamma(1+1/shape) * gamma(1+2/shape) + gamma(1+3/shape) 
denominator <- ( gamma(1+2/shape) - (gamma(1+1/shape))^2 )^(3/2)
skew.weibull <- numerator/denominator

# Now put all true means, variances and skewness together ------------
realMeans <- c(exp.rt.norm, exp.rt.exG, exp.rt.wald, exp.rt.weibull)
realVars <- c(exp.var.norm, exp.var.exG, exp.var.wald, exp.var.weibull)
realSkew <- c(0, skew.exG, skew.wald, skew.weibull)

# Take out each data sets (different Ns) sequentially.  
lf <- list()
for(i in 1:length(n)){
  tmp <- data.frame(y0[i], y1[i], y2[i], y3[i]) # sim RTs
  tmp$subj <- rep(1:20, each=NTrials[i])  # subject ID code
  names(tmp) <- c("normal", "exG", "wald", "weibull", "subj")
  lf[[i]] <- tmp
}
names(lf) <- NTrials; 
# str(lf)

# Calculate simulated means ------------------------------------------
# tmp1 is a data frame. The first 4 columns are simulated RTs 
# from each distributions. 5th column is subject ID. 6th column 
# is cell size. 
tmp1 <-ldply(lf, function(x) ddply(x, .(subj), colMeans) )
tmp1$ntrials <- rep(NTrials, each=20)
tmp1 <- tmp1[,-1]

# Calculate simulated Variances -------------------------------------
tmp2 <-ldply(lf, function(x) ddply(x, .(subj), colVars) )
tmp2$ntrials <- rep(NTrials, each=20)
tmp2$subj <- rep(1:20, 10)
tmp2 <- tmp2[,-1]

# Calculate simulated Skewnesses -------------------------------
tmp3 <-ldply(lf, function(x) ddply(x, .(subj), colSkewness) )
tmp3$ntrials <- rep(NTrials, each=20)
tmp3$subj <- rep(1:20, 10)
tmp3 <- tmp3[,-1]

# Bind true values with simulated values ----------------------------
# The last row is the true value. 5th and 6th column of the last row
# contains meaningless value (99, 1000000) for the sake of building
# a data frame
meanMatrix <- rbind(tmp1, c(realMeans, 99, 10^6))
varMatrix <- rbind(tmp2, c(realVars, 99, 10^6))
skewMatrix <- rbind(tmp3, c(realSkew, 99, 10^6))

tmpP2 <- llply(lf, function(x){  
    # 1. cut each list and deal them separately
    ddply(x, .(subj), function(y) { 
    # 2. Within each list, deal with each subject separately
    # 3. Subset the first 4 columns, containing RTs for each simulated
    # distributions
      df <- y[,1:4]
    # 4. do the maximum likelihood estimation for each column
    # using 3-parameter Weibull function
      suppressWarnings(apply(df, 2, thetahat.weibull))
      }
    )
  } 
)


estimateDf2 <- ldply(tmpP2, function(x){
  melt(x, id.vars="subj")
})

# Save as a RData ----------------------------------------------------
estimateDf2$dist <- rep( rep(c("normal", "exG", "wald", "weibull"), each=60), 10)
estimateDf2$para <- rep( rep( rep(c("shape", "scale", "thres"), each=20), 4), 10)
estimateDf2$trial <- estimateDf2$.id
estimateDf2 <- estimateDf2[,-c(1,3)]
save(estimateDf2,lf, file="./data/simData/simulation.RData")
