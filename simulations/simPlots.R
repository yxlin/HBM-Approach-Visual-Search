# Load packages ----------------------------------------------------
loadedPackages <-c("plyr", "ggplot2", "grid", "reshape2") 
suppressPackageStartupMessages(lapply(loadedPackages, require, character.only=TRUE));
rm(list=ls())

# True values -------------------------------------------------------
# Gaussian 
mu <- 1000; sigma <- 100
tMnorm <- mu/1000 
tVnorm <- (sigma/1000)^2 
tSnorm <- 0

# exGaussian 
mu <- 910.56/1000; tau <- 89.443/1000; sigma <- 44.721/1000
tMexG <- (mu + tau)
tVexG <- sigma^2 + tau^2
tSexG <- (2*tau^3) / ((sigma^2+tau^2)^(3/2))

# inverse Gaussian, or called Wald
mu <- 275/1000; lambda <- 2000/1000; kappa <- 725/1000
tMW <- mu + kappa
tVW <- (mu^3) / lambda
tSW <- 3*sqrt(mu/lambda)

# three-parameter Weibull 
# shape = gamma (or a), scale = beta (or b) and thres=shift=alpha (or m)
shape <- 2.0  # gamma
scale <- 220/1000   # beta
shift <- 800/1000  # alpha

# Convert to mean, variance and skewness-----------------------------
tMWb <- scale * gamma(1+(1/shape)) + shift
tVWb <- scale^2 * ( gamma(1+ (2/shape)) - gamma(1 + (1/shape))^2 )
#sqrt(exp.var.weibull)
numerator <- 2 * (gamma(1+1/shape))^3 - 3*gamma(1+1/shape) * gamma(1+2/shape) + gamma(1+3/shape) 
denominator <- ( gamma(1+2/shape) - (gamma(1+1/shape))^2 )^(3/2)
tSWb <- numerator/denominator

# Load data ----------------------------------------------------------
load("./data/simData/simWB.RData")
load("./data/simData/simulation.RData")
source("./functions/summarise.R")

simDesc <- ddply(sim, .(trialn, dist, subj), summarise,
                 m.rt=scale * gamma(1 + 1/shape) + shift, 
                 v.rt=scale^2 * ( gamma(1+2/shape) - gamma(1+1/shape)^2 ),
                 s.rt = ( 2 * (gamma(1+1/shape))^3 - 3*gamma(1+1/shape) * 
                            gamma(1+2/shape) + gamma(1+3/shape) ) / 
                   ((gamma(1+2/shape) - (gamma(1+1/shape))^2 )^(3/2))
)

simN <- subset(simDesc, dist == "normal")
simexG <- subset(simDesc, dist == "exG")
simW <- subset(simDesc, dist == "wald")
simWb <- subset(simDesc, dist == "weibull")

# Calculate bias scores ---------------------------------------------
mbias.n <- abs(simN$m.rt -  tMnorm)
vbias.n <- abs(simN$v.rt -  tVnorm)
sbias.n <- abs(simN$s.rt -  0)

mbias.exG <- abs(simexG$m.rt -  tMexG)
vbias.exG <- abs(simexG$v.rt -  tVexG)
sbias.exG <- abs(simexG$s.rt -  tSexG) 

mbias.W <- abs(simW$m.rt -  tMW)
vbias.W <- abs(simW$v.rt -  tVW)
sbias.W <- abs(simW$s.rt -  tSW) 

mbias.Wb <- abs(simWb$m.rt -  tMWb)
vbias.Wb <- abs(simWb$v.rt -  tVWb)
sbias.Wb <- abs(simWb$s.rt -  tSWb) 


biasdfBHM <- data.frame(Bias = c(mbias.n, mbias.exG, mbias.W, mbias.Wb,vbias.n, vbias.exG, vbias.W, vbias.Wb,sbias.n, sbias.exG, sbias.W, sbias.Wb),tn = rep(simN$trialn, 12),subj = rep(simN$subj, 12),Distribution = rep( rep(c("normal", "ex-Gaussian", "wald", "weibull"), each=200), 3),stat = rep( c("Mean", "Variance", "Skewness"), each=800))

mleDf <- as.data.frame(estimateDf2)
mleDf$trialn <- as.numeric(mleDf$trial)

mleavg <- dcast(mleDf, dist + trialn + subj ~ para, value.var="value")

mleDesc <- ddply(mleavg, .(trialn, dist, subj), summarise,
                 m.rt =( scale * gamma(1 + 1/shape) + thres ) / 1000, 
                 v.tmp = scale^2 * ( gamma(1+2/shape) - gamma(1+1/shape)^2 ),
                 s.rt = ( 2 * (gamma(1+1/shape))^3 - 3*gamma(1+1/shape) * 
                            gamma(1+2/shape) + gamma(1+3/shape) ) / 
                   ((gamma(1+2/shape) - (gamma(1+1/shape))^2 )^(3/2))
)
mleDesc$v.rt <- (sqrt(mleDesc$v.tmp)/1000)^2

mleN <- subset(mleDesc, dist == "normal")
mleexG <- subset(mleDesc, dist == "exG")
mleW <- subset(mleDesc, dist == "wald")
mleWb <- subset(mleDesc, dist == "weibull")


mbias.n <- abs(mleN$m.rt -  tMnorm)
vbias.n <- abs(mleN$v.rt -  tVnorm)
sbias.n <- abs(mleN$s.rt -  0)

mbias.exG <- abs(mleexG$m.rt -  tMexG)
vbias.exG <- abs(mleexG$v.rt -  tVexG)
sbias.exG <- abs(mleexG$s.rt -  tSexG) 

mbias.W <- abs(mleW$m.rt -  tMW)
vbias.W <- abs(mleW$v.rt -  tVW)
sbias.W <- abs(mleW$s.rt -  tSW) 

mbias.Wb <- abs(mleWb$m.rt -  tMWb)
vbias.Wb <- abs(mleWb$v.rt -  tVWb)
sbias.Wb <- abs(mleWb$s.rt -  tSWb) 


biasdfMLE <- data.frame(Bias = c(mbias.n, mbias.exG, mbias.W, mbias.Wb,
                                 vbias.n, vbias.exG, vbias.W, vbias.Wb,
                                 sbias.n, sbias.exG, sbias.W, sbias.Wb),
                        tn = rep(mleN$trial, 12),
                        subj = rep(simN$subj, 12),
                        Distribution = rep( rep(c("normal", "ex-Gaussian", 
                                                  "wald", "weibull"), each=200), 3),
                        stat = rep( c("Mean", "Variance", "Skewness"), each=800))

biasdfMLE$Method <- "MLE" 
biasdfBHM$Method <- "HBM"
biasdf <- rbind(biasdfMLE, biasdfBHM)

tDf <- ddply(biasdf, .(Distribution, stat, tn), summarise,
             var.p = var.test(Bias~Method)$p.value,
             ttest.p = t.test(Bias~Method, paired=F, var.equal=F)$p.value,
             BHM.mean = t.test(Bias~Method, paired=F, var.equal=F)$estimate[1],
             MLE.mean = t.test(Bias~Method, paired=F, var.equal=F)$estimate[2]
)

biasdfAsubj <- summarySE(biasdf, measurevar = "Bias", groupvars=c("tn", "Distribution", "stat", "Method"))
biasdfAsubj$Distribution <- factor(biasdfAsubj$Distribution, 
              levels=c('normal','ex-Gaussian','wald', 'weibull'), 
              labels=c('Normal','ex-Gaussian','Wald', 'Weibull'))

bp <- ggplot(biasdfAsubj, aes(x = tn, y = Bias, colour=Distribution)) + 
  geom_line(size=2.5) + facet_grid(stat~Method, scales="free")
bp <- bp + geom_vline(xintercept = 120, linetype="longdash", color="bisque4") +
  geom_vline(xintercept = 170, linetype="longdash", color="brown2") +
  geom_vline(xintercept = 220, linetype="longdash", color="black") +
  scale_colour_brewer(type="div") +
  scale_x_continuous(breaks=seq(20, 500, 100), name="Cell size") +
  theme(axis.title.x = element_text(size=24), #element_blank(), 
        axis.text.x  = element_text(angle=0, size=22), #element_blank()
        axis.title.y = element_text(angle=90, size=24),
        axis.text.y  = element_text(size=22),
        strip.text.x = element_text(size=24, angle=0),
        strip.text.y = element_text(size=24, angle=90),
        strip.background = element_blank(), 
        legend.position= c(.90, .88),  
        legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        legend.key.height=unit(3, "line"),
        legend.key.width=unit(3, "line"))

jpeg(filename = "./figures/simulation/simBias.jpeg",
     width = 1280, height = 1024, units = "px", pointsize = 8,
     quality = 100, bg = "white")
bp;
dev.off()

# Comparing HBM to MLE------------------------------------------
dfSkew <- subset(biasdf, stat == "Skewness")
dfSkewS <- summarySE(dfSkew, measurevar="Bias", groupvar=c("Method", "tn", "Distribution"))
names(dfSkewS) <- c("Method", "tn", "Distribution", "N", "Bias",
                    "sd", "se", "ci")
dfSkewS$Distribution <- factor(dfSkewS$Distribution, 
                                   levels=c('normal','ex-Gaussian','wald', 'weibull'), 
                                   labels=c('Normal','ex-Gaussian','Wald', 'Weibull'))


skewP <- ggplot(dfSkewS, aes(x=tn, y=Bias, 
                             linetype=Method, shape=Method)) + 
  geom_point(size=7) + geom_line(size=2) +  facet_grid(.~Distribution)
skewP <- skewP + scale_x_continuous(name="Cell size") + 
    scale_linetype_manual(values=c(1, 4)) +
    theme(axis.title.x = element_text(size=30),  
          axis.text.x  = element_text(angle=45, vjust = 1, hjust = 1, size=26), 
          axis.title.y = element_text(angle=90, size=30),
          axis.text.y  = element_text(size=26),
          strip.text.x = element_text(size=30, angle=0),
          strip.text.y = element_text(size=30, angle=90),
          strip.background = element_blank(), 
          legend.position= c(.90, .91),  
          legend.title = element_text(size=26),
          legend.text = element_text(size=24),
          legend.key.height=unit(3, "line"),
          legend.key.width=unit(3, "line"))

jpeg(filename = "./figures/simulation/simSkewBias.jpeg",
      width = 1280, height = 1024, units = "px", pointsize = 8,
       quality = 95, bg = "white")
skewP; 
dev.off()

