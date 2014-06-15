# Disclaimer----------------------------------------------------
# Author: Yishin Lin
# Date: 2 Apr, 2014
# Description: Plot simulation data 

# Load data and packages ----------------------------------------
loadedPackages <-c("plyr", "ggplot2", "grid", "reshape2") 
suppressPackageStartupMessages(lapply(loadedPackages, require, character.only=TRUE));
rm(list=ls())
load("./data/simData/simPlot.RData")
source("./functions/summarise.R")
# Plot figures ---------------------------------------------------
bp <- ggplot(biasdfAsubj, aes(x = tn, y = Bias, 
                              colour=Distribution)) + 
  geom_line(size=2.5) + facet_grid(stat~Method, scales="free") 
  
bp <- bp + geom_vline(xintercept = 120, linetype="longdash") +
  geom_vline(xintercept = 170, linetype="dashed") +
  geom_vline(xintercept = 220, linetype="solid") +
  theme_bw()+ 
  scale_colour_grey(start=0, end=.9) +
  scale_x_continuous(breaks=seq(20, 500, 100), name="Cell size") +
  theme(axis.title.x = element_text(size=34), 
        axis.text.x  = element_text(angle=0, size=30), 
        axis.title.y = element_text(angle=90, size=34),
        axis.text.y  = element_text(size=30),
        strip.text.x = element_text(size=30, angle=0),
        strip.text.y = element_text(size=30, angle=90),
        strip.background = element_blank(), 
        panel.border = element_rect(size=3),
        legend.position= c(.90, .88),  
        legend.title = element_text(size=34),
        legend.text = element_text(size=30),
        legend.key.height=unit(3, "line"),
        legend.key.width=unit(3, "line"))

# jpeg(filename = "./figures/simulation/simBias_grey.jpeg",
#      width = 1280, height = 1024, units = "px", pointsize = 8,
#      quality = 95, bg = "white")
bp;
# dev.off()

# line plot ------------------------------------------------------
dfSkew <- subset(biasdf, stat == "Skewness")
dfSkewS <- summarySE(dfSkew, measurevar="Bias", 
                     groupvar=c("Method", "tn", "Distribution"))
names(dfSkewS) <- c("Method", "tn", "Distribution", "N", "Bias",
                    "sd", "se", "ci")
dfSkewS$Distribution <- factor(dfSkewS$Distribution, 
                levels=c('normal','ex-Gaussian','wald', 'weibull'), 
                labels=c('Normal','ex-Gaussian','Wald', 'Weibull'))

skewP <- ggplot(dfSkewS, aes(x=tn, y=Bias, 
                linetype=Method, shape=Method)) + 
         geom_point(size=7) + 
         geom_line(size=2) +  
         facet_grid(.~Distribution) + theme_bw()

skewP <- skewP + scale_x_continuous(name="Cell size") + 
    scale_linetype_manual(values=c(1, 4)) +
    theme(axis.title.x = element_text(size=34), 
          axis.text.x  = element_text(angle=45, 
                      vjust = 1, hjust = 1, size=30), 
          axis.title.y = element_text(angle=90, size=34),
          axis.text.y  = element_text(size=30),
          strip.text.x = element_text(size=30, angle=0),
          strip.text.y = element_text(size=30, angle=90),
          strip.background = element_blank(), 
          legend.position= c(.90, .85),  
          legend.title = element_text(size=34),
          legend.text = element_text(size=30),
          legend.key.height=unit(3.5, "line"),
          legend.key.width=unit(1.5, "cm"))

jpeg(filename = "./figures/simulation/simSkewBias.jpeg",
      width = 1280, height = 1024, units = "px", pointsize = 8,
      quality = 100, bg = "white")
skewP; 
dev.off()