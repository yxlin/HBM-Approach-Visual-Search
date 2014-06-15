# Disclaimer----------------------------------------------------
# Author: Yishin Lin
# Date: 2 Apr, 2014
# Description: Plot simulation data 

loadedPackages <-c("plyr", "ggplot2", "grid") 
suppressPackageStartupMessages(lapply(loadedPackages, require, character.only=TRUE));
rm(list=ls())
library(plyr);library(ggplot2);library(grid)

load("./data/diagData/gewekeDS.RData")
load("./data/diagData/heidelDS.RData")
load("./data/diagData/rafteryDS.RData")
load("./data/diagData/gelmanDS.RData")

load("./data/diagData/gewekeDC.RData")
load("./data/diagData/heidelDC.RData")
load("./data/diagData/rafteryDC.RData")
load("./data/diagData/gelmanDC.RData")

load("./data/diagData/gewekeDF.RData")
load("./data/diagData/heidelDF.RData")
load("./data/diagData/rafteryDF.RData")
load("./data/diagData/gelmanDF.RData")

source("./functions/nonparadiag.R")

diagF <- nonparadiag(gewekeDF, heidelDF, gelmanDF, rafteryDF)
diagC <- nonparadiag(gewekeDC, heidelDC, gelmanDC, rafteryDC)
diagS <- nonparadiag(gewekeD, heidelD, gelmanD, rafteryD)
diagF[[5]]$task <- "F"
diagC[[5]]$task <- "C"
diagS[[5]]$task <- "S"

tmp <- rbind(diagF[[5]], diagC[[5]], diagS[[5]])

vlineDfF <- data.frame(diag= c("Geweke z", "Geweke z", "H-W p", "Gelman-Rubin R"), vl=c(1.96, -1.96, .05, mean(diagF[[3]]$upper.ci)))

vlineDfC <- data.frame(diag= c("Geweke z", "Geweke z", "H-W p", "Gelman-Rubin R"), vl=c(1.96, -1.96, .05, mean(diagC[[3]]$upper.ci)))

vlineDfS <- data.frame(diag= c("Geweke z", "Geweke z", "H-W p", "Gelman-Rubin R"), vl=c(1.96, -1.96, .05, mean(diagS[[3]]$upper.ci)))

vlineDfF$task <- "F"
vlineDfC$task <- "C"
vlineDfS$task <- "S"
vlineDf <- rbind(vlineDfF, vlineDfC, vlineDfS)

diagP <- ggplot(tmp, aes(x= factor(size), y = refvalue)) +
  geom_boxplot(notch=F,  outlier.colour="grey", outlier.size=3) +   
  geom_hline(aes(yintercept=vl), data=vlineDf, 
             colour="black", linetype="longdash") +
  facet_grid(diag~task, scale="free") + theme_bw() +
  scale_x_discrete(name='Display size') +
  scale_y_continuous(name = "Diagnostic Statistics") + 
  theme(axis.title.x = element_text(size=30),  
        axis.text.x  = element_text(angle=0, size=26), 
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y  = element_text(size=26),
        strip.text.x = element_text(size=30, angle=0), 
        strip.background = element_blank(),
        strip.text.y = element_text(size=30, angle=90),
        legend.position= 'none', #c(.85, .90),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.key.size = unit(1.2, "lines"),
        legend.key.width = unit(1.2, "cm"))
diagP

jpeg(filename = "../figures/diagAll.jpeg",
     width = 1024, height = 768, units = "px", pointsize = 8,
     quality = 95,bg = "white")
diagP; dev.off()
