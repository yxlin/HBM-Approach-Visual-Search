# Disclaimer----------------------------------------------------
# Author: Yishin Lin
# Date: 2 Apr, 2014
# Description: Plot goodness-of-fit data 
rm(list=ls())

# Load packages, functions and data--------------------------------
loadedPackages <-c("plyr","ggplot2", "grid", "FAdist", "reshape2") 
suppressPackageStartupMessages(lapply(loadedPackages, require, character.only=TRUE));
source("./Functions/summarise.R")
load("./data/gofData/gofstatsS.RData")
gofstatsS <- gofstats
load("./data/gofData/gofstatsC.RData")
gofstatsC <- gofstats
load("./data/gofData/gofstatsF.RData")
gofstatsF <- gofstats

gofstatsS$task <- "S"; gofstatsC$task <- "C"; gofstatsF$task <- "F"
gofstats <- rbind(gofstatsS, gofstatsC, gofstatsF)

# Calculate Anderson Darling statistics ---------------------------
tmp <- gofstats[,c(1,5,7,8,9, 10)]

tmp$method <- as.character(tmp$method)
tmp$subj <- as.character(tmp$subj)
tmp$size <- as.character(tmp$size)
tmp$size <- factor(tmp$size, levels=c('3','6', '12', '18'),
                   labels=c('3','6', '12', '18'))


gofSum <- summarySEwithin(tmp, measurevar = 'ad', withinvar=c('size'), betweenvars=c('method', "task"), idvar='subj', na.rm=FALSE, conf.interval=.95, .drop=TRUE)

# Plot figures ----------------------------------------------------
pAD <- ggplot(gofSum, aes(x=size, y=ad, linetype=method, 
                          ymin=ad-se, ymax=ad+se)) +
  geom_errorbar(width=.5, size=2) +
  geom_line(position=position_dodge(), 
            aes(group=method), size=2)+
  theme_bw() + facet_grid(.~task) +
  scale_linetype_manual(values=c(1, 6)) +
  scale_x_discrete(name='Display size') +
  scale_y_continuous(name = "Goodness-of-fit statistic") + 
  coord_cartesian(ylim=c(25, 110)) + 
  theme(axis.title.x = element_text(size=34), 
        axis.text.x  = element_text(angle=0, size=30), 
        axis.title.y = element_text(angle=90, size=34),
        axis.text.y  = element_text(size=30),
        strip.text.x = element_text(size=30, angle=0), 
        strip.background = element_blank(),
        strip.text.y = element_text(size=30, angle=90),
        legend.position= c(.20, .93),  
        legend.title = element_text(size=34),
        legend.text = element_text(size=30),
        legend.key.size = unit(3.5, "lines"),
        legend.key.width = unit(1.5, "cm"))


jpeg(filename = "./figures/adplotLine.jpeg",
     width = 1280, height = 1024, units = "px", pointsize = 8,
     quality = 100, bg = "white")
pAD;
dev.off()

# Colour figures---------------------------------------------------
# pAD <- ggplot(gofSum, aes(x=size, y=ad, fill=method)) +
#   geom_bar(position=position_dodge(), stat='identity') +
#   geom_errorbar(aes(ymin=ad-se, ymax=ad+se),  
#                 width=.2, position=position_dodge(.9)) +
#   theme_bw() + facet_grid(.~task) +
#   scale_fill_manual(values=c("#999999", "#E69F00")) +
#   scale_x_discrete(name='Set size') +
#   scale_y_continuous(name = "Anderson-Darling statistic") + 
#   coord_cartesian(ylim=c(25, 110)) + 
#   theme(axis.title.x = element_text(size=20), #blank(), 
#         axis.text.x  = element_text(angle=0, size=20), #blank(),  
#         axis.title.y = element_text(angle=90, size=20),
#         axis.text.y  = element_text(size=20),
#         strip.text.x = element_text(size=20, angle=0), #element_blank(), 
#         strip.background = element_blank(),
#         strip.text.y = element_text(size=20, angle=90),
#         legend.position= c(.20, .93),  #'none'
#         legend.title = element_text(size=20),
#         legend.text = element_text(size=20),
#         legend.key.size = unit(1.2, "lines"),
#         legend.key.width = unit(1.2, "cm"))
