#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        18 October, 2013
# Description: This script draws boxplots for  the mean 
#              RTs and error rates at the target present trials.
# Revision:    11 June, 2014
# Note:        Discard reduntant figures (barplots) and spaces
#              between sub-panels
rm(list=ls())
load('./data/WolfePalmer/trialAvg.RData')
load('./data/myData/avgFC.RData')
load('./data/myData/avgS.RData')
source("./functions/switch_facet_strip.R")
library(ggplot2);library(grid); library(reshape2);library(gtable)

# Bind mean RTs and error rates as long form ---------------------
boxRTErr1l <- subset(rbind(avgFC, avgS), target=="P")
tmp1 <- subset(boxRTErr1l, select=c("size","task",
                                   "subj", "mean"))
tmp1$para <- "mean"
tmp2 <- subset(boxRTErr1l, select=c("size","task",
                                   "subj", "errRate"))
tmp2$para <- "errRate"
names(tmp1) <- c("size","task","subj","value","para")
names(tmp2) <- c("size","task","subj","value","para")
boxRTErr2l <- rbind(tmp1,tmp2)
boxRTErr2l$paraName <- ifelse(boxRTErr2l$para == "mean", 
                             "Correct Mean RTs (ms)", 
                             "Mean error rates (%)")

# Wolfe et al., (2010)
boxRTErr1w <- subset(avg1, target=="P")
tmp1 <- subset(boxRTErr1w, select=c("size","task",
                                   "subj", "mean"))
tmp1$para <- "mean"
tmp2 <- subset(boxRTErr1w, select=c("size","task",
                                   "subj", "errRate"))
tmp2$para <- "errRate"
names(tmp1) <- c("size","task","subj","value","para")
names(tmp2) <- c("size","task","subj","value","para")
boxRTErr2w <- rbind(tmp1,tmp2)
boxRTErr2w$paraName <- ifelse(boxRTErr2w$para == "mean", 
                             "Correct Mean RTs (ms)", 
                             "Mean error rates (%)")
boxRTErr2l$dataset <- "Lin, Heinke & Humphreys (2014)"
boxRTErr2w$dataset <- "Wolfe, Palmer & Horowitz (2010)"
boxRTErr2 <- rbind(boxRTErr2l, boxRTErr2w)

# Plot figures -------------------------------------------------
# My data
p0 <- ggplot(boxRTErr2, aes(x=factor(size), 
                            y = value, fill=task))   + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(notch=F,  outlier.colour="grey", 
               outlier.size=3) +
  facet_grid(paraName~dataset, scales="free_y") +
  theme_bw() +
  scale_x_discrete(name='Display size') +
  scale_fill_grey(start=0, end=.9) +
  theme(axis.title.x = element_text(size=34),
        axis.text.x  = element_text(size=30), 
        axis.title.y = element_blank(), 
        axis.text.y  = element_text(size=30),
        strip.text.x = element_text(size=34),  
        strip.text.y = element_text(size=34),
        strip.background = element_blank(),
        legend.position= c(.10, .35),
        legend.title = element_text(size=30),
        legend.text = element_text(size=30),
        legend.key.size = unit(2, "lines"),
        legend.key.width = unit(1.5, "cm"))


jpeg(filename = "./figures/boxAndBarPlots/boxplots_RT_presentTrial_Lin_and_Wolfe_greys.jpeg",width = 1280, height = 1024, units = "px", pointsize = 8,quality = 95 ,bg = "white")
switch_facet_strip(p0, switch = "y")
dev.off()

