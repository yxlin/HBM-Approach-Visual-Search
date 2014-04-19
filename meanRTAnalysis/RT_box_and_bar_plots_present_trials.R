#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        18 October, 2013
# Description: This codes draws the bar- and box-plots for the mean
# RTs and mean error rates at the target present trials.
rm(list=ls())
load('./data/myData/avgFC.RData')
load('./data/myData/avgS.RData')
source("./functions/summarise.R")
source("./functions/multiplot.R")
library(ggplot2); library(grid)

# --------------------------------
# Preparing for bar plots
# use summarySEwithin to compute condition average across particpants
avgRT1 <- summarySEwithin(avgFC, measurevar="mean", 
                          withinvars=c("task", "size", "target"),
                          idvar="subj")
avgRT2 <- summarySEwithin(avgS, measurevar="mean", 
                          withinvars=c("size", "target"),
                          betweenvars="task",
                          idvar="subj")
avgErr1 <- summarySEwithin(avgFC, measurevar="errRate", 
                           withinvars=c("task", "size", "target"),
                           idvar="subj")
avgErr2 <- summarySEwithin(avgS, measurevar="errRate", 
                           withinvars=c("size", "target"),
                           betweenvars="task",
                           idvar="subj")

featP <- subset(avgRT1, task == "F" & target == "P")
conjP <- subset(avgRT1, task == "C" & target == "P")
spatP <- subset(avgRT2, task == "S" & target == "P")


# Show descriptive statistics ---------------------------------------------
mean(featP$mean / c(3,6,12,18))  # 67.43036
mean(conjP$mean / c(3,6,12,18)) # 87.55613
mean(spatP$mean / c(3,6,12,18)) # 127.4212

avgRT0 <- rbind(avgRT1, avgRT2)
avgErr0 <- rbind(avgErr1, avgErr2)
avgRT.Present <- subset(avgRT0, target=="P")
avgErr.Present <- subset(avgErr0, target=="P")

ddply(avgErr.Present, .(task), summarise,
      meanErr = mean(errRate))
# 11.80-8.62
# 8.62-5


# Plot figures ------------------------------------------------------------
pd <- position_dodge(1) 
p1 <- ggplot(avgRT.Present, 
                aes(x=factor(size), y = mean, fill=task)) + 
                geom_bar(stat="identity", position=pd) + 
                geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                              width=.3, position=pd)
barRT <- p1 + theme_bw() + 
  scale_x_discrete(name='Display size') +
  scale_y_continuous(name = "Mean RT (ms)") +
  coord_cartesian(ylim=c(400, 1200)) +
  theme(axis.title.x = element_blank(), #text(size=20), #blank(), 
        axis.text.x  = element_blank(), #text(angle=0, size=20), #blank(),  
        axis.title.y = element_text(angle=90, size=36),
        axis.text.y  = element_text(size=34),
        legend.position= "none")

p2 <- ggplot(avgErr.Present, 
            aes(x=factor(size), y = errRate, fill=task)) + 
            geom_bar(stat="identity", position=pd) + 
            geom_errorbar(aes(ymin=errRate-se, ymax=errRate+se), 
                          width=.3, position=pd)
barErr <- p2 +  theme_bw() + 
  scale_x_discrete(name='Display size') +
  scale_y_continuous(name = "Mean error rate (%)") +
  coord_cartesian(ylim=c(3, 25)) +
  theme(axis.title.x = element_text(size=36), #blank(), 
        axis.text.x  = element_text(angle=0, size=34), #blank(),  
        axis.title.y = element_text(angle=90, size=36),
        axis.text.y  = element_text(size=34),
        legend.position= c(.20, .85),  #'none'
        legend.title = element_text(size=24),
        legend.text = element_text(size=24),
        legend.key.size = unit(1.2, "lines"),
        legend.key.width = unit(1.2, "cm"))

## --------------------------------
## Preparing for box plots
## --------------------------------
boxRT.Err <- rbind(avgFC, avgS)
boxRT.Err.Present <- subset(boxRT.Err, target=="P")

p3 <- ggplot(boxRT.Err.Present, 
                aes(x=factor(size), y = mean, fill=task))   + 
                stat_boxplot(geom ='errorbar') +
                geom_boxplot(notch=F,  outlier.colour="grey", 
                             outlier.size=3)   
boxrt <- p3 + theme_bw() +
  scale_x_discrete(name='Display size') +
  scale_y_continuous(name = "RT (ms)") + 
  theme(axis.title.x = element_blank(), #text(size=20), #blank(), 
        axis.text.x  = element_blank(), #text(angle=0, size=20), #blank(),  
        axis.title.y = element_text(angle=90, size=36),
        axis.text.y  = element_text(size=34),
        legend.position= "none")

p4 <- ggplot(boxRT.Err.Present, 
            aes(x=factor(size), y = errRate, fill=task))   + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot(notch=F,  outlier.colour="grey", 
                         outlier.size=3)   
boxErr <- p4 + theme_bw() +
  scale_x_discrete(name='Display size') +
  scale_y_continuous(name = "Error rate (%)") + 
  theme(axis.title.x = element_text(size=36), #blank(), 
        axis.text.x  = element_text(angle=0, size=34), #blank(),  
        axis.title.y = element_text(angle=90, size=36),
        axis.text.y  = element_text(size=34),
        legend.position= "none")  

# jpeg(filename = "./figures/boxplots_and_barplots_RT_presentTrial.jpeg",
#      width = 1280, height = 1024, units = "px", pointsize = 8,
#      quality = 95 ,bg = "white")
multiplot(boxrt, barRT, boxErr, barErr, cols=2); 
# dev.off()

## --------------------------------
## Separately draw feature search 
## --------------------------------
# Because the RTs in the feature are relatively quicker than other
# condition, we need to zoom in to the range to see the reliable 
# difference (ANOVA does show significant effect).
avgRT.Feature <- subset(avgRT.Present, task == "F")
p5 <- ggplot(avgRT.Feature, 
             aes(x=factor(size), y = mean, fill=task)) + 
  geom_bar(stat="identity", position=pd, colour="black") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.4, position=pd, size=2)
barRT.Feature <- p5 + theme_bw() + 
  scale_x_discrete(name='Display size') +
  scale_y_continuous(name = "Mean RT (ms)") +
  coord_cartesian(ylim=c(400, 450)) +
  theme(axis.title.x = element_text(size=40), #blank(), 
        axis.text.x  = element_text(angle=0, size=34), #blank(),  
        axis.title.y = element_text(angle=90, size=40),
        axis.text.y  = element_text(size=34),
        legend.position= "none")

# jpeg(filename = "./figures/rtBarFeaturePresent.jpeg",
#      width = 1024, height = 768, units = "px", pointsize = 8,
#      quality = 95 ,bg = "white")
print(barRT.Feature)
# dev.off()
