#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        18 October, 2013
# Description: This file draws the bar- and boxplot for the mean
# RTs and error rates at the target absent trials.
#-------------------------------------------------------  
rm(list=ls())
load('./data/mydata/avgFC.RData')
load('./data/mydata/avgS.RData')
source("./functions/summarise.R")
source("./functions/multiplot.R")
library(ggplot2)

## --------------------------------
## Preparing for bar plots
## --------------------------------
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

avgRT0 <- rbind(avgRT1, avgRT2)
avgErr0 <- rbind(avgErr1, avgErr2)
avgRT.Absent <- subset(avgRT0, target=="A")
avgErr.Absent <- subset(avgErr0, target=="A")
pd <- position_dodge(1) 

p1 <- ggplot(avgRT.Absent, 
             aes(x=factor(size), y = mean, fill=task)) + 
  geom_bar(stat="identity", position=pd, colour="black") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.3, position=pd)
barRT <- p1 + theme_bw() + 
  scale_x_discrete(name='Set size') +
  scale_y_continuous(name = "Mean RT (ms)") +
  coord_cartesian(ylim=c(400, 1600)) +
  theme(axis.title.x = element_text(size=20), #blank(), 
        axis.text.x  = element_text(angle=0, size=20), #blank(),  
        axis.title.y = element_text(angle=90, size=20),
        axis.text.y  = element_text(size=20),
        strip.text.x = element_text(size=20, angle=0), #element_blank(), 
        strip.background = element_blank(),
        strip.text.y = element_text(size=20, angle=90),
        legend.position= "none",  # c(.80, .20),  
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.key.size = unit(1.2, "lines"),
        legend.key.width = unit(1.2, "cm"))

p2 <- ggplot(avgErr.Absent, 
             aes(x=factor(size), y = errRate, fill=task)) + 
  geom_bar(stat="identity", position=pd, 
           colour="black") + 
  geom_errorbar(aes(ymin=errRate-se, ymax=errRate+se), 
                width=.3, position=pd)
barErr <- p2 +  theme_bw() + 
  scale_x_discrete(name='Set size') +
  scale_y_continuous(name = "Error rate (%)") +
  coord_cartesian(ylim=c(1, 7.5)) +
  # scale_fill_grey(name="Task") +
  theme(axis.title.x = element_text(size=20), #blank(), 
        axis.text.x  = element_text(angle=0, size=20), #blank(),  
        axis.title.y = element_text(angle=90, size=20),
        axis.text.y  = element_text(size=20),
        strip.text.x = element_text(size=20, angle=0), #element_blank(), 
        strip.background = element_blank(),
        strip.text.y = element_text(size=20, angle=90),
        legend.position= c(.80, .85),  #'none'
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.key.size = unit(1.2, "lines"),
        legend.key.width = unit(1.2, "cm"))

## --------------------------------
## Preparing for box plots
## --------------------------------
boxRT.Err <- rbind(avgFC, avgS)
boxRT.Err.Absent <- subset(boxRT.Err, target=="P")

p3 <- ggplot(boxRT.Err.Absent, 
             aes(x=factor(size), y = mean, fill=task))   + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(notch=F,  outlier.colour="grey", 
               outlier.size=3)   
boxrt <- p3 + theme_bw() +
  # scale_fill_grey(name="task")+ 
  scale_x_discrete(name='Set size') +
  scale_y_continuous(name = "RT (ms)") + 
  theme(axis.title.x = element_text(size=20), #blank(), 
        axis.text.x  = element_text(angle=0, size=20), #blank(),  
        axis.title.y = element_text(angle=90, size=20),
        axis.text.y  = element_text(size=20),
        strip.text.x = element_text(size=20, angle=0), #element_blank(), 
        strip.background = element_blank(),
        strip.text.y = element_text(size=20, angle=90),
        legend.position= "none",# c(.80, .20),  
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.key.size = unit(1.2, "lines"),
        legend.key.width = unit(1.2, "cm"))

p4 <- ggplot(boxRT.Err.Absent, 
             aes(x=factor(size), y = errRate, fill=task))   + 
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot(notch=F,  outlier.colour="grey", 
               outlier.size=3)   
boxErr <- p4 + theme_bw() +
  # scale_fill_grey(name="Task")+ 
  scale_x_discrete(name='Set size') +
  scale_y_continuous(name = "Error rate (%)") + 
  theme(axis.title.x = element_text(size=20), #blank(), 
        axis.text.x  = element_text(angle=0, size=20), #blank(),  
        axis.title.y = element_text(angle=90, size=20),
        axis.text.y  = element_text(size=20),
        strip.text.x = element_text(size=20, angle=0), #element_blank(), 
        strip.background = element_blank(),
        strip.text.y = element_text(size=20, angle=90),
        legend.position= "none",  # c(.80, .20),  
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.key.size = unit(1.2, "lines"),
        legend.key.width = unit(1.2, "cm"))

jpeg(filename = "./figures/rtErrorBoxBarAbsent.jpeg",
     width = 800, height = 800, units = "px", pointsize = 8,
     quality = 95 ,bg = "white")
multiplot(boxrt, barRT, boxErr, barErr, cols=2); 
dev.off()

## --------------------------------
## Separately draw feature search 
## --------------------------------
# Because the RTs in the feature are relatively quick,
# we need to zoom in to the range to see the reliable difference.
avgRT.Feature <- subset(avgRT.Absent, task == "F")
p5 <- ggplot(avgRT.Feature, 
             aes(x=factor(size), y = mean, fill=task)) + 
  geom_bar(stat="identity", position=pd, colour="black") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.4, position=pd, size=2)
barRT.Feature <- p5 + theme_bw() + 
  scale_x_discrete(name='Set size') +
  scale_y_continuous(name = "Mean RT (ms)") +
  coord_cartesian(ylim=c(400, 450)) +
  theme(axis.title.x = element_text(size=20), #blank(), 
        axis.text.x  = element_text(angle=0, size=20), #blank(),  
        axis.title.y = element_text(angle=90, size=20),
        axis.text.y  = element_text(size=20),
        strip.text.x = element_text(size=20, angle=0), #element_blank(), 
        strip.background = element_blank(),
        strip.text.y = element_text(size=20, angle=90),
        legend.position= "none",  # c(.80, .20),  
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.key.size = unit(1.2, "lines"),
        legend.key.width = unit(1.2, "cm"))

jpeg(filename = "./figures/rtBarFeatureAbsent.jpeg",
     width = 800, height = 800, units = "px", pointsize = 8,
     quality = 95 ,bg = "white")
print(barRT.Feature)
dev.off()
