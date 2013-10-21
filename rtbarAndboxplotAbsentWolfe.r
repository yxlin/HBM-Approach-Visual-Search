#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        21 October, 2013
# Description: These codes use Wolfe et al. (2010) and 
# Palmer et al.'s (2011) data
#-------------------------------------------------------  
rm(list=ls())
load('./data/WolfePalmer/trialAvg.RData')
source("./functions/summarise.R")
source("./functions/multiplot.R")
library(plyr); library(plotrix); library(ggplot2); library(grid)

## --------------------------------
## Preparing for bar plots
## --------------------------------
# use summarySEwithin to compute condition average across particpants
avgRT1 <- summarySEwithin(avg1, measurevar="mean", 
                          withinvars=c("task", "size", 
                                       "target", "task"),
                          idvar="subj")
avgErr1 <- summarySEwithin(avg1, measurevar="errRate", 
                           withinvars=c("task", "size", 
                                        "target", "task"),
                           idvar="subj")

avgRT.Absent <- subset(avgRT1, target=="A")
avgErr.Absent <- subset(avgErr1, target=="A")
pd <- position_dodge(1) 

p1 <- ggplot(avgRT.Absent, 
             aes(x=factor(size), y = mean, fill=task)) + 
  geom_bar(stat="identity", position=pd, colour="black") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.3, position=pd)
barRT <- p1 + theme_bw() + 
  scale_x_discrete(name='Display size') +
  scale_y_continuous(name = "Mean RT (ms)") +
  coord_cartesian(ylim=c(300, 2500)) +
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
  scale_x_discrete(name='Display size') +
  scale_y_continuous(name = "Error rate (%)") +
  coord_cartesian(ylim=c(0, 13)) +
  # scale_fill_grey(name="Task") +
  theme(axis.title.x = element_text(size=20), #blank(), 
        axis.text.x  = element_text(angle=0, size=20), #blank(),  
        axis.title.y = element_text(angle=90, size=20),
        axis.text.y  = element_text(size=20),
        strip.text.x = element_text(size=20, angle=0), #element_blank(), 
        strip.background = element_blank(),
        strip.text.y = element_text(size=20, angle=90),
        legend.position= c(.20, .85),  #'none'
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.key.size = unit(1.2, "lines"),
        legend.key.width = unit(1.2, "cm"))

## --------------------------------
## Preparing for box plots
## --------------------------------
boxRT.Err.Absent <- subset(avg1, target=="P")
p3 <- ggplot(boxRT.Err.Absent, 
             aes(x=factor(size), y = mean, fill=task))   + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(notch=F,  outlier.colour="grey", 
               outlier.size=3)   

boxrt <- p3 + theme_bw() +
  # scale_fill_grey(name="task")+ 
  scale_x_discrete(name='Display size') +
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
  scale_x_discrete(name='Display size') +
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

jpeg(filename = "./figures/rtErrorBoxBarAbsentWolfe.jpeg",
     width = 800, height = 800, units = "px", pointsize = 8,
     quality = 95 ,bg = "white")
multiplot(boxrt, barRT, boxErr, barErr, cols=2); 
dev.off()

