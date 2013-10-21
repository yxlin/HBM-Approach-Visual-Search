#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        21 October, 2013
# Description: These codes use Wolfe et al. (2010) and 
# Palmer et al.'s (2011) data
#-------------------------------------------------------  
rm(list=ls())
load('./data/WolfePalmer/trialAvg.RData')
source('./functions/themeFuns.R')

library(ggplot2); library(grid)
avg1$target <- factor(avg1$target, levels=c('P','A'),
                      labels=c('P','A'))
avg1$task <- factor(avg1$task, levels=c('F','C','S'),
                    labels=c('F','C','S'))
avg2 <- subset(avg1, target == 'A')

## --------------------------------
## Use qqnorm and ggplot to contruct the plots
## --------------------------------
avg2$qqnorm <- qqnorm(avg2$mean, plot=F)$x
size3F <- subset(avg2, size == 3 & task == "F")
size3C <- subset(avg2, size == 3 & task == "C")
size3S <- subset(avg2, size == 3 & task == "S")
size6F <- subset(avg2, size == 6 & task == "F")
size6C <- subset(avg2, size == 6 & task == "C")
size6S <- subset(avg2, size == 6 & task == "S")
size12F <- subset(avg2, size == 12 & task == "F")
size12C <- subset(avg2, size == 12 & task == "C")
size12S <- subset(avg2, size == 12 & task == "S")
size18F <- subset(avg2, size == 18 & task == "F")
size18C <- subset(avg2, size == 18 & task == "C")
size18S <- subset(avg2, size == 18 & task == "S")

## --------------------------------
## Use set size 3
## --------------------------------
# x is standard normal
# y is the original values
s3FA <- ggplot(size3F, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s3FAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s3FA; dev.off()

s3CA <- ggplot(size3C, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s3CAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s3CA ; dev.off()

s3SA <- ggplot(size3S, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s3SAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s3SA ; dev.off()

## --------------------------------
## Use set size 6
## --------------------------------
s6FA <- ggplot(size6F, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s6FAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s6FA; dev.off()

s6CA <- ggplot(size6C, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s6CAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s6CA; dev.off()

s6SA <- ggplot(size6S, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s6SAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s6SA; dev.off()

## --------------------------------
## Use set size 12
## --------------------------------
s12FA <- ggplot(size12F, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s12FAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s12FA; dev.off()

s12CA <- ggplot(size12C, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s12CAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s12CA ; dev.off()

s12SA <- ggplot(size12S, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s12SAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s12SA ; dev.off()


## --------------------------------
## Use set size 18
## --------------------------------
s18FA <- ggplot(size18F, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s18FAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s18FA; dev.off()

s18CA <- ggplot(size18C, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s18CAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s18CA ; dev.off()

s18SA <- ggplot(size18S, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s18SAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s18SA ; dev.off()
