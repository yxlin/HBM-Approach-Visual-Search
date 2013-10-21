#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        20 October, 2013
# Description: This file qqnorm for each cell at RT mean 
# level
#-------------------------------------------------------  
rm(list=ls())
load('./data/mydata/avgFC.RData')
load('./data/mydata/avgS.RData')
source('./functions/themeFuns.R')
library(ggplot2); library(grid)

## --------------------------------
## Preparing for bar plots
## --------------------------------
avg1 <- rbind(avgFC, avgS)
avg1$task <- factor(avg1$task, levels=c('F','C','S'),
                    labels=c('F','C','S'))
avg2 <- subset(avg1, target == 'P')

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
s3FP <- ggplot(size3F, aes(x=qqnorm, y=mean)) +  
    geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
    theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
    scale_y_continuous(name ="RT (ms)")  
  
jpeg(filename = "./figures/s3FP.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s3FP; dev.off()
  
s3CP <- ggplot(size3C, aes(x=qqnorm, y=mean)) +  
   geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
   theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
   scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s3CP.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s3CP ; dev.off()

s3SP <- ggplot(size3S, aes(x=qqnorm, y=mean)) +  
   geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
   theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
   scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s3SP.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s3SP ; dev.off()


## --------------------------------
## Use set size 6
## --------------------------------
s6FP <- ggplot(size6F, aes(x=qqnorm, y=mean)) +  
    geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
    theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
    scale_y_continuous(name ="RT (ms)")  
  
jpeg(filename = "./figures/s6FP.jpeg",
       width = 600, height = 600, units = "px", pointsize = 8,
       quality = 90, bg = "white")
s6FP; dev.off()
  
s6CP <- ggplot(size6C, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s6CP.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s6CP; dev.off()

s6SP <- ggplot(size6S, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s6SP.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s6SP; dev.off()

## --------------------------------
## Use set size 12
## --------------------------------
s12FP <- ggplot(size12F, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  
 
jpeg(filename = "./figures/s12FP.jpeg",
       width = 600, height = 600, units = "px", pointsize = 8,
       quality = 90, bg = "white")
s12FP; dev.off()
  
s12CP <- ggplot(size12C, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s12CP.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s12CP ; dev.off()

s12SP <- ggplot(size12S, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s12SP.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s12SP ; dev.off()


## --------------------------------
## Use set size 18
## --------------------------------
s18FP <- ggplot(size18F, aes(x=qqnorm, y=mean)) +  
    geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
    theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
    scale_y_continuous(name ="RT (ms)")  
  
jpeg(filename = "./figures/s18FP.jpeg",
       width = 600, height = 600, units = "px", pointsize = 8,
       quality = 90, bg = "white")
s18FP; dev.off()

s18CP <- ggplot(size18C, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s18CP.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s18CP ; dev.off()

s18SP <- ggplot(size18S, aes(x=qqnorm, y=mean)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s18SP.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s18SP ; dev.off()


