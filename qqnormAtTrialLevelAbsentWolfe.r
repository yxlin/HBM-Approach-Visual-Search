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

## --------------------------------
## Preparing for qqnorm measure
## --------------------------------
trimWolfeData$qqnorm <- qqnorm(trimWolfeData$rt, plot=F)$x
trimWolfeDataA <- subset(trimWolfeData, target == 'A')
size3FT <- subset(trimWolfeDataA, size == 3 & task == "F")
size3CT <- subset(trimWolfeDataA, size == 3 & task == "C")
size3ST <- subset(trimWolfeDataA, size == 3 & task == "S")
size6FT <- subset(trimWolfeDataA, size == 6 & task == "F")
size6CT <- subset(trimWolfeDataA, size == 6 & task == "C")
size6ST <- subset(trimWolfeDataA, size == 6 & task == "S")
size12FT <- subset(trimWolfeDataA, size == 12 & task == "F")
size12CT <- subset(trimWolfeDataA, size == 12 & task == "C")
size12ST <- subset(trimWolfeDataA, size == 12 & task == "S")
size18FT <- subset(trimWolfeDataA, size == 18 & task == "F")
size18CT <- subset(trimWolfeDataA, size == 18 & task == "C")
size18ST <- subset(trimWolfeDataA, size == 18 & task == "S")

## --------------------------------
## Set size 3 
## --------------------------------
s3FTrialP <- ggplot(size3FT, aes(x=qqnorm, y=rt)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s3FTrialAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s3FTrialP; dev.off()

s3CTrialP <- ggplot(size3CT, aes(x=qqnorm, y=rt)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s3CTrialAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s3CTrialP ; dev.off()

s3STrialP <- ggplot(size3ST, aes(x=qqnorm, y=rt)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  
jpeg(filename = "./figures/s3STrialAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s3STrialP ; dev.off()


## --------------------------------
## Set size 6 
## --------------------------------
s6FTrialP <- ggplot(size6FT, aes(x=qqnorm, y=rt)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s6FTrialAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s6FTrialP; dev.off()

s6CTrialP <- ggplot(size6CT, aes(x=qqnorm, y=rt)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  
jpeg(filename = "./figures/s6CTrialAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s6CTrialP ; dev.off()

s6STrialP <- ggplot(size6ST, aes(x=qqnorm, y=rt)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  
jpeg(filename = "./figures/s6STrialAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s6STrialP ; dev.off()


## --------------------------------
## Set size 12 
## --------------------------------
s12FTrialP <- ggplot(size12FT, aes(x=qqnorm, y=rt)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s12FTrialAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s12FTrialP; dev.off()

s12CTrialP <- ggplot(size12CT, aes(x=qqnorm, y=rt)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s12CTrialAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s12CTrialP ; dev.off()

s12STrialP <- ggplot(size12ST, aes(x=qqnorm, y=rt)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  
jpeg(filename = "./figures/s12STrialAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s12STrialP ; dev.off()


## --------------------------------
## Set size 18 
## --------------------------------
s18FTrialP <- ggplot(size18FT, aes(x=qqnorm, y=rt)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  

jpeg(filename = "./figures/s18FTrialAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s18FTrialP; dev.off()

s18CTrialP <- ggplot(size18CT, aes(x=qqnorm, y=rt)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  
jpeg(filename = "./figures/s18CTrialAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s18CTrialP ; dev.off()

s18STrialP <- ggplot(size18ST, aes(x=qqnorm, y=rt)) +  
  geom_point(size=12) + geom_smooth(method="lm", se=FALSE, size=4) +
  theme_bw() + theme_min(size=40) + scale_x_continuous(name="Z-score") +
  scale_y_continuous(name ="RT (ms)")  
jpeg(filename = "./figures/s18STrialAWolfe.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 90, bg = "white")
s18STrialP ; dev.off()
