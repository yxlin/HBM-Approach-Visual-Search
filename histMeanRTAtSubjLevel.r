#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        20 October, 2013
# Description: This file draws histogram overplotted with 
# density plot using RT mean per cell at the participant 
# level
#-------------------------------------------------------  
rm(list=ls())
load('./data/mydata/avgFC.RData')
load('./data/mydata/avgS.RData')
library(ggplot2); library(grid)

## --------------------------------
## Preparing for bar plots
## --------------------------------
avg1 <- rbind(avgFC, avgS)
avg1$task <- factor(avg1$task, levels=c('F','C','S'),
      labels=c('F','C','S'))
avg1$target <- factor(avg1$target, levels=c('P','A'),
                    labels=c('present','absent'))

## --------------------------------
## Use ggplot to contruct the plots
## --------------------------------
baseP <- ggplot(avg1, aes(x=mean)) +   
         geom_histogram(aes(y = ..density..), binwidth=80, 
                        fill="grey", colour="black") + 
         geom_density(alpha = 0.2)  + 
         facet_grid(size~task*target, scales="fixed", space="fixed")
m <- baseP +  theme_bw() +
    coord_cartesian(xlim = c(0, 3000)) +
    scale_x_continuous(breaks=seq(0, 5000, 800), name="Mean RT (ms)") + 
    scale_y_continuous(breaks=seq(0, 1, .004), name ="Density") + 
    theme(axis.title.x = element_text(size=20), #blank(), 
     axis.text.x  = element_text(angle=90, size=20), #blank(),  
     axis.title.y = element_text(angle=90, size=20),
     axis.text.y  = element_text(size=20),
     strip.text.x = element_text(size=20, angle=0), #element_blank(), 
     strip.background = element_blank(),
     strip.text.y = element_text(size=20, angle=90))

## --------------------------------
## Save output to a jpg file
## --------------------------------
jpeg(filename = "./figures/histSubjLevel.jpeg",
     width = 800, height = 600, units = "px", pointsize = 8,
     quality = 95, bg = "white")
m; dev.off()

