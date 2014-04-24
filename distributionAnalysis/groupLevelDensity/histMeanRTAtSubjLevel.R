#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        20 October, 2013
# Description: This file draws histogram overplotted with 
# density plot using RT mean per cell at the participant 
# level
rm(list=ls())
load('./data/mydata/avgFC.RData')
load('./data/mydata/avgS.RData')
library(ggplot2); library(grid)

## Preparing for bar plots--------------------------------
avg1 <- rbind(avgFC, avgS)
avg1$task <- factor(avg1$task, levels=c('F','C','S'),
      labels=c('F','C','S'))
avg1$target <- factor(avg1$target, levels=c('P','A'))
avg1$taskTgt <- paste0(avg1$task,avg1$target)
avg1$taskTgt <- factor(avg1$taskTgt, levels=c('FP','FA', 
                                              'CP', 'CA',
                                              'SP', 'SA'))

## Plot figures--------------------------------
baseP <- ggplot(avg1, aes(x=mean)) +   
         geom_histogram(aes(y = ..density..), binwidth=80, 
                        fill="grey", colour="black") + 
         geom_density(alpha = 0.2)  + 
         facet_grid(size~taskTgt, scales="fixed", space="fixed")
m <- baseP +  theme_bw() +
    coord_cartesian(xlim = c(0, 3000)) +
    scale_x_continuous(breaks=seq(0, 5000, 800), name="Mean RT (ms)") + 
    scale_y_continuous(breaks=seq(0, 1, .004), name ="Density") + 
    theme(axis.title.x = element_text(size=34), #blank(), 
     axis.text.x  = element_text(angle=70, size=30, vjust = 1, hjust = 1),   
     axis.title.y = element_text(angle=90, size=34),
     axis.text.y  = element_text(size=30),
     strip.text.x = element_text(size=30, angle=0), #element_blank(), 
     strip.background = element_blank(),
     strip.text.y = element_text(size=30, angle=0))

## Save output to a jpg file--------------------------------
# jpeg(filename = "./figures/histSubjLevel.jpeg",
#      width = 1280, height = 1024, units = "px", pointsize = 8,
#      quality = 95, bg = "white")
m; 
# dev.off()