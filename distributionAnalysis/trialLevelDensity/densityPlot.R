# Disclaimer----------------------------------------------------
# Author:      Yishin Lin
# Date:        20 October, 2013
# Description: This file drew qqnorm for each cell at RT 
# mean level
rm(list=ls())
load('./data/mydata/ylinrt.RData')
library(ggplot2); library(grid)

## Preparing for density plots--------------------------------
ylinrt$subjNum <- sub("^d", "", ylinrt$subj)
ylinrt$target <- factor(ylinrt$target, levels=c('P','A'))
ylinrt$taskTgt <- paste0(ylinrt$task,ylinrt$target)
ylinrt$taskTgt <- factor(ylinrt$taskTgt, levels=c('FP','FA', 
                                              'CP', 'CA',
                                              'SP', 'SA'))

## Plot figures--------------------------------
den <- ggplot(ylinrt, aes(x=rt, colour=as.numeric(subjNum), 
                       group=as.numeric(subjNum)))
den <- den + geom_density(fill=NA, size=.5) +
    facet_grid(size~taskTgt, scales="free") + theme_bw() + 
    scale_x_continuous(breaks=seq(0, 10000, 1500), 
                       name="RT (ms)") + 
    scale_y_continuous(breaks=seq(0, 1, .004), name ="Density") +
    coord_cartesian(xlim = c(0, 4300)) +
    theme(axis.title.x = element_text(size=34), 
          axis.text.x  = element_text(angle=70, size=30, 
                                      vjust = 1, hjust = 1),   
          axis.title.y = element_text(angle=90, size=30),
          axis.text.y  = element_text(size=34),
          strip.text.x = element_text(size=30, angle=0),
          strip.text.y = element_text(size=30, angle=0),
          strip.background = element_blank(), 
          legend.position= "none", 
          legend.title = element_text(size=34),
          legend.text = element_text(size=30))

jpeg(filename = "./figures/densityAndcdfPlots/densityTrialLevel.jpeg",    width = 1280, height = 1024, units = "px", pointsize = 8,quality = 100, bg = "white")
den;
dev.off()
