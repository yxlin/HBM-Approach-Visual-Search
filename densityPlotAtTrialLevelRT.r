#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        20 October, 2013
# Description: This file drew qqnorm for each cell at RT 
# mean level
#-------------------------------------------------------  
rm(list=ls())
load('./data/mydata/ylinrt.RData')
library(ggplot2); library(grid)

## --------------------------------
## Preparing for bar plots
## --------------------------------
ylinrt$subjNum <- sub("^d", "", ylinrt$subj)

## --------------------------------
## Use ggplot to build the plots
## --------------------------------
den <- ggplot(ylinrt, aes(x=rt, colour=as.numeric(subjNum), 
                       group=as.numeric(subjNum)))
den <- den + geom_density(fill=NA, size=.5) +
    facet_grid(size~task*target, scales="free") + theme_bw() + 
    scale_x_continuous(breaks=seq(0, 10000, 2500), name="RT (ms)") + 
    scale_y_continuous(breaks=seq(0, 1, .004), name ="Density") +
    coord_cartesian(xlim = c(0, 4300)) +
    theme(axis.title.x = element_text(size=20), #element_blank(), 
          axis.text.x  = element_text(angle=90, size=20), #element_blank()
          axis.title.y = element_text(angle=90, size=20),
          axis.text.y  = element_text(size=20),
          strip.text.x = element_text(size=20, angle=0),
          strip.text.y = element_text(size=20, angle=90),
          strip.background = element_blank(), # rect(colour="red", fill="#CCCCFF"))
          legend.position= "none", #c(.18, .91),  
          legend.title = element_text(size=20),
          legend.text = element_text(size=20))

jpeg(filename = "./figures/densityTrialLevel.jpeg",
     width = 900, height = 900, units = "px", pointsize = 8,
     quality = 100, bg = "white")
den;
dev.off()

