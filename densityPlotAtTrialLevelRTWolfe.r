#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        21 October, 2013
# Description: These codes use Wolfe et al. (2010) and 
# Palmer et al.'s (2011) data
#-------------------------------------------------------  
rm(list=ls())
load('./data/WolfePalmer/trialAvg.RData')
library(ggplot2); library(grid)

## --------------------------------
## Use ggplot to build the plots
## --------------------------------
den <- ggplot(trimWolfeData, aes(x=rt, colour=as.numeric(subj), 
                          group=as.numeric(subj)))
den <- den + geom_density(fill=NA, size=.5) +
  facet_grid(size~task*target, scales="free") + theme_bw() + 
  scale_x_continuous(breaks=seq(0, 10000, 1500), name="RT (ms)") + 
  scale_y_continuous(breaks=seq(0, 1, .005), name ="Density") +
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

jpeg(filename = "./figures/densityTrialLevelWolfe.jpeg",
     width = 800, height = 600, units = "px", pointsize = 8,
     quality = 100, bg = "white")
den;
dev.off()
