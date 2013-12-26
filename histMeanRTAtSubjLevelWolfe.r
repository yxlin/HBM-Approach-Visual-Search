#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        21 October, 2013
# Description: These codes use Wolfe et al. (2010) and 
# Palmer et al.'s (2011) data
#-------------------------------------------------------  
rm(list=ls())
load('./data/WolfePalmer/trialAvg.RData')
library(ggplot2); library(grid)
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
jpeg(filename = "./figures/histSubjLevelWolfe.jpeg",
     width = 800, height = 600, units = "px", pointsize = 8,
     quality = 95, bg = "white")
m;
dev.off()
