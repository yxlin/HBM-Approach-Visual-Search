# Disclaimer-----------------------------------------------
# Author:      Yishin Lin
# Date:        21 October, 2013
# Description: These codes use Wolfe et al.'s (2010) data
rm(list=ls())
load('./data/WolfePalmer/trialAvg.RData')
library(ggplot2); library(grid)
avg1$target <- factor(avg1$target, levels=c('P','A'))
avg1$taskTgt <- paste0(avg1$task,avg1$target)
avg1$taskTgt <- factor(avg1$taskTgt, levels=c('FP','FA', 
                                                  'CP', 'CA',
                                                  'SP', 'SA'))
## Use ggplot to contruct the plots--------------------------------
baseP <- ggplot(avg1, aes(x=mean)) +   
  geom_histogram(aes(y = ..density..), binwidth=80, 
                 fill="grey", colour="black") + 
  geom_density(alpha = 0.2)  + 
  facet_grid(size~taskTgt, scales="fixed", space="fixed")
m <- baseP +  theme_bw() +
  coord_cartesian(xlim = c(0, 3000)) +
  scale_x_continuous(breaks=seq(0, 5000, 800), name="Mean RT (ms)") + 
  scale_y_continuous(breaks=seq(0, 1, .004), name ="Density") + 
  theme(axis.title.x = element_text(size=34),  
        axis.text.x  = element_text(angle=70, size=34, 
                                    vjust = 1, hjust = 1),   
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y  = element_text(size=34),
        strip.text.x = element_text(size=30, angle=0), 
        strip.background = element_blank(),
        strip.text.y = element_text(size=30, angle=0))
 
## --------------------------------
## Save output to a jpg file
## --------------------------------
jpeg(filename = "./figures/densityAndcdfPlots/histSubjLevel_Wolfe.jpeg",width = 1280, height = 1024, units = "px", pointsize = 8,quality = 100, bg = "white")
m;
dev.off()
