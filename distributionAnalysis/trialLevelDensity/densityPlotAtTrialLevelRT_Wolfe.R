# Disclaimer-------------------------------------------------
# Author:      Yishin Lin
# Date:        21 October, 2013
# Description: These codes use Wolfe et al. (2010) and 
# Palmer et al.'s (2011) data
rm(list=ls())

# Load data and packages ------------------------------------------
load('./data/WolfePalmer/trialAvg.RData')
library(ggplot2); library(grid)
trimWolfeData$target <- factor(trimWolfeData$target, levels=c('P','A'))
trimWolfeData$taskTgt <- paste0(trimWolfeData$task,trimWolfeData$target)
trimWolfeData$taskTgt <- factor(trimWolfeData$taskTgt, levels=c('FP','FA','CP', 'CA','SP', 'SA'))

## Use ggplot to build the plots--------------------------------
den <- ggplot(trimWolfeData, aes(x=rt, colour=as.numeric(subj), 
                          group=as.numeric(subj)))
den <- den + geom_density(fill=NA, size=.5) +
  facet_grid(size~taskTgt, scales="free") + theme_bw() + 
  scale_x_continuous(breaks=seq(0, 10000, 1500), name="RT (ms)") + 
  scale_y_continuous(breaks=seq(0, 1, .004), name ="Density") +
  coord_cartesian(xlim = c(0, 4300)) +
  theme(axis.title.x = element_text(size=34), 
        axis.text.x  = element_text(angle=70, size=30, 
                                    vjust = 1, hjust = 1),   
        axis.title.y = element_text(angle=90, size=34),
        axis.text.y  = element_text(size=30),
        strip.text.x = element_text(size=30, angle=0),
        strip.text.y = element_text(size=30, angle=0),
        strip.background = element_blank(),
        legend.position= "none", 
        legend.title = element_text(size=34),
        legend.text = element_text(size=30))

jpeg(filename = "./figures/densityAndcdfPlots/densityTrialLevel_Wolfe.jpeg",width = 1280, height = 1024, units = "px", pointsize = 8,quality = 100, bg = "white")
den;
dev.off()
