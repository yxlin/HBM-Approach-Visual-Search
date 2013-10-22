#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        20 October, 2013
# Description: This file drew qqnorm for each cell at RT 
# mean level
#-------------------------------------------------------  
rm(list=ls())
load('./data/mydata/ylinrt.RData')
library(plyr); library(ggplot2); library(grid)

## --------------------------------
## Preparing for plotting
## --------------------------------
ylinrt$subjNum <- sub("^d", "", ylinrt$subj)
ecdfylinrt <- ddply(ylinrt, .(size, task, target), transform, 
                   ecdf = ecdf(rt)(rt))
ecdfylinrt$targettask <- paste(ecdfylinrt$target, ecdfylinrt$task, sep="-")


ecdfP1 <- ggplot(ecdfylinrt, aes(rt, ecdf, colour = targettask))
ecdfP1 <- ecdfP1 +  geom_step(subset = .(rt > 0), size=2) + 
  facet_grid(size~., as.table = F) +
  #   geom_vline(xintercept = 1300, linetype="longdash", color="darkorchid4") +
  #   geom_vline(xintercept = 1800, linetype="longdash", color="darkblue") +
  #   geom_vline(xintercept = 2600, linetype="longdash", color="black") +
  scale_y_continuous(breaks=seq(0, 2, .40), 
                     name="Empirical CDF") + 
  scale_x_continuous(name="RT (ms)") + 
  coord_cartesian(xlim = c(150, 3100), ylim=c(0,1.01)) + 
  theme_bw() + 
  theme(axis.title.x =  element_text(angle=0, size=20),
        axis.text.x  =  element_text(angle=0, size=20),
        axis.title.y = element_text(angle=90, size=20),
        axis.text.y  = element_text(angle=0, size=20),
        strip.text.x = element_blank(),
        strip.text.y = element_text(angle=0, size=20),
        strip.background = element_blank(), # rect(colour="red", fill="#CCCCFF")), 
        legend.position= c(.90, .10),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20))

jpeg(filename = "./figures/rtecdfAllLines.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 95, bg = "white")
ecdfP1; 
dev.off()


## --------------------------------
## Show regions 
## --------------------------------
ecdfP2 <- ggplot(ecdfylinrt, aes(rt, ecdf, colour = task))
ecdfP2 <- ecdfP2 +  geom_step(subset = .(rt > 0), size=2) + 
  facet_grid(size~., as.table = F) +
  geom_hline(yintercept = .5, linetype="longdash", 
             color="darkorchid4") +
  geom_hline(yintercept = .95, linetype="longdash", 
             color="darkblue") +
  #   geom_vline(xintercept = 1300, linetype="longdash", color="darkorchid4") +
  #   geom_vline(xintercept = 1800, linetype="longdash", color="darkblue") +
  #   geom_vline(xintercept = 2600, linetype="longdash", color="black") +
  scale_y_continuous(breaks=seq(0, 2, .40), 
                     name="Empirical CDF") + 
  scale_x_continuous(name="RT (ms)") + 
  coord_cartesian(xlim = c(150, 3100), ylim=c(0,1.01)) + 
  theme_bw() + 
  theme(axis.title.x =  element_text(angle=0, size=20),
        axis.text.x  =  element_text(angle=0, size=20),
        axis.title.y = element_text(angle=90, size=20),
        axis.text.y  = element_text(angle=0, size=20),
        strip.text.x = element_blank(),
        strip.text.y = element_text(angle=0, size=20),
        strip.background = element_blank(), # rect(colour="red", fill="#CCCCFF")), 
        legend.position= c(.90, .10),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20))

jpeg(filename = "./figures/rtecdfTrialRT.jpeg",
     width = 600, height = 600, units = "px", pointsize = 8,
     quality = 95, bg = "white")
ecdfP2; 
dev.off()



