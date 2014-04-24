#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        15 Feb, 2014
# Description: This file drew CDF using my and Wolfe et al.
# data, putting them together 
loadedPackages <-c("plyr", "grid", "ggplot2") 
suppressPackageStartupMessages(lapply(loadedPackages, require, character.only=TRUE));
rm(list=ls())
load('./data/mydata/ylinrt.RData')
load('./data/WolfePalmer/trialAvg.RData')

## --------------------------------
## Preparing for plotting
ylinrt$subjNum <- sub("^d", "", ylinrt$subj)
tmp1 <- ylinrt[,c(7,10,3,2,6)]
tmp2 <- trimWolfeData[,c(1,2,4,5,8)]
ecdfylinrt <- ddply(tmp1, .(size, task, target), transform, 
                    ecdf = ecdf(rt)(rt))
ecdfylinrt$targettask <- paste(ecdfylinrt$target, ecdfylinrt$task, sep="-")
ecdfwolfe <- ddply(tmp2, .(size, task, target), transform, 
                   ecdf = ecdf(rt)(rt))
ecdfwolfe$targettask <- paste(ecdfwolfe$target, ecdfwolfe$task, 
                              sep="-")
ecdfylinrt$dataset <- "Lin, Heinke & Humphreys (2014)"
ecdfwolfe$dataset <- "Wolfe, Palmer & Horowitz (2010)"
twoDatasets <- rbind(ecdfylinrt,ecdfwolfe)

## --------------------------------
## Drawing
## --------------------------------
ecdfP2 <- ggplot(twoDatasets, aes(rt, ecdf, colour = task))
ecdfP2 <- ecdfP2 +  geom_step(subset = .(rt > 0), size=2) + 
  facet_grid(size~dataset, as.table = F) +
  scale_colour_grey(start=0, end=.9) +
  geom_hline(yintercept = .5, linetype="longdash", 
             color="black") +
  geom_hline(yintercept = .95, linetype="longdash", 
             color="black") +
  #   geom_vline(xintercept = 1300, linetype="longdash", color="darkorchid4") +
  #   geom_vline(xintercept = 1800, linetype="longdash", color="darkblue") +
  #   geom_vline(xintercept = 2600, linetype="longdash", color="black") +
  scale_y_continuous(breaks=seq(0, 2, .40), 
                     name="Empirical CDF") + 
  scale_x_continuous(name="RT (ms)") + 
  coord_cartesian(xlim = c(150, 4500), ylim=c(0,1.05)) + 
  theme_bw() + 
  theme(axis.title.x =  element_text(angle=0, size=34),
        axis.text.x  =  element_text(angle=0, size=30),
        axis.title.y = element_text(angle=90, size=34),
        axis.text.y  = element_text(angle=0, size=30),
        strip.text.x = element_text(angle=0, size=34),
        strip.text.y = element_text(angle=0, size=34),
        strip.background = element_blank(), # rect(colour="red", fill="#CCCCFF")), 
        legend.position= c(.90, .10),
        legend.title = element_text(size=34),
        legend.text = element_text(size=30))

jpeg(filename = "./figures/CDF_LinAndWolfe_grey.jpeg",
     width = 1280, height = 1024, units = "px", pointsize = 8,
     quality = 95, bg = "white")
ecdfP2; 
dev.off()
