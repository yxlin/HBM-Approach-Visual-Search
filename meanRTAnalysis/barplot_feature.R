#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        18 October, 2013
# Description: Draw feature searc task separately
rm(list=ls())
load('./data/myData/avgFC.RData')
load('./data/myData/avgS.RData')
source("./functions/summarise.R")
source("./functions/multiplot.R")

Average across participants --------------------------------
avgRT1 <- summarySEwithin(avgFC, measurevar="mean", 
                          withinvars=c("task", "size", "target"),
                          idvar="subj")
avgRT2 <- summarySEwithin(avgS, measurevar="mean", 
                          withinvars=c("size", "target"),
                          betweenvars="task",
                          idvar="subj")
avgErr1 <- summarySEwithin(avgFC, measurevar="errRate", 
                           withinvars=c("task", "size", "target"),
                           idvar="subj")
avgErr2 <- summarySEwithin(avgS, measurevar="errRate", 
                           withinvars=c("size", "target"),
                           betweenvars="task",
                           idvar="subj")

featP <- subset(avgRT1, task == "F" & target == "P")
conjP <- subset(avgRT1, task == "C" & target == "P")
spatP <- subset(avgRT2, task == "S" & target == "P")


# Show descriptive statistics ------------------------------------
# mean(featP$mean / c(3,6,12,18))
# mean(conjP$mean / c(3,6,12,18))
# mean(spatP$mean / c(3,6,12,18))

avgRT0 <- rbind(avgRT1, avgRT2)
avgErr0 <- rbind(avgErr1, avgErr2)
avgRT.Present <- subset(avgRT0, target=="P")
avgErr.Present <- subset(avgErr0, target=="P")
ddply(avgErr.Present, .(task), summarise,
      meanErr = mean(errRate))
# 11.80-8.62
# 8.62-5

## Separately draw feature search  ----------------------------
# Because the RTs in the feature are relatively quick,
# we need to zoom in to the range to see the reliable difference.
avgRT.Feature <- subset(avgRT.Present, task == "F")
p1 <- ggplot(avgRT.Feature, 
             aes(x=factor(size), y = mean, fill=task)) + 
  geom_bar(stat="identity", position=position_dodge(1)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                width=.4, position=position_dodge(1), size=2)
barRT.Feature <- p1 + theme_bw() +  
  scale_fill_grey(start=0, end=.9) +
  scale_x_discrete(name='Display size') +
  scale_y_continuous(name = "Mean RT (ms)") +
  coord_cartesian(ylim=c(405, 450)) +
  theme(axis.title.x = element_blank(), 
        axis.text.x  = element_text(angle=0, size=40),   
        axis.title.y = element_blank(), 
        axis.text.y  = element_text(size=40),
        legend.position= "none")

# jpeg(filename = "./figures/boxAndBarPlots/rtBarFeaturePresent_grey.jpeg",width = 1280, height = 1024, units = "px", pointsize = 8,quality = 95 ,bg = "white")
print(barRT.Feature)
# dev.off()
