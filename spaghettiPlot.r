#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        17 October, 2013
# Description: This code does a preliminary examiniation 
# of each pariticpant's mean RT to size size separately
# a spaghetti plot is produced to visually examine the 
# relationship between different experimental conditons
#-------------------------------------------------------  

rm(list=ls())
#----------------------------
# Load package and data 
#----------------------------
load('./data/ylinrt.RData')
raw1 <- ddply(ylinrt, .(target, size, subj, task), summarize, 
              N = length(rt), mean = mean(rt), median = median(rt))


#----------------------------
# Spaghetti plot 
#----------------------------
# Each participant is drawn as a function of mean RT against display
# size
p <- ggplot(raw1, aes(x=size, y = mean, group=subj)) + 
  geom_line(aes(colour=subj), size = 1.2) + theme_bw() +
  facet_grid(target~task)

png("./figures/spaghetti.png", width=800, height=600)
print(p)
dev.off()




