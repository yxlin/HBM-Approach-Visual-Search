# Disclaimer-------------------------------------------------------
# Author:      Yishin Lin
# Date:        30 March, 2014
# Description: Calculate ANOVA and eta square for miss errors and 
# false alarm
rm(list=ls())
library(ggplot2); library(grid); library(RColorBrewer)

# Load data and functions ---------------------------------------------
load('./data/myData/avgFC.RData')
load('./data/myData/avgS.RData')
source("./functions/summarise.R")
source("./functions/multiplot.R")


# Summarise data ------------------------------------------------------
avgMiss1 <- summarySEwithin(avgFC, measurevar="miss", 
                           withinvars=c("size", "task"),
                           idvar="subj")
avgFA1 <- summarySEwithin(avgFC, measurevar="fa", 
                           withinvars=c("size", "task"),
                           idvar="subj")

avgMiss2 <- summarySEwithin(avgS, measurevar="miss", 
                           withinvars=c("size", "task"),
                           idvar="subj")
avgFA2 <- summarySEwithin(avgS, measurevar="fa", 
                         withinvars=c("size", "task"),
                         idvar="subj")
avgMiss <- rbind(avgMiss1,avgMiss2)
avgFA <- rbind(avgFA1,avgFA2)
names(avgMiss) <- c("size", "task", "N", "value", "sd", "se", "ci")
names(avgFA) <- c("size", "task", "N", "value", "sd", "se", "ci")
avgMiss$Type <- "Miss Errors"
avgFA$Type <- "False Alarm"
avg <- rbind(avgMiss, avgFA)

avg <- within(avg, Type <- factor(Type, levels = 
                                  c("Miss Errors", "False Alarm") ))

PuRdSet <- colorRampPalette(brewer.pal(9,"PuRd"))(10)
GreenSet <- colorRampPalette(brewer.pal(9,"Greens"))(10)

avg$colour <- ifelse(avg$Type == "Miss Errors" & avg$size == 3,
                     PuRdSet[4],
              ifelse(avg$Type == "Miss Errors" & avg$size == 6,
                     PuRdSet[6],
              ifelse(avg$Type == "Miss Errors" & avg$size == 12,
                     PuRdSet[8],
              ifelse(avg$Type == "Miss Errors" & avg$size == 18,
                     PuRdSet[10],
              ifelse(avg$Type == "False Alarm" & avg$size == 3,
                     GreenSet[4],
              ifelse(avg$Type == "False Alarm" & avg$size == 6,
                     GreenSet[6],
              ifelse(avg$Type == "False Alarm" & avg$size == 12,
                     GreenSet[8],
              ifelse(avg$Type == "False Alarm" & avg$size == 18,
                     GreenSet[10], "Unknown"))))))))
                            
jColours <- avg$colour
avg$colourNames <- paste(avg$size, avg$Type, sep="-")
names(jColours) <- paste(avg$size, avg$Type, sep="-")

avg <- within(avg, colourNames <- factor(colourNames, levels = 
  c("3-Miss Errors", "6-Miss Errors", "12-Miss Errors", "18-Miss Errors",
    "3-False Alarm", "6-False Alarm", "12-False Alarm", "18-False Alarm")))


# Plot figures ------------------------------------------------------------
p3 <- ggplot(avg, aes(x=task, y=value, fill=colourNames)) +
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.25, 
                aes(ymin=value-se, ymax=value+se)) +
  scale_fill_manual(values=jColours) +
  facet_grid(.~Type)+
  scale_x_discrete(name='Task') +
  scale_y_continuous(name = "") +
  theme(axis.title.x = element_blank(), # text(size=20)
        axis.text.x  = element_text(angle=0, size=24, colour="black"), #blank(),  
        axis.title.y = element_blank(), #text(angle=90, size=20),
        axis.text.y  = element_text(size=24, colour="black"),
        strip.text.x = element_text(size=26, angle=0), 
        strip.background = element_blank(),
        #strip.text.y = element_text(size=20, angle=90),
        legend.position= "none")
p4 <- p3 + annotate("text", x = 1, y = 5, size=7, label = "Display Size") +
     annotate("text", x = seq(.65, 1.4, .23), y = rep(4,4), 
              label = c("3", "6", "12", "18")) 
     


# Save output -------------------------------------------------------------
jpeg(filename = "./figures/missErrors_falseAlarm.jpeg",
     width = 1024, height = 768, units = "px", pointsize = 8,
     quality = 95 ,bg = "white")
print(p4)
dev.off()