# Disclaimer------------------------------------------------------
# Author:      Yishin Lin
# Date:        24 December, 2013
# Description: draw visually weighted plots (two datasets) 
loadedPackages <-c("plyr", "car", "ggplot2", "grid", "reshape2") 
suppressPackageStartupMessages(lapply(loadedPackages, require, character.only=TRUE));
rm(list=ls())

# Load packages, functions and data--------------------------------
load('./data/myData/BayesDataF/exp1WBF.RData')
load('./data/myData/BayesDataC/exp1WBC.RData')
load('./data/myData/BayesDataS/exp1WBS.RData')

# Preprocessing data  ------------------------------------
exp1WBF$task <- "F"
exp1WBC$task <- "C"
exp1WBS$task <- "S"
wbdf <-  rbind(exp1WBF, exp1WBC, exp1WBS)  

wbdf$shape <- wbdf$shape
wbdf$shift <- wbdf$shift*1000
wbdf$scale <- wbdf$scale*1000
wbdf$rt <- wbdf$rt*1000

wbdfPF <- subset(wbdf, target == "present" & task == "F")
wbdfPC <- subset(wbdf, target == "present" & task == "C")
wbdfPS <- subset(wbdf, target == "present" & task == "S")

rm(list=setdiff(ls(), c("wbdf", "wbdfPF", "wbdfPC", "wbdfPS")))

# Load private functions
source("./functions/multiplot.R")
source("./functions/vw2.R")

# Calculating shift, shape and scale  ------------------------------
pshiftPF <- vwReg(shift~size, wbdfPF, spag=T, 
                  spag.color="turquoise4", shade=F) 
pshiftPC <- vwReg(shift~size, wbdfPC, spag=T, 
                  spag.color="violetred4", shade=F, add=TRUE) 
pshiftPS <- vwReg(shift~size, wbdfPS, spag=T, 
                  spag.color="orange3", shade=F, add=TRUE) 

# shape 
pshapePF <- vwReg(shape~size, wbdfPF, spag=T, 
                  spag.color="turquoise4", shade=F) 
pshapePC <- vwReg(shape~size, wbdfPC, spag=T, 
                  spag.color="violetred4", shade=F, add=TRUE) 
pshapePS <- vwReg(shape~size, wbdfPS, spag=T, 
                  spag.color="orange3", shade=F, add=TRUE) 

# scale
pscalePF <- vwReg(scale~size, wbdfPF, spag=T, 
                  spag.color="turquoise4", shade=F) 
pscalePC <- vwReg(scale~size, wbdfPC, spag=T, 
                  spag.color="violetred4", shade=F, add=TRUE) 
pscalePS <- vwReg(scale~size, wbdfPS, spag=T, 
                  spag.color="orange3", shade=F, add=TRUE) 

# Plot shift, shape and scale--------------------------------
pShift3Task <- pshiftPF + pshiftPC + pshiftPS + 
  labs(x= 'Display Size', y='Shift (ms)', 
       title="Lin, Heinke, & Humphreys (2014)") +
  scale_y_continuous(breaks=seq(0, 600, 50)) +
  coord_cartesian(ylim=c(220, 520)) +
  theme(title = element_text(size=20),
        axis.title.x = element_blank(), 
        axis.text.x  = element_blank(), 
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y  = element_text(size=26))

pShape3Task <- pshapePF + pshapePC + pshapePS + 
  labs(x= 'Display Size', y='Shape') + 
  scale_y_continuous(breaks=seq(0, 3, 0.25)) +
  coord_cartesian(ylim=c(1.25, 2.28)) +
  theme(title = element_text(size=20),
        axis.title.x = element_blank(), 
        axis.text.x  = element_blank(), 
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y  = element_text(size=26))

pScale3Task <- pscalePF + pscalePC + pscalePS + 
  labs(x= 'Display Size', y='Scale (ms)') + 
  scale_y_continuous(breaks=seq(0, 2000, 250)) +
  coord_cartesian(ylim=c(100, 1200)) +
  theme(title = element_text(size=20),
        axis.title.x = element_text(size=30), 
        axis.text.x  = element_text(angle=0, size=26),
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y  = element_text(size=26))

# Load Wolfe et al.'s (2010)  data --------------------------------
load('./data/WolfePalmer/featureWB.Wolfe.RData')
load('./data/WolfePalmer/conjWB.Wolfe.RData')
load('./data/WolfePalmer/twovFiveWB.Wolfe.RData')

# Preprocessing data  ------------------------------------
featureWB$task <- "F"
conjWB$task <- "C"
twovFiveWB$task <- "S"

wolfedf <-  rbind(featureWB, conjWB, twovFiveWB)  

wolfedf$shape <- wolfedf$shape
wolfedf$shift <- wolfedf$shift*1000
wolfedf$scale <- wolfedf$scale*1000
wolfedf$rt <- wolfedf$rt*1000

wolfedfPF <- subset(wolfedf, target == "present" & task == "F")
wolfedfPC <- subset(wolfedf, target == "present" & task == "C")
wolfedfPS <- subset(wolfedf, target == "present" & task == "S")

# Calculating shift, shape and scale  ------------------------------
# shape 
wolfeshapePF <- vwReg(shape~size, wolfedfPF, spag=T, 
                      spag.color="turquoise4", shade=F) 
wolfeshapePC <- vwReg(shape~size, wolfedfPC, spag=T, 
                      spag.color="violetred4", shade=F, add=TRUE) 
wolfeshapePS <- vwReg(shape~size, wolfedfPS, spag=T, 
                      spag.color="orange3", shade=F, add=TRUE) 

# shift
wolfeshiftPF <- vwReg(shift~size, wolfedfPF, spag=T, 
                      spag.color="turquoise4", shade=F) 
wolfeshiftPC <- vwReg(shift~size, wolfedfPC, spag=T, 
                      spag.color="violetred4", shade=F, add=TRUE) 
wolfeshiftPS <- vwReg(shift~size, wolfedfPS, spag=T, 
                      spag.color="orange3", shade=F, add=TRUE) 

# scale
wolfescalePF <- vwReg(scale~size, wolfedfPF, spag=T, 
                      spag.color="turquoise4", shade=F) 
wolfescalePC <- vwReg(scale~size, wolfedfPC, spag=T, 
                      spag.color="violetred4", shade=F, add=TRUE) 
wolfescalePS <- vwReg(scale~size, wolfedfPS, spag=T, 
                      spag.color="orange3", shade=F, add=TRUE) 

# Plot shift --------------------------------
wolfeShift3Task <- wolfeshiftPF + wolfeshiftPC + wolfeshiftPS + 
  labs(x= 'Display Size', y='',
       title="Wolfe, Palmer, & Horowitz (2010)") +
  scale_y_continuous(breaks=seq(0, 600, 50)) +
  coord_cartesian(ylim=c(220, 520)) +
  theme(title = element_text(size=20),
        axis.title.x = element_blank(), 
        axis.text.x  = element_blank(), 
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y  = element_text(size=26))

## Plot shape --------------------------------
wolfeShape3Task <- wolfeshapePF + wolfeshapePC + wolfeshapePS + 
  labs(x= 'Display Size', y='') + 
  scale_y_continuous(breaks=seq(0, 3, 0.25)) +
  coord_cartesian(ylim=c(1.25, 2.28)) +
  theme(title = element_text(size=20),
        axis.title.x = element_blank(), 
        axis.text.x  = element_blank(), #text(angle=0, size=16), 
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y  = element_text(size=26))

## Plot scale --------------------------------
wolfeScale3Task <- wolfescalePF + wolfescalePC + wolfescalePS + 
  labs(x= 'Display Size', y='') + 
  scale_y_continuous(breaks=seq(0, 2000, 250)) +
  coord_cartesian(ylim=c(100, 1200)) +
  theme(title = element_text(size=20),
        axis.title.x = element_text(size=30), 
        axis.text.x  = element_text(angle=0, size=26),
        axis.title.y = element_text(angle=90, size=30),
        axis.text.y  = element_text(size=26))


# Put them altogether -----------------------------------------------
jpeg(filename = "./figures/vwWB_Lin_and_Wolfe.jpeg",
     width = 1280, height = 1024, units = "px", pointsize = 8,
     quality = 100,bg = "white")
multiplot(pShift3Task, wolfeShift3Task, 
          pShape3Task, wolfeShape3Task, 
          pScale3Task, wolfeScale3Task,
          cols=2)
dev.off()
