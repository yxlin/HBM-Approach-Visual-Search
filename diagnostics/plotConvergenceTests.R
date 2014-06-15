# Disclaimer-------------------------------------------------------
# Author: Yishin Lin
# Date: 15 June, 2014
# Description: Redraw diagnostic figures for MCMC process

# Load mcmc, rjags pacakge and data------------------------------
# update:
loadedPackages <-c("rjags", "mcmcplots", "plyr", "ggplot2",
                   "grid")
lapply(loadedPackages, require, character.only=TRUE);
rm(list=ls())
load("./data/mcmcImage/data18present.RData")
source("./functions/multiplot.R")
jagsfit.mcmc <- as.mcmc(jagsfit) # convert to MCMC list 

# Draw beta 1 only for demo -------------------------------------
tmp <- jagsfit.mcmc[, "beta[1]", drop=FALSE]
chain1 <- as.data.frame( tmp[[1]] ); names(chain1) <- "beta1"
chain2 <- as.data.frame( tmp[[2]] ); names(chain2) <- "beta1"
chain3 <- as.data.frame( tmp[[3]] ); names(chain3) <- "beta1"

bacf1 <- acf(chain1, plot=F)
bacfdf1 <- with(bacf1, data.frame(lag, acf))
bacf2 <- acf(chain2, plot=F)
bacfdf2 <- with(bacf2, data.frame(lag, acf))
bacf3 <- acf(chain3, plot=F)
bacfdf3 <- with(bacf3, data.frame(lag, acf))

bacfdf1$chain <- "Chain 1"
bacfdf2$chain <- "Chain 2"
bacfdf3$chain <- "Chain 3"
bacfdf <- rbind(bacfdf1,bacfdf2,bacfdf3)

# Autocorrelation plots ------------------------------------------
pacf <- ggplot(bacfdf, aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  facet_grid(chain~.) +
  theme_bw() +
  scale_x_continuous(name='Lag') +
  scale_y_continuous(name = "Autocorrelation") + 
  theme(axis.title.x = element_text(size=34),  
        axis.text.x  = element_text(angle=0, size=30), 
        axis.title.y = element_text(angle=90, size=34),
        axis.text.y  = element_text(size=30),
        strip.text.x = element_text(size=34, angle=0), 
        strip.background = element_blank(),
        strip.text.y = element_text(size=30, angle=90),
        legend.position= 'none')

jpeg(filename = "./figures/mcmcplots/acfplots.jpeg",
     width = 1280, height = 1024, units = "px", pointsize = 8,
     quality = 100,bg = "white")
pacf
dev.off()

# Prepare data frame -----------------------------------
chain1$chain <- 1
chain2$chain <- 2
chain3$chain <- 3

chain1$iteration <- seq(1, nrow(chain1))
chain2$iteration <- seq(1, nrow(chain2))
chain3$iteration <- seq(1, nrow(chain3))

chain <- rbind(chain1,chain2,chain3)

# Plot posterior distributions -----------------------------------
den0 <- ggplot(chain, aes(x=beta1, group=chain, 
                          linetype=as.factor(chain)))

den1 <- den0 + geom_density() + 
  scale_linetype_manual(values=c(1, 2, 3), name="Chain") +
  theme_bw() +
  scale_x_continuous(name='Beta1') +
  scale_y_continuous(name = "Density") +
  theme(axis.title.x = element_text(size=34),  
        axis.text.x  = element_text(angle=0, size=30), 
        axis.title.y = element_blank(),
        axis.text.y  = element_text(size=30),
        strip.text.x = element_text(size=34, angle=0), 
        strip.background = element_blank(),
        strip.text.y = element_text(size=30, angle=90),
        legend.position= c(.8,.8),
        legend.title = element_text(size=34),
        legend.text = element_text(size=30),
        legend.key.height=unit(3.5, "line"),
        legend.key.width=unit(1.5, "cm"))

den2 <- den0 + geom_density() + 
  facet_grid(chain~.) +
  scale_linetype_manual(values=c(1, 2, 3), name="Chain") +
  theme_bw() +
  scale_x_continuous(name='Beta1') +
  scale_y_continuous(name = "Density") +
  theme(axis.title.x = element_text(size=34),  
        axis.text.x  = element_text(angle=0, size=30), 
        axis.title.y = element_text(angle=90, size=34),
        axis.text.y  = element_text(size=30),
        strip.text.x = element_text(size=34, angle=0), 
        strip.background = element_blank(),
        strip.text.y = element_blank(), 
        legend.position= "none",
        legend.title = element_text(size=34),
        legend.text = element_text(size=30),
        legend.key.height=unit(3.5, "line"),
        legend.key.width=unit(1.5, "cm"))

jpeg(filename = "./figures/mcmcplots/denplots.jpeg",
     width = 1280, height = 1024, units = "px", pointsize = 8,
     quality = 100,bg = "white")
multiplot(den2, den1, cols=2)
dev.off()

# Trace plots ----------------------------------------------------
tra1 <- ggplot(chain, aes(x=iteration, y=beta1)) +
  geom_line() + facet_grid(chain~.) + theme_bw() +
  scale_x_continuous(name='Iteration') +
  scale_y_continuous(name = "Beta1") +
  theme(axis.title.x = element_text(size=34),  
        axis.text.x  = element_text(angle=70, size=30,
                                    vjust = 1, hjust = 1), 
        axis.title.y = element_blank(), 
        axis.text.y  = element_text(size=30),
        strip.text.x = element_text(size=34, angle=0), 
        strip.background = element_blank(),
        strip.text.y = element_blank(), 
        legend.position= "none")


tra2 <- ggplot(chain, aes(x=iteration, y=beta1)) +
  geom_line() + theme_bw() +
  scale_x_continuous(name='Iteration') +
  scale_y_continuous(name = "Beta1") +
  theme(axis.title.x = element_text(size=34),  
        axis.text.x  = element_text(angle=70, size=30,
                                    vjust = 1, hjust = 1), 
        axis.title.y = element_text(size=30),
        axis.text.y  = element_text(size=30),
        strip.text.x = element_text(size=34, angle=0), 
        strip.background = element_blank(),
        strip.text.y = element_blank(), 
        legend.position= "none")

jpeg(filename = "./figures/mcmcplots/tracePlots.jpeg",
     width = 1280, height = 1024, units = "px", pointsize = 8,
     quality = 100,bg = "white")
multiplot(tra2, tra1, cols=2)
dev.off()
