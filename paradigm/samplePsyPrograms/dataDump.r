# Disclaimer-------------------------------------------------------
# Author: Yishin Lin
# Date: 04 January, 2012. 
#       14 Jan, 2012. Modified to fit simple search 
# Description: Save data from temporary storage in /dev/shm/data 
# to a file when a participant finished a block.
rm(list=ls())

x0 <- read.table( "/dev/shm/data" )
#----------------------------------
#-- Dump data into to a save keep--#
colnames(x0) <- c('blockName', 'counter', 'target', 'setsize', 
                  'keyPressed', 'keyStatus', 'rt')
write.table(x0, file='dataDump.txt', row.names=F, col.names=T, 
            quote=F)
