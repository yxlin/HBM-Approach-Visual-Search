rm(list=ls())

# 04 January, 2012. 
# 14 Jan, 2012. Modified to fit simple search 
x0 = read.table( "/dev/shm/data" )
#----------------------------------#
#-- Dump data into to a save keep--#
#----------------------------------#
colnames(x0) <- c('blockName', 'counter', 'target', 'setsize', 'keyPressed', 'keyStatus', 'rt')
write.table(x0, file='dataDump.txt', row.names=F, col.names=T, quote=F)
#----------------------------------#