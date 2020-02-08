# df <- featAll
# table(df$signalDetection)
# featErr <- ddply(df, .(size, target, signalDetection, subj), 
#                  .drop=FALSE, summarise, 
#                  errRate = length(rt) )
# 
# featErr <- ddply(featAll, .(size, target, keyStatus, subj), 
#                  .drop=FALSE, summarise, 
#                  errRate = length(rt) )


signalDetection <- function(df){
  df$signalDetection <- ifelse(df$which == "present" & 
                           df$target == "P", "hit",
                    ifelse(df$which == "absent" & 
                           df$target == "A", "cr",
                    ifelse(df$which == "absent" &  
                           df$target == "P", "miss",
                           "fa")))
  featErr <- ddply(df, .(size, target, signalDetection, subj), 
                   .drop=FALSE, summarise, 
                   errRate = length(rt) )
  
  featHit <- subset(featErr, signalDetection=="hit",  select=errRate)
  featCR <- subset(featErr, signalDetection=="cr", select=errRate)
  featMiss <- subset(featErr, signalDetection=="miss", select=errRate)
  featFA <- subset(featErr, signalDetection=="fa", select=errRate)
  
  hit <- (featHit / (featHit+featCR+featMiss+featFA))*100
  cr <- (featCR / (featHit+featCR+featMiss+featFA))*100
  miss <- (featMiss / (featHit+featCR+featMiss+featFA))*100
  fa <- (featFA / (featHit+featCR+featMiss+featFA))*100
  
  tmp <- data.frame(hit,cr,miss,fa)
  names(tmp) <- c("hit","cr","miss","fa")
  return(tmp)
}