eta2 <- function(fit){
  repeatedTest <- summary(fit, multivariate=F)[4]$univariate.tests
  cond.effect <- repeatedTest[2,1]  ; cond.error <- repeatedTest[2,3]     
  cond.Eta2 <- signif(cond.effect / (cond.effect + cond.error), 3)
  
  size.effect <- repeatedTest[3,1]  ; size.error <- repeatedTest[3,3]      
  size.Eta2 <- signif(size.effect / (size.effect + size.error), 3)
  
  int.effect <- repeatedTest[4,1]  ; int.error <- repeatedTest[4,3]  
  int.Eta2 <- signif(int.effect / (int.effect + int.error), 3)
  eta2 <- c(size.Eta2,cond.Eta2,int.Eta2)
  names(eta2) <- c("size", "cond", "int")
  out <- list(repeatedTest,eta2)
  names(out) <- c("rm.anova", "eta2")
  return(out)
}