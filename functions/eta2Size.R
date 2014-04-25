eta2Size <- function(fit){
  repeatedTest <- summary(fit, multivariate=F)[4]$univariate.tests
  size.effect <- repeatedTest[2,1]  ; size.error <- repeatedTest[2,3]     
  size.Eta2 <- signif(size.effect / (size.effect + size.error), 3)
  eta2 <- c(size.Eta2)
  names(eta2) <- c("size")
  out <- list(repeatedTest,eta2)
  names(out) <- c("rm.anova", "eta2")
  return(out)
}
