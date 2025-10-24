#glmmTMB residuals
source("scripts/install_packages_function.R")
lp("DHARMa")
glmm.resids<-function(model){
  t1 <- simulateResiduals(model)
  print(testDispersion(t1))
  print(testZeroInflation(t1))
  plot(t1)
}
