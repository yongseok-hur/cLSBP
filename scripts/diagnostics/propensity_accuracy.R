# Accuracy of the GLM- and BART-based propensity score estimates against the true propensity score
# (Simulation 1, seed 1, t = -2, 0, 2; Appendix C.1). Usage: Rscript scripts/diagnostics/propensity_accuracy.R

source("R/utils.R")
tags <- c("-0264","00","0264")
for(t in tags){
  d <- read.csv(sprintf("data/Kang_v4/d0001_tarsel%s_500.csv", t))
  x <- as.matrix(d[,c("x1","x2","x3","x4")]); z <- d$z
  pg <- estimate_pi(method="GLM", z, x); pb <- estimate_pi(method="BART", z, x)
  cat(t, "| GLM  corr", round(cor(pg,d$pi),3), "RMSE", round(sqrt(mean((pg-d$pi)^2)),3),
         "| BART corr", round(cor(pb,d$pi),3), "RMSE", round(sqrt(mean((pb-d$pi)^2)),3), "\n")
}
