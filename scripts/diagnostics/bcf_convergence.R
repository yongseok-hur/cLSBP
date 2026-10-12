# BCF convergence diagnostics (R-hat, ESS) on representative Simulation 1 fits (seed 1, t = -2, 0, 2),
# BART propensity score (primary specification). Fits are saved by scripts/sim/02_sim1_kang.R for seeds 1-2.
suppressMessages(library(coda))
base <- "results/Kang_v4/R1000_260617/BCF/"
tags <- c("t=-2"="tarsel-0264", "t=0"="tarsel00", "t=2"="tarsel0264")
for(nm in names(tags)){
  e <- new.env(); load(paste0(base,"Kang_v4_seed1_",tags[nm],"_defaultx4_x4_plugin_BART.Rdata"), e); fit <- get("test", e)
  cc <- fit$coda_chains; nch <- length(cc); per <- nrow(cc[[1]]); keep <- (per/2+1):per   # second half of each chain
  cc2 <- mcmc.list(lapply(cc, function(m) mcmc(m[keep,])))
  gd <- gelman.diag(cc2, multivariate=FALSE)$psrf[,1]; es <- effectiveSize(cc2)
  ate <- rowMeans(fit$tau)
  ate_ch <- mcmc.list(lapply(1:nch, function(k) mcmc(ate[(k-1)*per + keep])))
  cat(sprintf("[%s] (2nd half) hyper: max Rhat=%.3f (%s), min ESS=%.0f (%s) | ATE: Rhat=%.3f, ESS=%.0f\n",
      nm, max(gd), names(gd)[which.max(gd)], min(es), names(es)[which.min(es)],
      gelman.diag(ate_ch)$psrf[1,1], effectiveSize(ate_ch)))
  cat("     all Rhat:", paste(names(gd), round(gd,3), collapse="  "), "\n")
}
