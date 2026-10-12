# Truncation check: allocation to the residual component H+1 and number of occupied components,
# for Simulation 1 (seed 1, H = 20, BART propensity score) and the 401(k) fit (H = 50, scripts/pension/pension_clsbp.R).
base <- "results/Kang_v4/R1000_260617/LSBP_1_mixture/"
files <- c(
  "t=-2" = paste0(base,"Kang_v4_seed1_tarsel-0264_theta_hs_xout_ig_sig_10_pid10_x4_x4_plugin_BART_H20.Rdata"),
  "t=0"  = paste0(base,"Kang_v4_seed1_tarsel00_theta_hs_xout_ig_sig_10_pid10_x4_x4_plugin_BART_H20.Rdata"),
  "t=2"  = paste0(base,"Kang_v4_seed1_tarsel0264_theta_hs_xout_ig_sig_10_pid10_x4_x4_plugin_BART_H20.Rdata"),
  "401k" = "results/pension/241026/test_lsbp_20_25_95.Rdata")
for(nm in names(files)){
  e <- new.env(); load(files[nm], e); fit <- get(ls(e)[1], e)
  H <- length(fit[[1]]$xout); n <- length(fit[[1]]$membershipOut)
  keep <- seq(length(fit)/2 + 1, length(fit))          # second half (burn-in removed)
  mem  <- sapply(fit[keep], function(it) it$membershipOut)   # n x iters
  occ_resid <- colMeans(mem == H)                       # share of subjects allocated to the residual (H+1)-th component, per iteration
  n_occ     <- apply(mem, 2, function(v) length(unique(v)))
  cat(sprintf("[%s] H=%d n=%d iters=%d | max index seen=%d | resid-comp share: max=%.4f mean=%.5f, iters with any=%d/%d | occupied comps: median=%d max=%d\n",
      nm, H, n, length(keep), max(mem), max(occ_resid), mean(occ_resid), sum(occ_resid>0), length(keep),
      median(n_occ), max(n_occ)))
  cat("   xout[[1]] length =", length(fit[[1]]$xout[[1]]), " (= q, weight design dim)\n")
}
