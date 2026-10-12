#!/usr/bin/env Rscript
# ============================================================
# Causal forest estimates for the 401(k) data (Section 4.2): ATE and the subgroups of Table 4.
#   Part A: subgroup means of a stored unit-level tau_hat (skipped if results/pension/cf/pension_cf_260701.rds is absent).
#   Part B: causal forest fit (fixed seed) and grf's AIPW estimates with 95% intervals, using grf's default
#           propensity score and the BART propensity score (Table 4 reports the BART version).
# Usage: Rscript scripts/pension/pension_cf_table4.R
# Output: results/pension/cf/pension_cf_table4_260929.rds
# ============================================================
suppressMessages({ library(dplyr); library(grf); library(hdm) })

## ---- data (same pipeline as the 401(k) analysis) ----
data(pension)
pension <- pension %>% filter(e401 == 1)
Y_full <- pension$net_tfa
TRUNC_IDX <- abs(Y_full) < quantile(abs(Y_full), 0.95)
Z <- as.numeric(pension$p401[TRUNC_IDX])
X <- pension %>%
  dplyr::select(a1,a2,a3,a4, i1,i2,i3,i4,i5,i6, fsize, nohs,hs,smcol, marr,twoearn,db,pira,hown)
X <- as.matrix(X[TRUNC_IDX, ]); X[,"fsize"] <- scale(X[,"fsize"])
Y <- Y_full[TRUNC_IDX]
cat("n =", nrow(X), "\n")

## ---- groups (16 groups, including the base categories) ----
groups <- list(
  "age: base" = rowSums(X[,c("a1","a2","a3","a4")]) == 0,
  "age: a1" = X[,"a1"]==1, "age: a2" = X[,"a2"]==1, "age: a3" = X[,"a3"]==1, "age: a4" = X[,"a4"]==1,
  "inc: base" = rowSums(X[,c("i1","i2","i3","i4","i5","i6")]) == 0,
  "inc: i1" = X[,"i1"]==1, "inc: i2" = X[,"i2"]==1, "inc: i3" = X[,"i3"]==1,
  "inc: i4" = X[,"i4"]==1, "inc: i5" = X[,"i5"]==1, "inc: i6" = X[,"i6"]==1,
  "edu: nohs" = X[,"nohs"]==1, "edu: hs" = X[,"hs"]==1, "edu: smcol" = X[,"smcol"]==1,
  "edu: base(college)" = rowSums(X[,c("nohs","hs","smcol")]) == 0)

## ---- Part A: means of the stored tau_hat ----
f <- "results/pension/cf/pension_cf_260701.rds"
if (file.exists(f)) {
  r <- readRDS(f)
  cat("\n=== Part A: stored rds (", f, ") -- mean tau_hat, 1000 USD ===\n")
  for (m in names(r)) {
    th <- r[[m]]$tau_hat
    cat(sprintf("[%s] ATE(AIPW, stored) = %.2f\n", m, r[[m]]$ate["estimate"]/1000))
    for (g in names(groups)) cat(sprintf("  %-20s n=%4d  %6.1f\n", g, sum(groups[[g]]),
                                         mean(th[groups[[g]]])/1000))
  }
}

## ---- Part B: refit + AIPW subset ATE ----
set.seed(20260929)
get_pihat_BART <- function() {   # BART propensity score (BayesTree if available, else dbarts)
  if (requireNamespace("BayesTree", quietly = TRUE)) {
    fit <- BayesTree::bart(X, Z, verbose = FALSE); return(pnorm(colMeans(fit$yhat.train))) }
  fit <- dbarts::bart(X, Z, verbose = FALSE); pnorm(colMeans(fit$yhat.train))
}
NTH <- 4   # number of threads for grf
fits <- list(default = causal_forest(X, Y, Z, seed = 20260929, num.threads = NTH),
             BART    = causal_forest(X, Y, Z, W.hat = get_pihat_BART(), seed = 20260929, num.threads = NTH))

fmt <- function(a) sprintf("%5.1f (%5.1f, %5.1f)", a[1]/1000,
                           (a[1]-1.96*a[2])/1000, (a[1]+1.96*a[2])/1000)
out <- list()
for (m in names(fits)) {
  cf <- fits[[m]]
  cat(sprintf("\n=== Part B: CF-%s (AIPW, 1000 USD, 95%% CI) ===\n", m))
  cat(sprintf("  %-20s n=%4d  %s\n", "ATE", nrow(X),
              fmt(average_treatment_effect(cf, target.sample = "all"))))
  tab <- data.frame()
  for (g in names(groups)) {
    idx <- groups[[g]]
    a <- average_treatment_effect(cf, target.sample = "all", subset = idx)
    tau_mean <- mean(predict(cf)$predictions[idx])      # reference only: mean tau_hat(x)
    cat(sprintf("  %-20s n=%4d  %s   [mean tau_hat %.1f]\n", g, sum(idx), fmt(a), tau_mean/1000))
    tab <- rbind(tab, data.frame(ps = m, group = g, n = sum(idx),
                                 est = a[1]/1000, se = a[2]/1000, mean_tauhat = tau_mean/1000))
  }
  out[[m]] <- tab
}
dir.create("results/pension/cf", recursive = TRUE, showWarnings = FALSE)
saveRDS(list(tables = out, fits_ate = lapply(fits, average_treatment_effect)),
        "results/pension/cf/pension_cf_table4_260929.rds")
cat("\nsaved: results/pension/cf/pension_cf_table4_260929.rds\n")
