## ============================================================
## 401(k) analysis (Section 4.2): cLSBP fit to the full sample, and the BART propensity score used by all methods.
## The argument selects a prior / truncation case from case_list; the paper uses case 7
## (nu0 = 20, s0^2 = 0.025, 95% truncation), with H = 50 and 25,000 iterations.
## Usage: Rscript scripts/pension/pension_clsbp.R 7
## Output: results/pension/241026/test_lsbp_20_25_95.Rdata and pihat_BART_20_25_95.Rdata (summaries: pension_table4_figure6.R)
## ============================================================

start_time <- Sys.time()

source("R/requirements.R") # including RcppArmadillo
source("R/utils.R")
source("R/wrappers.R")
Rcpp::sourceCpp("src/lsbp_1_mixture.cpp")

args<-commandArgs(trailingOnly = TRUE)
case <- as.numeric(args[1])
print(paste0("case ", case, "starting at ", start_time))

case_list <- list(c(10, 0.2, 0.95), c(10, 0.1, 0.95), c(10, 0.05, 0.95), c(10, 0.02, 0.95),
                  c(20, 0.1, 0.95), c(20, 0.05, 0.95), c(20, 0.025, 0.95), c(20, 0.01, 0.95),
                  c(10, 0.2, 0.99), c(10, 0.1, 0.99), c(10, 0.05, 0.99), c(10, 0.02, 0.99),
                  c(20, 0.1, 0.99), c(20, 0.05, 0.99), c(20, 0.025, 0.99), c(20, 0.01, 0.99))

IG_DOF <- case_list[[case]][1]
IG_SCALE <- case_list[[case]][2]
Y_TRUNC <- case_list[[case]][3]
print(paste0("IG_DOF : ", IG_DOF, "IG_SCALE : ", IG_SCALE, ", Y_TRUNC : ", Y_TRUNC))

require(hdm)
data(pension)
pension <- pension %>% filter(e401==1)

Y <- pension$net_tfa
TRUNC_IDX <- abs(Y) < quantile(abs(Y), Y_TRUNC)
Z <- pension$p401[TRUNC_IDX] %>% as.numeric()
X <- pension %>% dplyr::select(a1, a2, a3, a4, i1, i2, i3, i4, i5, i6, fsize, nohs, hs, smcol, marr, twoearn, db, pira, hown) %>%
  mutate(intercept = 1) %>% as.matrix
X <- X[TRUNC_IDX, ]
X[, 11] <- scale(X[, 11]) # scale fsize. the only numerical value.

Y <- Y[TRUNC_IDX]
Y <- scale(Y)

centerY <- attr(Y, "scaled:center")
scaleY <- attr(Y, "scaled:scale")
attributes(Y) <- NULL
fit <- BayesTree::bart(X, Z, verbose = FALSE)
pihat <- pnorm(colMeans(fit$yhat.train))
save(pihat, file = paste0("results/pension/241026/pihat_BART_", IG_DOF, "_", IG_SCALE*1000, "_", 100*Y_TRUNC, ".Rdata"))
Xpi <- cbind(X, pihat)

test_pi <- get_posterior_lsbp_1_mixture(Y, Z, Xpi, Xpi, Xpi, 5, 1, 0, 1, IG_DOF, IG_SCALE, nskip = 0, nsim = 25000, H = 50, printEvery = 2000)

save(test_pi, file = paste0("results/pension/241026/test_lsbp_", IG_DOF, "_", IG_SCALE*1000, "_", 100*Y_TRUNC, ".Rdata"))

end_time <- Sys.time()
print(paste0("case ", case, "ending at ", end_time))
