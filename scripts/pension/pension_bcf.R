## ============================================================
## 401(k) analysis (Section 4.2): BCF fit to the full sample (four chains, 5,000 burn-in + 25,000 draws each),
## with the BART propensity score saved by pension_clsbp.R.
## Usage: Rscript scripts/pension/pension_bcf.R
## Output: results/pension/241112/test_bcf_Y_95.Rdata (summaries: pension_table4_figure6.R)
## ============================================================

start_time <- Sys.time()

source("R/requirements.R") # including RcppArmadillo
source("R/utils.R")
source("R/wrappers.R")

case <- "ABCD"
print(paste0("case ", case, "starting at ", start_time))

Y_TRUNC <- 0.95

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

load(file = paste0("results/pension/241026/pihat_BART_20_25_", 100*Y_TRUNC, ".Rdata"))
Xpi <- cbind(X, pihat)

test_bcf <- bcf(
  y = Y, z = Z, x_control = X, pihat = pihat,
  n_threads = 50,
  nburn = 5000, nsim = 25000,
  update_interval = 5000,
  no_output = F, save_tree_directory = "./results/pension/bcf_Y95", log_file = NULL,
  include_pi = "control", verbose = F
  )
save(test_bcf, file = paste0("results/pension/241112/test_bcf_Y_", 100*Y_TRUNC, ".Rdata"))

end_time <- Sys.time()
print(paste0("case ", case, "ending at ", end_time))
