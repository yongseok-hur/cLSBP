## ============================================================
## Table 5, step 2 (cLSBP): posterior predictive distribution of one held-out household, from the fit of its fold
## (iterations 10,001-20,000).
## Usage: Rscript scripts/pension/pension_cv_predict_clsbp.R <household index 1-3497>
## Output: results/pension/CV_5fold/fold<k>/pred_cLSBP_CV_<index>.rds
## ============================================================

start_time <- Sys.time()

source("R/requirements.R") # including RcppArmadillo
source("R/utils.R")

args<-commandArgs(trailingOnly = TRUE)
subject_idx = as.numeric(args[1])

case_list <- list(c(10, 0.2, 0.95), c(10, 0.1, 0.95), c(10, 0.05, 0.95), c(10, 0.02, 0.95),
                  c(20, 0.1, 0.95), c(20, 0.05, 0.95), c(20, 0.025, 0.95), c(20, 0.01, 0.95),
                  c(10, 0.2, 0.99), c(10, 0.1, 0.99), c(10, 0.05, 0.99), c(10, 0.02, 0.99),
                  c(20, 0.1, 0.99), c(20, 0.05, 0.99), c(20, 0.025, 0.99), c(20, 0.01, 0.99))

case <- 7

IG_DOF <- case_list[[case]][1]
IG_SCALE <- case_list[[case]][2]
Y_TRUNC <- case_list[[case]][3]

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

load(file = "config/pension_cv_folds.Rdata")

# fold specification
# Note that for cLSBP, we predict the subject 1 by 1.
fold_num <- cv_folds[subject_idx]

train_idx <- which(cv_folds != fold_num)
test_idx <- which(cv_folds == fold_num)
test_idx_infold <- which(test_idx == subject_idx)

X_train <- X[train_idx, ]
Y_train <- Y[train_idx]
Z_train <- Z[train_idx]

Y_train <- scale(Y_train)
centerY_train <- attr(Y_train, "scaled:center")
scaleY_train <- attr(Y_train, "scaled:scale")
attributes(Y_train) <- NULL

load(file = paste0("results/pension/CV_5fold/fold", fold_num, "/pihat_", "train", "_BART_Y", 100*Y_TRUNC, ".Rdata"))
Xpi_train <- cbind(X_train, pihat_train)

X_test <- X[test_idx, ]
Y_test <- Y[test_idx]
Z_test <- Z[test_idx]

X_test <- X_test[test_idx_infold, ]
Y_test <- Y_test[test_idx_infold]
Z_test <- Z_test[test_idx_infold]

load(file = paste0("results/pension/CV_5fold/fold", fold_num, "/pihat_", "test", "_BART_Y", 100*Y_TRUNC, ".Rdata"))
pihat_test <- pihat_test[test_idx_infold]
Xpi_test <- c(X_test, pihat = pihat_test)

# cLSBP
# (pi-hat enters cLSBP only as a covariate)

# Run once (kept to document how the cached posterior draws loaded below were created):
# for(fold_num in 1:5){
#   load(file = paste0("results/pension/CV_5fold/fold", fold_num, "/test_cLSBP_Y_", 100*Y_TRUNC, ".Rdata"))
#
#   xout_param_list <- 1:40 %>% lapply(function(h){
#     (10001:20000) %>% sapply(function(iter){test_cLSBP[[iter]]$xout[[h]]$param})
#   })
#
#   theta_param_list <- 1:41 %>% lapply(function(h){
#     (10001:20000) %>% sapply(function(iter){test_cLSBP[[iter]]$theta[[h]]$param})
#   })
#
#   theta_sigma2_list <- 1:41 %>% lapply(function(h){
#     (10001:20000) %>% sapply(function(iter){test_cLSBP[[iter]]$theta[[h]]$sigma2})
#   })
#
#   saveRDS(xout_param_list, file = paste0("results/pension/CV_5fold/fold", fold_num, "_xout_param_list.rds"))
#   saveRDS(theta_param_list, file = paste0("results/pension/CV_5fold/fold", fold_num, "_theta_param_list.rds"))
#   saveRDS(theta_sigma2_list, file = paste0("results/pension/CV_5fold/fold", fold_num, "_theta_sigma2_list.rds"))
# }

xout_param_list <- readRDS(file = paste0("results/pension/CV_5fold/fold", fold_num, "_xout_param_list.rds"))
theta_param_list <- readRDS(file = paste0("results/pension/CV_5fold/fold", fold_num, "_theta_param_list.rds"))
theta_sigma2_list <- readRDS(file = paste0("results/pension/CV_5fold/fold", fold_num, "_theta_sigma2_list.rds"))

density_grid_ret <- 10001:20000 %>% sapply(function(iter_original){
  iter = iter_original-10000
  # calculate the weight by X^\top b_h.
  products_ex <- 1:40 %>% sapply(function(h){
    (Xpi_test * xout_param_list[[h]][, iter]) %>% sum
  })
  cumsum_log = 0
  probs_log = c()

  for(product in products_ex){
    probs_log = c(probs_log, cumsum_log + logSigmoid_r(product))
    cumsum_log = cumsum_log + logSigmoid_r(product) - product
  }
  probs_log = c(probs_log, cumsum_log)

  # calculate the mean and variance of the normal, by X^\top beta_h

  means_iter <- 1:41 %>% sapply(function(h){
    (c(Xpi_test, Z_test*Xpi_test) * theta_param_list[[h]][, iter]) %>% sum
  })
  sigma2s_iter <- 1:41 %>% sapply(function(h){theta_sigma2_list[[h]][iter]})
  density_grid <- seq(-5, 5, by = 0.01) %>% sapply(function(x){
    dnorm(x = x, mean = means_iter, sd = sqrt(sigma2s_iter))
  }) %>% sweep(1, get_prob_from_logsigmoid(probs_log), FUN = '*') %>% colSums

  if(iter_original == 15001){
    half_time = Sys.time()
    saveRDS(half_time, file = paste0("results/pension/CV_5fold/status/fold", fold_num, "_", subject_idx, ".rds"))
    }

  density_grid
})

posterior_predictive <- density_grid_ret %>% apply(2, function(densities){
  sample(seq(-5, 5, by = 0.01), 1, prob = densities)
})

saveRDS(posterior_predictive, file = paste0("results/pension/CV_5fold/fold", fold_num, "/pred_cLSBP_CV_", subject_idx, ".rds"))
