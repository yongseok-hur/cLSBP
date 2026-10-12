## ============================================================
## Table 5, step 2 (BCF): predictions for the held-out households of one fold.
## Usage: Rscript scripts/pension/pension_cv_predict_bcf.R <fold 1-5>
## Output: results/pension/CV_5fold/pred_bcf_CV_<fold>.Rdata
## ============================================================

start_time <- Sys.time()

source("R/requirements.R") # including RcppArmadillo
source("R/wrappers.R")

args<-commandArgs(trailingOnly = TRUE)
fold_num <- as.numeric(args[1])

case_list <- list(c(10, 0.2, 0.95), c(10, 0.1, 0.95), c(10, 0.05, 0.95), c(10, 0.02, 0.95),
                  c(20, 0.1, 0.95), c(20, 0.05, 0.95), c(20, 0.025, 0.95), c(20, 0.01, 0.95),
                  c(10, 0.2, 0.99), c(10, 0.1, 0.99), c(10, 0.05, 0.99), c(10, 0.02, 0.99),
                  c(20, 0.1, 0.99), c(20, 0.05, 0.99), c(20, 0.025, 0.99), c(20, 0.01, 0.99))

case <- 7

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

load(file = "config/pension_cv_folds.Rdata")

# fold specification

load(file = paste0("results/pension/CV_5fold/fold", fold_num, "/test_bcf_Y_", 100*Y_TRUNC, ".Rdata"))

train_idx <- which(cv_folds != fold_num)
test_idx <- which(cv_folds == fold_num)

X_train <- X[train_idx, ]
Y_train <- Y[train_idx]
Z_train <- Z[train_idx]

Y_train <- scale(Y_train)
centerY_train <- attr(Y_train, "scaled:center")
scaleY_train <- attr(Y_train, "scaled:scale")
attributes(Y_train) <- NULL

X_test <- X[test_idx, ]
Y_test <- Y[test_idx]
Z_test <- Z[test_idx]

Y_test <- scale(Y_test)
centerY_test <- attr(Y_test, "scaled:center")
scaleY_test <- attr(Y_test, "scaled:scale")
attributes(Y_test) <- NULL

load(file = paste0("results/pension/CV_5fold/fold", fold_num, "/pihat_", "test", "_BART_Y", 100*Y_TRUNC, ".Rdata"))
Xpi_test <- cbind(X_test, pihat_test)

# BCF
# BCF needs the propensity score of the test units

save(start_time, file = paste0("results/pension/CV_5fold/bcf_predict_start_f", fold_num, ".Rdata"))

pred_bcf_CV = predict(object=test_bcf,
                      x_predict_control=X_test,
                      x_predict_moderate=X_test,
                      pi_pred=pihat_test,
                      z_pred=Z_test,
                      n_cores = 80,
                      save_tree_directory = paste0("./results/pension/CV_5fold/fold", fold_num))

save(pred_bcf_CV, file = paste0("results/pension/CV_5fold/pred_bcf_CV_", fold_num, ".Rdata"))

end_time <- Sys.time()
save(end_time, file = paste0("results/pension/CV_5fold/bcf_predict_done_f", fold_num, ".Rdata"))
