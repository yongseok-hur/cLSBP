## ============================================================
## Table 5 (5-fold cross-validation), step 1: cLSBP fit on the training part of one fold
## (case 7: nu0 = 20, s0^2 = 0.025, 95% truncation; H = 40; 20,000 iterations).
## Usage: Rscript scripts/pension/pension_cv_fit.R <fold 1-5>
## Output: results/pension/CV_5fold/fold<k>/test_cLSBP_Y_95.Rdata
## ============================================================

start_time <- Sys.time()

source("R/requirements.R") # including RcppArmadillo
source("R/utils.R")
source("R/wrappers.R")
Rcpp::sourceCpp("src/lsbp_1_mixture.cpp")

args<-commandArgs(trailingOnly = TRUE)
fold_num <- as.numeric(args[1])
print(paste0("CV ", fold_num, "starting at ", start_time))

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

# 5-fold split of the 95%-truncated sample
{
# Run once (kept to document how config/pension_cv_folds.Rdata was created):
# create_balanced_folds <- function(n = 3497, k = 5, seed = 123) {
#   set.seed(seed)
#
#   # Calculate base size and remainder
#   base_size <- floor(n/k)
#   remainder <- n %% k
#
#   # Create a vector of fold sizes
#   fold_sizes <- rep(base_size, k)
#   # Add the remainder across folds
#   if(remainder > 0) {
#     fold_sizes[1:remainder] <- fold_sizes[1:remainder] + 1
#   }
#
#   # Create a vector of fold assignments
#   folds <- rep(1:k, times = fold_sizes)
#
#   # Randomly shuffle
#   shuffled_indices <- sample(1:n)
#
#   # Create the final fold assignments
#   final_folds <- numeric(n)
#   final_folds[shuffled_indices] <- folds
#
#   return(final_folds)
# }
# # Generate the folds
# n <- 3497
# k <- 5
# cv_folds <- create_balanced_folds(n = n, k = k)
# save(cv_folds, file = "config/pension_cv_folds.Rdata")
}
load(file = "config/pension_cv_folds.Rdata")

train_idx <- which(cv_folds != fold_num)
test_idx <- which(cv_folds == fold_num)

X_train <- X[train_idx, ]
Y_train <- Y[train_idx]
Z_train <- Z[train_idx]

Y_train <- scale(Y_train)
centerY_train <- attr(Y_train, "scaled:center")
scaleY_train <- attr(Y_train, "scaled:scale")
attributes(Y_train) <- NULL

# Run once (kept to document how the cached file loaded below was created):
# fit_train <- BayesTree::bart(X_train, Z_train, verbose = FALSE)
# pihat_train <- pnorm(colMeans(fit_train$yhat.train))
# save(pihat_train, file = paste0("results/pension/CV_5fold/fold", fold_num, "/pihat_", "train", "_BART_Y", 100*Y_TRUNC, ".Rdata"))
load(file = paste0("results/pension/CV_5fold/fold", fold_num, "/pihat_", "train", "_BART_Y", 100*Y_TRUNC, ".Rdata"))
Xpi_train <- cbind(X_train, pihat_train)

X_test <- X[test_idx, ]
Y_test <- Y[test_idx]
Z_test <- Z[test_idx]

Y_test <- scale(Y_test)
centerY_test <- attr(Y_test, "scaled:center")
scaleY_test <- attr(Y_test, "scaled:scale")
attributes(Y_test) <- NULL

# Run once (kept to document how the cached file loaded below was created):
# fit_test <- BayesTree::bart(X_test, Z_test, verbose = FALSE)
# pihat_test <- pnorm(colMeans(fit_test$yhat.train))
# save(pihat_test, file = paste0("results/pension/CV_5fold/fold", fold_num, "/pihat_", "test", "_BART_Y", 100*Y_TRUNC, ".Rdata"))
load(file = paste0("results/pension/CV_5fold/fold", fold_num, "/pihat_", "test", "_BART_Y", 100*Y_TRUNC, ".Rdata"))
Xpi_test <- cbind(X_test, pihat_test)

ready_to_run = Sys.time()
save(ready_to_run, file = paste0("results/pension/CV_5fold/cLSBP_ready_f", fold_num, ".Rdata"))

# Run once (kept to document how the BCF fits used by pension_cv_predict_bcf.R were created):
# test_bcf <- bcf(
#   y = Y_train, z = Z_train, x_control = X_train, pihat = pihat_train,
#   nburn = 5000, nsim = 15000,
#   update_interval = 5000,
#   no_output = F, save_tree_directory = paste0("./results/pension/CV_5fold/fold", fold_num), log_file = NULL,
#   include_pi = "control", verbose = F
# )
# save(test_bcf, file = paste0("results/pension/CV_5fold/fold", fold_num, "/test_bcf_Y_", 100*Y_TRUNC, ".Rdata"))

test_cLSBP <- get_posterior_lsbp_1_mixture(Y_train, Z_train, Xpi_train, Xpi_train, Xpi_train, 5, 1, 0, 1, IG_DOF, IG_SCALE, nskip = 0, nsim = 20000, H = 40, printEvery = 10000)

save(test_cLSBP, file = paste0("results/pension/CV_5fold/fold", fold_num, "/test_cLSBP_Y_", 100*Y_TRUNC, ".Rdata"))

end_time <- Sys.time()
save(end_time, file = paste0("results/pension/CV_5fold/cLSBP_done_f", fold_num, ".Rdata"))
