## ============================================================
## Simulation 2 (Section 3.2; Tables 2-3 and Appendix D): one replication of the eight scenarios.
## Same model set as 02_sim1_kang.R.
## Usage (from the repository root): Rscript scripts/sim/03_sim2_hahn.R <seed>     (seeds 1-1000; data from 01_generate_data.R)
## Output: results/Hahn_v2/R1000_260617/{LSBP_1_mixture,BCF,CF}/test_results_df_*.rds; seeds 1-2 also save the full fits.
## ============================================================

start_time <- Sys.time()

source("R/requirements.R")
library(bcf)   # (also attached in R/requirements.R)
source("R/utils.R")
source("R/dgp.R")
source("R/wrappers.R")

Rcpp::sourceCpp(paste0("src/lsbp_1_mixture.cpp"))

args <- commandArgs(trailingOnly = TRUE)
seed <- as.numeric(args[1])
seed_str = str_sub(paste0('000', seed), start = -4)
set.seed(533 * seed)   # global seed per replication: restores the seeding formerly done inside the DGP and fixes the whole RNG stream -> reproducible and independent across seeds. BCF random_seed and the CF seed are also drawn from this global stream (verified)
print(paste0("Seed index ", seed, " start at ", start_time))

### 0. Basic settings ###

date <- "R1000_260617"   # output folder
dir_result <- paste0("results/Hahn_v2/", date, "/")

## 1. Problems ##

linearity_options <- c("lin", "non")
TE_types <- c("hom", "het")
n_s <- c(250, 500)
# problem_df: the eight scenarios (linear/nonlinear mu x homogeneous/heterogeneous tau x n = 250, 500)
load(file = paste0("config/hahn_problem_df.Rdata"))

## 2. Models and priors ##

models <- c(
  "LSBP_1_mixture",
  "BCF"
)

# prior settings considered (for reference; the settings used are loaded from config/hahn_prior_pool.Rdata)
prior_condition <- list(
  LSBP_1_mixture = list(
    DOF_XOUT = c(0, 5), SCALE_XOUT = c(1),
    DOF_THETA = c(0, 5), SCALE_THETA = c(1),
    DOF_SIGMA = c(5, 2*3, 2*5, 2*10, 2*20, 2*50),
    SCALE_SIGMA = c(1, 1/3, 1/5, 1/10, 1/20, 1/50)
  ),
  BCF = list(
    option = "default"
  )
)

load(file = paste0("config/hahn_prior_pool.Rdata"))

## 2.1.2 Choose priors to run (filter)
## 4 comparators: cLSBP & BCF, each with plugin {TRUE, FALSE}
{
  test_priors_list <- list()
  test_priors_list[["LSBP_1_mixture"]] <- prior_pool[["LSBP_1_mixture"]] %>% filter(
    (DOF_XOUT == 5 & SCALE_XOUT == 1 & DOF_THETA == 0 & DOF_SIGMA == 10)  ## sig_10, both plugin values
  )
  test_priors_list[["BCF"]] <- prior_pool[["BCF"]] %>% filter(
    T  ## both plugin values
  )
}

posterior_samplers <- list(
  LSBP_1_mixture = get_posterior_lsbp_1_mixture,
  BCF = get_posterior_bcf_default
)

## additional dimensions: PS estimator, truncation level H
ps_methods <- c("BART", "GLM")
H_list_LSBP <- c(10, 20, 30)   # truncation levels; the paper reports H = 20
NSIM <- 4000                        # 2000 burn-in + 2000 kept
RET  <- 2001:4000

## 3. Run and save ##

## create result folders if absent (avoids gzfile errors)
for(m in models){
  dir.create(paste0(dir_result, m), recursive = TRUE, showWarnings = FALSE)
}

for(problem_idx in seq(nrow(problem_df))){  ## 1. Problems
  problem_info <- problem_df[problem_idx, ]
  simulation_name = "Hahn_v2"

  for(model in models){                     ## 2. Model
    prior_df <- test_priors_list[[model]]
    if(nrow(prior_df) == 0){next}

    for(idx in seq(nrow(prior_df))){        ## 3. prior row (includes plugin)
      prior_info <- prior_df[idx, ]
      plugin <- prior_info$plugin

      ### prior naming ###
      if(str_detect(model, "LSBP")){
        beta_prior = (prior_info$DOF_THETA == 0)
        beta_prior_names <- c()
        beta_prior_names$data = ifelse(beta_prior, "theta_hs_", "theta_ig_")
        b_prior = (prior_info$DOF_XOUT == 0)
        b_prior_names <- c()
        b_prior_names$data = ifelse(b_prior, "xout_hs_", "xout_ig_")
        sigma_prior_names = paste0("sig_", prior_info$DOF_SIGMA, "_")
        prior_name_data <- paste0(beta_prior_names$data, b_prior_names$data,
                                  sigma_prior_names, "pid", prior_info$prior_id, "_")
      } else {
        prior_name_data <- "default"
      }

      ## Hahn_v2 data path (lin/het/n_size)
      linear = (problem_info$linearity == "lin")
      hetero = (problem_info$TE_type == "het")
      n_size = (problem_info$n_size)
      lin_non = ifelse(linear, 'lin', 'non')
      het_hom = ifelse(hetero, 'het', 'hom')

      NAME = paste0('data/', simulation_name, '/d', seed_str, '_',
                    lin_non, '_', het_hom, '_', n_size, '.csv')
      problem_name <- paste0("Hahn_v2_",
                             lin_non, "_", het_hom, "_", n_size, "_",
                             "seed", seed, "_")

      data <- read.csv(file = NAME, header = TRUE)
      y <- data$y
      y_sc <- scale(y)
      sc_y <- attr(y_sc, "scaled:scale")
      ctr_y <- attr(y_sc, "scaled:center")
      tau <- data$tau
      mu <- data$mu
      true_pi <- data$pi
      z <- data$z
      x <- cbind(rep(1, n_size), as.matrix(data[, 2 + 1:5]) %>% scale)   # Hahn: 5 covariates, n_size

      ## ============ PS loop ============
      ps_loop <- if(plugin) ps_methods else c("BART")  # without plugin the PS estimator is irrelevant -> run once (no duplicates)
      for(ps_method in ps_loop){
        estimated_pi <- estimate_pi(method = ps_method, z, x)
        if(plugin){
          x_plugin <- cbind(x, estimated_pi)
          x_alpha <- x_plugin; x_beta <- x_plugin
          x_gamma <- x_plugin; x_out <- x_plugin
        } else {
          x_alpha <- x; x_beta <- x
          x_gamma <- x; x_out <- x
        }
        design_name_data <- paste0("x", dim(x)[2]-1, "_x", dim(x)[2]-1, ifelse(plugin, "_plugin", ""))

        ## ============ cLSBP: loop over H / BCF: single run ============
        H_list <- if(str_detect(model, "LSBP")) H_list_LSBP else c(NA)

        for(H_val in H_list){
          ## design arguments
          if(model == "LSBP_1_mixture"){
            design_args <- list(Y = y_sc, Z = z,
                                X_out = x_out, X_beta = x_beta, X_gamma = x_gamma)
          } else if(model == "BCF"){
            design_args <- list(y = y, Z = z, X = x,
                                pihat = estimated_pi, include_pi = ifelse(plugin, "control", "none"))
          }

          ## file name: PS tag and (cLSBP) H tag
          ps_tag <- if(plugin) paste0("_", ps_method) else "_noPS"  # without plugin the PS estimator is irrelevant
          H_tag  <- if(str_detect(model, "LSBP")) paste0("_H", H_val) else ""
          test_name_data <- paste0(problem_name, prior_name_data, design_name_data, ps_tag, H_tag)
          Rdata_save_dir <- paste0(dir_result, model, "/", test_name_data, ".Rdata")

          ## RUN
          if(str_detect(model, "LSBP")){
            test <- posterior_samplers[[model]] %>% do.call(c(design_args,
                      prior_info %>% select(-c(prior_id, plugin)) %>% as.list,
                      list(nsim = NSIM, H = H_val, printEvery = NSIM + 1)))
          } else if(model == "BCF"){
            test <- posterior_samplers[[model]] %>% do.call(c(design_args,
                      list(nskip = 0, nsim = NSIM),
                      list(printEvery = NSIM + 1)))
          }

          ## SAVE fit (seeds 1 and 2 only -- kept as representatives for convergence diagnostics)
          if(seed %in% 1:2){
            save(test, file = Rdata_save_dir)
          }

          ## summaries (ret = 2001:4000, i.e., the last 2000 draws)
          test_results_df <- data.frame()
          for(target in c("ATE", "CATE")){
            for(confi in c(80, 95)){
              for(what_name in c("tau", "mu")){
                result_data <- coverage_length_RMSE_function(
                  model = model, target = target, test = test,
                  ret = RET, what = data[[what_name]], what_name = what_name,
                  confi = confi, Y_sc = sc_y, Y_ctr = ctr_y
                ) %>% t %>% as.data.frame %>%
                  transmute(coverage = V1, length = V2, RMSE = V3)
                test_result <- cbind(target, confi, what_name, result_data)
                test_results_df <- test_results_df %>% rbind(test_result)
              }
            }
          }
          saveRDS(test_results_df,
                  file = paste0(dir_result, model, "/", "test_results_df_", test_name_data, ".rds"))
          rm(test)
        } ## end of H loop
      }   ## end of PS loop
    }
  }
}

## ============ causal forest (separate block) ============
## same estimands as cLSBP/BCF (CATE/ATE) but different outputs and summaries -> separate block
## PS supplied through W.hat (BART or GLM) or estimated internally by grf (default); plugin/H/prior do not apply
dir.create(paste0(dir_result, "CF"), recursive = TRUE, showWarnings = FALSE)
for(problem_idx in seq(nrow(problem_df))){
  problem_info <- problem_df[problem_idx, ]
  linear = (problem_info$linearity == "lin")
  hetero = (problem_info$TE_type == "het")
  n_size = (problem_info$n_size)
  lin_non = ifelse(linear, 'lin', 'non')
  het_hom = ifelse(hetero, 'het', 'hom')

  NAME = paste0('data/Hahn_v2/d', seed_str, '_',
                lin_non, '_', het_hom, '_', n_size, '.csv')
  data <- read.csv(file = NAME, header = TRUE)
  y <- data$y; z <- data$z
  true_tau <- data$tau
  x <- cbind(rep(1, n_size), as.matrix(data[, 2 + 1:5]) %>% scale)   # Hahn: 5 covariates, n_size

  problem_name <- paste0("Hahn_v2_",
                         lin_non, "_", het_hom, "_", n_size, "_",
                         "seed", seed, "_")

  for(ps_method in c("BART", "GLM", "default")){
    if(ps_method == "default"){
      # standard grf usage: without W.hat, grf estimates the PS with its own regression forest
      # num.trees: grf default (2000), consistent with "standard usage"
      cf <- causal_forest(X = x[, -1], Y = y, W = z)
    } else {
      # same PS as supplied to cLSBP/BCF (fair comparison): BART/GLM PS passed as W.hat
      epi <- estimate_pi(method = ps_method, z, x)
      cf <- causal_forest(X = x[, -1], Y = y, W = z, W.hat = epi)
    }
    pr_cf <- predict(cf, estimate.variance = TRUE)
    est <- pr_cf$predictions
    se  <- sqrt(pr_cf$variance.estimates)

    cf_df <- data.frame()
    for(confi in c(80, 95)){
      zq <- qnorm(0.5 + confi/200)
      # CATE
      cov_cate <- mean((est - zq*se < true_tau) & (true_tau < est + zq*se))
      len_cate <- mean(2 * zq * se)
      rmse_cate <- sqrt(mean((est - true_tau)^2))
      cf_df <- rbind(cf_df, data.frame(target = "CATE", confi = confi, what_name = "tau",
                                       coverage = cov_cate, length = len_cate, RMSE = rmse_cate))
      # ATE (doubly robust, as recommended by grf)
      ate <- average_treatment_effect(cf)
      ate_est <- unname(ate["estimate"]); ate_se <- unname(ate["std.err"])
      true_ate <- mean(true_tau)
      cov_ate <- (ate_est - zq*ate_se < true_ate) & (true_ate < ate_est + zq*ate_se)
      cf_df <- rbind(cf_df, data.frame(target = "ATE", confi = confi, what_name = "tau",
                                       coverage = as.numeric(cov_ate), length = 2*zq*ate_se,
                                       RMSE = abs(ate_est - true_ate)))
    }
    cf_name <- paste0(problem_name, "CF_", ps_method)
    saveRDS(cf_df, file = paste0(dir_result, "CF/", "test_results_df_", cf_name, ".rds"))
  }
}

end_time = Sys.time()
print(paste0("Seed index ", seed, " end at ", end_time))
