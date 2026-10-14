## ============================================================
## 2022 ACIC data challenge (Section 4.1): cLSBP (H = 12, 6,000 iterations, first 3,000 discarded) with each
## propensity score estimator (BART, CBPS, GBM, LASSO, none) for one of the 3,400 practice-level datasets.
## Usage: Rscript scripts/acic/acic_run.R <dataset index 1-3400>   (all datasets: acic_run.sh)
## Needs the challenge data (not redistributed here); output: results/ACIC_2022/<date>/...
## ============================================================

start_time <- Sys.time()

source("R/requirements.R")
source("R/utils.R")
source("R/wrappers.R")

Rcpp::sourceCpp("src/lsbp_1_mixture.cpp")

citation("gbm")

args<-commandArgs(trailingOnly = TRUE)
dataset_idx <- as.numeric(args[1])
print(paste0("Dataset index ", dataset_idx, "/3400 start at ", start_time))

RNGkind("L'Ecuyer-CMRG")   # Random Number Generator
base_seed <- 533L          # Random Base Seed
set.seed(base_seed + dataset_idx)

### 0. Basic settings ###

# Prior of sigma_h^2: IG(5, 1), i.e. nu0 = 10, s0^2 = 0.2. The output folder below is that of the run used for Figures 2-5.
date <- "251203" # output folder results/ACIC_2022/<date>/, read by acic_score_figures.R and acic_figures_revised.R
dir_result <- paste0("results/ACIC_2022/", date, "/")

### 1. Problems ###

## Filter the realization number from ACIC_estimand_truths.csv

ACIC_truths <- read.csv("data/ACIC_estimand_truths.csv")

# Run once (kept to document how problem_df.Rdata was created):
# {
# problem_df <- ACIC_truths %>% filter(
# ) %>% group_by(
#   dataset.num,
#   Confounding.Strength, Confounding.Source, Impact.Heterogeneity, Idiosyncrasy.of.Impacts
# ) %>% summarise(
#   n_results = n()
# ) %>% arrange(Confounding.Strength, Impact.Heterogeneity)
# save(problem_df, file = paste0(dir_result, "problem_df.Rdata"))
# }
# The stored problem_df is loaded:
load(file = paste0(dir_result, "problem_df.Rdata"))

dataset_num = problem_df$dataset.num[dataset_idx]
answer_truth <- ACIC_truths %>% filter(dataset.num == dataset_num) %>% head(15)
rm(ACIC_truths)

### 2. Models and priors ###

models <- c(
  "LSBP_1_mixture"
)

## 2.1. Priors ##

## 2.1.1 Prior pools
## Caution : The prior_id corresponds to its prior_pool!

# prior condition list
{
  # Run once (kept to document how prior_pool.Rdata was created):
  # prior_condition <- list(
  #   LSBP_1_mixture = list(
  #     DOF_XOUT = c(5, 2.5, 10), SCALE_XOUT = c(1, 2),  ## DOF == 0 means horseshoe prior, otherwise inverse-gamma prior
  #     DOF_THETA = c(0), SCALE_THETA = c(1),
  #     DOF_SIGMA = c(5, 2*3, 2*5, 2*10, 2*20, 2*50),
  #     SCALE_SIGMA = c(1, 1/3, 1/5, 1/10, 1/20, 1/50)
  #     # THETA == (BETA, GAMMA)
  #   )
  # )
  #
  # prior_pool <- names(prior_condition) %>% lapply(function(model_name){
  #   expanded_df <- expand.grid %>% do.call(
  #     prior_condition[[model_name]]
  #   )
  #   if(model_name == "LSBP_1_mixture"){
  #     priors <- expanded_df %>% filter(((DOF_SIGMA == 5 & SCALE_SIGMA == 1) | (DOF_SIGMA * SCALE_SIGMA) == 2)) %>% mutate(prior_id = row_number())
  #   }
  #   else{
  #     priors <- expanded_df %>% mutate(prior_id = row_number())
  #   }
  #   rbind(priors %>% mutate(plugin = "BART"),
  #         priors %>% mutate(plugin = "CBPS"),
  #         priors %>% mutate(plugin = "LASSO"),
  #         priors %>% mutate(plugin = "GBM"),
  #         priors %>% mutate(plugin = NA)
  #   )
  # })
  #
  # names(prior_pool) <- names(prior_condition)
  #
  # save(prior_pool, file = paste0(dir_result, "prior_pool.Rdata"))
}

# The stored prior_pool is loaded:
load(file = paste0(dir_result, "prior_pool.Rdata"))

## 2.1.2 Choose priors to run
## Put constraints (filter)
{
  test_priors_list <- list()
  test_priors_list[["LSBP_1_mixture"]] <- prior_pool[["LSBP_1_mixture"]] %>% filter(
    (DOF_XOUT == 5 & SCALE_XOUT == 1), ## or just prior_id == 1
    DOF_SIGMA == 10
  )
}

## 2.1.3 Posterior samplers (for each model)
posterior_samplers <- list(
  LSBP_1_mixture = get_posterior_lsbp_1_mixture
)

## 3. Run and save ##

## Run ##

dataset_num_str = str_sub(paste0('000', dataset_num), start = -4)
dataset_row = problem_df %>% filter(dataset.num == dataset_num) %>% dplyr::select(-n_results)

## dgp, Design matrix ##
# 1
practice <- read.csv(paste0('data/acic_practice/acic_practice_', dataset_num_str, '.csv')) %>% mutate(
  X2_A = as.numeric(X2 == 'A'),
  X2_B = as.numeric(X2 == 'B'),
  X4_A = as.numeric(X4 == 'A'),
  X4_B = as.numeric(X4 == 'B')
) %>% transform(
  X6 = scale(X6),
  X7 = scale(X7),
  X8 = scale(X8),
  X9 = scale(X9)
) %>% dplyr::select(-X2, -X4)

# 2
practice_year <- read.csv(paste0('data/acic_practice_year/acic_practice_year_', dataset_num_str, '.csv')) %>% mutate(
  year_2 = as.numeric(year == 2),
  year_3 = as.numeric(year == 3),
  year_4 = as.numeric(year == 4),
  Z_p = Z * post
) %>% transform(
  V1_avg = scale(V1_avg),
  V2_avg = scale(V2_avg),
  V3_avg = scale(V3_avg),
  V4_avg = scale(V4_avg)
) %>% dplyr::select(-year, -V5_C_avg) #  -post, Z
n.p = practice_year$n.patients
practice_year <- practice_year %>% dplyr::select(-n.patients)

# 3
joined <- left_join(practice, practice_year, by = "id.practice")

Y <- joined$Y
Y_sc <- scale(Y)
Z <- joined$Z
X <- joined %>% dplyr::select(-c(id.practice, Y, Z, post, Z_p)) %>% mutate(intercept = 1) %>% as.matrix
first_second <- joined %>% filter(!year_3, !year_4)
first_second_info <- first_second %>%
  mutate(year = as.numeric(year_2) + 1) %>%
  pivot_wider(id_cols = id.practice, names_from = year, values_from = c("Y", ends_with("_avg")))
X_pi <- first_second %>% filter(!year_2, !year_3, !year_4) %>%
  dplyr::select(-c(Y, Z, post, ends_with("_avg"), starts_with("year"), Z_p)) %>%
  left_join(first_second_info, by = "id.practice") %>%
  dplyr::select(-id.practice) %>% as.matrix
Z_X <- Z[4*(1:500)]

## To save the first realization for each Scenario, get the dataset number of it.
# Run once (kept to document how d_n_vec.Rdata was created):
# d_n_vec <- (problem_df %>% arrange(dataset.num) %>% head(100) %>% group_by(Confounding.Strength, Confounding.Source, Impact.Heterogeneity, Idiosyncrasy.of.Impacts) %>% summarise(dataset.num = min(dataset.num)) %>% ungroup)$"dataset.num"
# save(d_n_vec, file = "d_n_vec.Rdata")
load(file = "d_n_vec.Rdata") # 26  1  3 14  6 16 19  4 25 11 10 34  2  5 13  8 17

for(model in models){
  prior_df <- test_priors_list[[model]]
  if(nrow(prior_df)==0){next}

  for(idx in seq(nrow(prior_df))){
    prior_info <- prior_df[idx, ]
    plugin <- prior_info$plugin

    ### prior specification ###

    if(str_detect(model, "LSBP")){
      beta_prior = (prior_info$DOF_THETA == 0)
      beta_prior_names <- c()
      beta_prior_names$data = ifelse(beta_prior,
                                     "theta_hs_",
                                     "theta_ig_")
      beta_prior_names$plot = ifelse(beta_prior,
                                     "$theta$ = HS; ",
                                     "$theta$ = IG; ")

      b_prior = (prior_info$DOF_XOUT == 0)
      b_prior_names <- c()
      b_prior_names$data = ifelse(b_prior,
                                  "xout_hs_",
                                  "xout_ig_")
      b_prior_names$plot = ifelse(b_prior,
                                  "$b$ = HS; ",
                                  "$b$ = IG; ")

      sigma_prior_names = paste0("sig_", prior_info$DOF_SIGMA, "_")

      (prior_name_data <- paste0(beta_prior_names$data, b_prior_names$data, sigma_prior_names, "pid", prior_info$prior_id, "_"))
      (prior_name_plot <- paste0(beta_prior_names$plot, b_prior_names$plot))
    }
    else{
    }

    prior_id <- prior_df$prior_id[idx]

    ## dgp, Design matrix ##

    # estimate pi for plug-in methods & construct design matrices.
    if(!is.na(plugin)){
      estimated_pi <- estimate_pi(method = plugin, Z_X, X_pi)
      X_plugin <- X %>% cbind(pi_est = rep(estimated_pi, each = 4))

      x_out <- X_plugin
      x_beta <- X_plugin
      x_gamma <- joined$post * (X_plugin %>% as.data.frame %>% dplyr::select(-c(year_2, year_3)) %>% as.matrix)
    }else{
      x_out <- X
      x_beta <- X
      x_gamma <- joined$post * (X %>% as.data.frame %>% dplyr::select(-c(year_2, year_3)) %>% as.matrix)
    }

    ## overall names ##
    test_name_data <- paste0("test_", dataset_num_str, "_", model, "_prior_", prior_id, ifelse(!is.na(plugin), paste0("_", plugin), ""))
    Rdata_save_dir <- paste0(dir_result, dataset_row$Confounding.Strength, "_", dataset_row$Impact.Heterogeneity, "/", model, "/", test_name_data, ".rds")

    ## design arguments ##
    if(model == "LSBP_1_mixture"){
      design_args <- list(Y = Y_sc, Z = Z,
                          X_out = x_out,
                          X_beta = x_beta, X_gamma = x_gamma
      )
    }

    ## RUN ##

    if(str_detect(model, "LSBP")){
      test <- posterior_samplers[[model]] %>% do.call(c(design_args,
                                                        prior_info %>% dplyr::select(-c(prior_id, plugin)) %>% as.list,
                                                        list(nsim = 6000, H = 12, printEvery = ifelse(dataset_idx %% 50 == 1, 5000, 10000))))
    }else if(model == "BCF"){
      test <- posterior_samplers[[model]] %>% do.call(c(design_args,
                                                        list(nskip = 0, nsim = 6000),
                                                        list(printEvery = 5000)))
    }

    ## Save the fit (first realization of each scenario only) ##

    if(dataset_num %in% d_n_vec){ # Only for the first realization, for each Scenario.
      saveRDS(test, file = Rdata_save_dir)
    }

    ## Save the answers ##

    ## Extracting results(RMSE, coverage) ##

    (sc_Y <- attr(Y_sc, "scaled:scale"))
    (ctr_Y <- attr(Y_sc, "scaled:center"))

    tau_post <- 1:6000 %>% sapply(function(iter) test[[iter]]$tau) * sc_Y

    if(dataset_num %in% d_n_vec){
      saveRDS(tau_post, file = paste0(dir_result, dataset_row$Confounding.Strength, "_", dataset_row$Impact.Heterogeneity, "/", model, "/", test_name_data, "_tau_post.rds"))
    }

    CATEs <- tau_post[, 3001:6000]
    colnames(CATEs) <- paste0("C", 1:ncol(CATEs))

    df_joined <- joined %>% cbind(n_p = n.p)

    df_with_results_treated <- df_joined %>% cbind(CATEs) %>% filter(Z_p==1) %>%
      mutate(X2 = ifelse(X2_A, "A", ifelse(X2_B, "B", "C")),
             X4 = ifelse(X4_A, "A", ifelse(X4_B, "B", "C")),
             year = ifelse(year_3, 3, 4) # values should be 3 or 4
      ) %>% dplyr::select(-c(X2_A, X2_B, X4_A, X4_B))

    grouped_results_overall <- df_with_results_treated %>%
      group_by() %>%
      summarise(across(starts_with("C"), ~weighted.mean(., n_p))) %>%
      ungroup()

    grouped_results_year <- df_with_results_treated %>%
      group_by(year) %>%
      summarise(across(starts_with("C"), ~weighted.mean(., n_p))) %>%
      ungroup()

    grouped_results_X1 <- df_with_results_treated %>%
      group_by(X1) %>%
      summarise(across(starts_with("C"), ~weighted.mean(., n_p))) %>%
      ungroup()

    grouped_results_X2 <- df_with_results_treated %>%
      group_by(X2) %>%
      summarise(across(starts_with("C"), ~weighted.mean(., n_p))) %>%
      ungroup()

    grouped_results_X3 <- df_with_results_treated %>%
      group_by(X3) %>%
      summarise(across(starts_with("C"), ~weighted.mean(., n_p))) %>%
      ungroup()

    grouped_results_X4 <- df_with_results_treated %>%
      group_by(X4) %>%
      summarise(across(starts_with("C"), ~weighted.mean(., n_p))) %>%
      ungroup()

    grouped_results_X5 <- df_with_results_treated %>%
      group_by(X5) %>%
      summarise(across(starts_with("C"), ~weighted.mean(., n_p))) %>%
      ungroup()

    results_list <- list(
      grouped_results_overall, grouped_results_year,
      grouped_results_X1, grouped_results_X2, grouped_results_X3, grouped_results_X4, grouped_results_X5
    ) %>% sapply(
      .%>% dplyr::select(starts_with("C")) %>% apply(1, .%>% quantile(probs = c(0.05, 0.5, 0.95))) %>% t
    )
    results <- rbind %>% do.call(results_list)

    answer <- answer_truth %>% cbind(results) %>% mutate(
      Scenario = str_c(c(Confounding.Strength, Confounding.Source, Impact.Heterogeneity, Idiosyncrasy.of.Impacts), collapse = "/"),
      model = model,
      prior_id = prior_id,
      plugin = plugin,
      SATT_truth = SATT
    ) %>% dplyr::select(-c(
      SATT, Confounding.Strength, Confounding.Source, Impact.Heterogeneity, Idiosyncrasy.of.Impacts, id.practice
      )) %>% dplyr::select(Scenario, dataset.num, everything())
    answer$'bias' = answer$'50%' - answer$SATT_truth
    answer$'covered' = (answer$SATT_truth > answer$'5%')&(answer$SATT_truth < answer$'95%')
    saveRDS(answer, file = paste0(dir_result, dataset_row$Confounding.Strength, "_", dataset_row$Impact.Heterogeneity, "/", model, "/", test_name_data, "_answers.rds"))

    ## Save practice-level SATTs (# of practices x 3 dataframe)
    # practice-level effect: average over the two post-treatment years, weighted by the number of patients n_p
    practice_results <- df_with_results_treated %>%
      group_by(id.practice) %>%
      summarise(across(starts_with("C"), ~weighted.mean(., n_p))) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(
        '5%' = quantile(c_across(starts_with("C")), 0.05),
        '50%' = quantile(c_across(starts_with("C")), 0.50),
        '95%' = quantile(c_across(starts_with("C")), 0.95)
      ) %>%
      ungroup() %>%
      dplyr::select('5%', '50%', '95%')

    saveRDS(practice_results, file = paste0(dir_result, dataset_row$Confounding.Strength, "_", dataset_row$Impact.Heterogeneity, "/", model, "/", test_name_data, "_answers_practice.rds"))

    # remove the test from the environment
    rm(test)
  }
}

end_time <- Sys.time()
print(paste0("Dataset index ", dataset_idx, "/3400 end at ", end_time))
