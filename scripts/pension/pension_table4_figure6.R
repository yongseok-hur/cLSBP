## ============================================================
## 401(k) analysis (Section 4.2): Table 4 (cLSBP and BCF columns) and Figure 6 from the stored fits.
##
## Inputs (not included in the repository, large):
##   results/pension/241026/test_lsbp_20_25_95.Rdata  and  pihat_BART_20_25_95.Rdata   <- pension_clsbp.R, case 7
##   results/pension/241112/test_bcf_Y_95.Rdata                                         <- pension_bcf.R
## Posterior draws used:
##   cLSBP: 25,000 iterations; Table 4 and Figure 6 use iterations 15,001-25,000 (10,000 draws).
##   BCF: four chains, each with 5,000 burn-in and 25,000 saved draws; Table 4 uses saved draws 10,001-20,000
##        of each chain (40,000 draws).
## Usage (from the repository root): Rscript scripts/pension/pension_table4_figure6.R
## ============================================================
source("R/requirements.R") # including RcppArmadillo
source("R/wrappers.R")
library(glue)

require(hdm)
data(pension)
pension <- pension %>% filter(e401==1)

TRUNC_95_IDX <- abs(pension$net_tfa)<quantile(abs(pension$net_tfa), 0.95)
grouped_data_95_original <- pension[TRUNC_95_IDX, ] %>%
  group_by(across(c(a1:a4, i1:i6, fsize, nohs, hs, smcol, marr, twoearn, db, pira, hown, p401)))

## ---- cLSBP posterior draws ----
IG_DOF = 20
IG_SCALE = 0.025
Y_TRUNC = 0.95
Y <- pension$net_tfa
TRUNC_IDX <- abs(Y) < quantile(abs(Y), Y_TRUNC)
Z <- pension$p401[TRUNC_IDX] %>% as.numeric()
Y <- Y[TRUNC_IDX]
Y <- scale(Y)
centerY <- attr(Y, "scaled:center")
scaleY <- attr(Y, "scaled:scale")
attributes(Y) <- NULL
load(file = paste0("results/pension/241026/test_lsbp_", IG_DOF, "_", IG_SCALE*1000, "_", 100*Y_TRUNC, ".Rdata"))
ret <- 5001:25000
Mu <- ret %>% sapply(function(it) test_pi[[it]]$mu)
Tau <- ret %>% sapply(function(it) test_pi[[it]]$tau)

## ============================================================
## Table 4, cLSBP
## ============================================================
# 3.0. ATE overall check
Tau[, 10001:20000] %>% colMeans %>% quantile(probs = c(0.025, 0.5, 0.975)) * scaleY/1000

# age level

grouped_data_age <- pension[TRUNC_95_IDX, ] %>%
  group_by(across(c(a1:a4)))

grouped_data_age %>% group_keys

subgroups_age <- c("25-29", "30-34", "35-44", "45-54", "55-64") %>% rev

subgroups_age_df <- rbind %>% do.call(grouped_data_age %>% group_rows %>% lapply(function(grouped_indices){
  Treated_indices <- grouped_indices[which(Z[grouped_indices]==1)]
  data.frame(
    CATE = Tau[grouped_indices, ] %>% colMeans,
    Subgroup_mu_est = Mu[grouped_indices, ] %>% colMeans,
    CATT = Tau[Treated_indices, ] %>% colMeans,
    iter = 1:20000,
    size = grouped_indices %>% length
  )
}))

subgroups_age_df$subgroup = rep(subgroups_age, each = 20000)

# CATE table(with interval)
subgroups_age_df$size %>% table

subgroups_age_df %>% filter(iter > 10000) %>%
  group_by(subgroup) %>% summarise(
    size = mean(size),
    percentage = mean(size)/3497*100,
    median = quantile(CATE, 0.500) * scaleY/1000,
    interval_low = quantile(CATE, 0.025) * scaleY/1000,
    interval_high = quantile(CATE, 0.975) * scaleY/1000,
    ) %>%
  mutate(
    latex_row = glue("& {subgroup} & {round(size)} & ({round(percentage)}\\%) & {round(median, 1)} & ({round(interval_low, 1)}, {round(interval_high, 1)}) \\\\")
  )

# educational level

subgroup_key = "edu"
grouped_data_edu <- pension[TRUNC_95_IDX, ] %>%
  group_by(across(c(nohs, hs, smcol, col)))

grouped_data_edu %>% group_keys()

subgroups_edu <- c("nohs", "hs", "smcol", "col")

subgroups_edu_df <- rbind %>% do.call(grouped_data_edu %>% group_rows %>% lapply(function(grouped_indices){
  Treated_indices <- grouped_indices[which(Z[grouped_indices]==1)]
  data.frame(
    CATE = Tau[grouped_indices, ] %>% colMeans,
    Subgroup_mu_est = Mu[grouped_indices, ] %>% colMeans,
    CATT = Tau[Treated_indices, ] %>% colMeans,
    iter = 1:20000,
    size = grouped_indices %>% length
  )
}))

subgroups_edu_df$subgroup = rep(subgroups_edu %>% rev, each = 20000)

# CATE table(with interval)

subgroups_edu_df %>% filter(iter > 10000) %>%
  group_by(subgroup) %>% summarise(
    size = mean(size),
    percentage = mean(size)/3497*100,
    median = quantile(CATE, 0.500) * scaleY/1000,
    interval_low = quantile(CATE, 0.025) * scaleY/1000,
    interval_high = quantile(CATE, 0.975) * scaleY/1000,
  ) %>%
  mutate(
    latex_row = glue("& {subgroup} & {round(size)} & ({round(percentage)}\\%) & {round(median, 1)} & ({round(interval_low, 1)}, {round(interval_high, 1)}) \\\\")
  )

# income level

subgroup_key = "income"
grouped_data_income <- pension[TRUNC_95_IDX, ] %>%
  group_by(across(c(i1:i7)))

grouped_data_income %>% group_keys()

subgroups_income <- c("0-10k", "10-20k", "20-30k", "30-40k", "40-50k", "50-75k", "75k+")

subgroups_income_df <- rbind %>% do.call(grouped_data_income %>% group_rows %>% lapply(function(grouped_indices){
  Treated_indices <- grouped_indices[which(Z[grouped_indices]==1)]
  data.frame(
    CATE = Tau[grouped_indices, ] %>% colMeans,
    Subgroup_mu_est = Mu[grouped_indices, ] %>% colMeans,
    CATT = Tau[Treated_indices, ] %>% colMeans,
    iter = 1:20000,
    size = grouped_indices %>% length
  )
}))

subgroups_income_df$subgroup = rep(subgroups_income %>% rev, each = 20000)

# CATE table(with interval)

subgroups_income_df %>% filter(iter > 10000) %>%
  group_by(subgroup) %>% summarise(
    size = mean(size),
    percentage = mean(size)/3497*100,
    median = quantile(CATE, 0.500) * scaleY/1000,
    interval_low = quantile(CATE, 0.025) * scaleY/1000,
    interval_high = quantile(CATE, 0.975) * scaleY/1000,
  ) %>%
  mutate(
    latex_row = glue("& {subgroup} & {round(size)} & ({round(percentage)}\\%) & {round(median, 1)} & ({round(interval_low, 1)}, {round(interval_high, 1)}) \\\\")
  )

## ============================================================
## Table 4, BCF (pension_bcf_evaluation.R)
## ============================================================
load(file = paste0("results/pension/241112/test_bcf_Y_", 100*0.95, ".Rdata")) # test_bcf

# draws 1-25000 / 25001-50000 / ... are the four chains; use draws 10001-20000 of each chain

target_ret <- (rep(10001:20000, 4) + (25000 * rep(c(0, 1, 2, 3), each = 10000)))

# ATE (overall)

test_bcf$tau[target_ret, ] %>% rowMeans %>% quantile(probs = c(0.025, 0.5, 0.975)) * scaleY/1000

# age level

# age labels in increasing order (the BCF summaries below use rev(subgroups_age))
subgroups_age <- c("25-29", "30-34", "35-44", "45-54", "55-64")

subgroups_age_df_bcf <- rbind %>% do.call(grouped_data_age %>% group_rows %>% lapply(function(grouped_indices){
  data.frame(
    CATE = test_bcf$tau[target_ret, grouped_indices] %>% rowMeans,
    iter = target_ret,
    size = grouped_indices %>% length
  )
}))

subgroups_age_df_bcf$subgroup = rep(subgroups_age %>% rev, each = 40000)

# CATE table(with interval)

subgroups_age_df_bcf %>%
  group_by(subgroup) %>% summarise(
    size = mean(size),
    percentage = mean(size)/3497*100,
    median = quantile(CATE, 0.500) * scaleY/1000,
    interval_low = quantile(CATE, 0.025) * scaleY/1000,
    interval_high = quantile(CATE, 0.975) * scaleY/1000,
  ) %>%
  mutate(
    latex_row = glue("& {subgroup} & {round(size)} & ({round(percentage)}\\%) & {round(median, 1)} & ({round(interval_low, 1)}, {round(interval_high, 1)}) \\\\")
  )

# educational level

subgroups_edu_df_bcf <- rbind %>% do.call(grouped_data_edu %>% group_rows %>% lapply(function(grouped_indices){
  data.frame(
    CATE = test_bcf$tau[target_ret, grouped_indices] %>% rowMeans,
    iter = target_ret,
    size = grouped_indices %>% length
  )
}))

subgroups_edu_df_bcf$subgroup = rep(subgroups_edu %>% rev, each = 40000)

# CATE table(with interval)

subgroups_edu_df_bcf %>%
  group_by(subgroup) %>% summarise(
    size = mean(size),
    percentage = mean(size)/3497*100,
    median = quantile(CATE, 0.500) * scaleY/1000,
    interval_low = quantile(CATE, 0.025) * scaleY/1000,
    interval_high = quantile(CATE, 0.975) * scaleY/1000,
  ) %>%
  mutate(
    latex_row = glue("& {subgroup} & {round(size)} & ({round(percentage)}\\%) & {round(median, 1)} & ({round(interval_low, 1)}, {round(interval_high, 1)}) \\\\")
  )

# income level

subgroup_key = "income"
grouped_data_income <- pension[TRUNC_95_IDX, ] %>%
  group_by(across(c(i1:i7)))

grouped_data_income %>% group_keys()

subgroups_income <- c("0-10k", "10-20k", "20-30k", "30-40k", "40-50k", "50-75k", "75k+")

subgroups_income_df_bcf <- rbind %>% do.call(grouped_data_income %>% group_rows %>% lapply(function(grouped_indices){
  data.frame(
    CATE = test_bcf$tau[target_ret, grouped_indices] %>% rowMeans,
    iter = target_ret,
    size = grouped_indices %>% length
  )
}))

subgroups_income_df_bcf$subgroup = rep(subgroups_income %>% rev, each = 40000)

# CATE table(with interval)

subgroups_income_df_bcf %>%
  group_by(subgroup) %>% summarise(
    size = mean(size),
    percentage = mean(size)/3497*100,
    median = quantile(CATE, 0.500) * scaleY/1000,
    interval_low = quantile(CATE, 0.025) * scaleY/1000,
    interval_high = quantile(CATE, 0.975) * scaleY/1000,
  ) %>%
  mutate(
    latex_row = glue("& {subgroup} & {round(size)} & ({round(percentage)}\\%) & {round(median, 1)} & ({round(interval_low, 1)}, {round(interval_high, 1)}) \\\\")
  )

## ============================================================
## Figure 6: posterior predictive density (a) and quantile treatment effects (b)
## for the covariate profile shared by 9 participating and 7 non-participating households
## ============================================================
dir.create("figures", showWarnings = FALSE)

length_9_row = c()
for(group_indices in grouped_data_95_original %>% group_rows){
  if(group_indices %>% length == 9){
    length_9_row = group_indices
  }
}
length_9_row

X <- pension %>% dplyr::select(a1, a2, a3, a4, i1, i2, i3, i4, i5, i6, fsize, nohs, hs, smcol, marr, twoearn, db, pira, hown) %>%
  mutate(intercept = 1) %>% as.matrix
X <- X[TRUNC_IDX, ]
X[, 11] <- scale(X[, 11]) # scale fsize. the only numerical value.

load(file = paste0("results/pension/241026/pihat_BART_", 20, "_", 0.025*1000, "_", 100*0.95, ".Rdata"))
Xpi <- cbind(X, pihat)

# the 7 households with the same covariates and Z = 0
filter_condition <- Xpi[length_9_row[1], ]
# rows whose covariates all equal the profile
same_X <- Xpi %>% as.data.frame %>% mutate(r_n = row_number()) %>% filter(colSums(t(Xpi) == filter_condition) == length(filter_condition))
length_9_row_Z0 <- same_X$r_n[which(! same_X$r_n %in% length_9_row)]

logSigmoid_r <- function(product){
  if (product < 0){product + logSigmoid_r(-product)}
  else if (product > 18){exp(-2.0 * product) / 2.0 - exp(-product)}
  else{-log(1.0 + exp(-product))}
}

sample_from_logsigmoid <- function(probs){
  probs = probs - max(probs)
  probs = exp(probs)
  probs = probs/sum(probs)
  sample(1:51, 1, prob = probs)
}

get_prob_from_logsigmoid <- function(probs){
  probs = probs - max(probs)
  probs = exp(probs)
  probs = probs/sum(probs)
  probs
}

subject_idx <- length_9_row[1]
density_grid_ret <- 15001:25000 %>% sapply(function(iter){
  # calculate the weight by X^\top b_h.
  products_ex <- 1:50 %>% sapply(function(h){
    (Xpi[subject_idx, ] * test_pi[[iter]]$xout[[h]]$param) %>% sum
  })
  cumsum_log = 0
  probs_log = c()

  for(product in products_ex){
    probs_log = c(probs_log, cumsum_log + logSigmoid_r(product))
    cumsum_log = cumsum_log + logSigmoid_r(product) - product
  }
  probs_log = c(probs_log, cumsum_log)

  # calculate the mean and variance of the normal, by X^\top beta_h
  means_iter <- 1:51 %>% sapply(function(h){
    (c(Xpi[subject_idx, ], Z[subject_idx]*Xpi[subject_idx, ]) * test_pi[[iter]]$theta[[h]]$param) %>% sum
  })
  sigma2s_iter <- 1:51 %>% sapply(function(h){test_pi[[iter]]$theta[[h]]$sigma2})
  density_grid <- seq(-5, 5, by = 0.01) %>% sapply(function(x){
    dnorm(x = x, mean = means_iter, sd = sqrt(sigma2s_iter))
  }) %>% sweep(1, get_prob_from_logsigmoid(probs_log), FUN = '*') %>% colSums

  if(iter %% 1000 == 0){print(iter)}
  if(density_grid %>% sum < 99.99){
    print(paste0("The density integration for [-5,5] occupies ", format(density_grid %>% sum, digit = 5), "%, in iter : ", iter))
  }

  density_grid
})

subject_idx <- length_9_row_Z0[1]
density_grid_ret_Z0 <- 15001:25000 %>% sapply(function(iter){
  # calculate the weight by X^\top b_h.
  products_ex <- 1:50 %>% sapply(function(h){
    (Xpi[subject_idx, ] * test_pi[[iter]]$xout[[h]]$param) %>% sum
  })
  cumsum_log = 0
  probs_log = c()

  for(product in products_ex){
    probs_log = c(probs_log, cumsum_log + logSigmoid_r(product))
    cumsum_log = cumsum_log + logSigmoid_r(product) - product
  }
  probs_log = c(probs_log, cumsum_log)

  # calculate the mean and variance of the normal, by X^\top beta_h
  means_iter <- 1:51 %>% sapply(function(h){
    (c(Xpi[subject_idx, ], Z[subject_idx]*Xpi[subject_idx, ]) * test_pi[[iter]]$theta[[h]]$param) %>% sum
  })
  sigma2s_iter <- 1:51 %>% sapply(function(h){test_pi[[iter]]$theta[[h]]$sigma2})
  density_grid <- seq(-5, 5, by = 0.01) %>% sapply(function(x){
    dnorm(x = x, mean = means_iter, sd = sqrt(sigma2s_iter))
  }) %>% sweep(1, get_prob_from_logsigmoid(probs_log), FUN = '*') %>% colSums

  if(iter %% 1000 == 0){print(iter)}
  if(density_grid %>% sum < 99.99){
    print(paste0("The density integration for [-5,5] occupies ", format(density_grid %>% sum, digit = 5), "%, in iter : ", iter))
  }

  density_grid
})

## Calculate the 90% credible interval of posterior predictive distribution

# Treatment group(Z=1)
# (density_grid_ret %>% rowMeans %>% cumsum) > 5 # [421] => -0.80
low_int <- -0.8*scaleY+centerY
# (density_grid_ret %>% rowMeans %>% cumsum) < 95 # [691] => 1.90
up_int <- 1.9*scaleY+centerY

# Control group(Z=0)
# (density_grid_ret_Z0 %>% rowMeans %>% cumsum) > 5 # [385] => -1.15
low_int_Z0 <- -1.15*scaleY+centerY
# (density_grid_ret_Z0 %>% rowMeans %>% cumsum) < 95 # [594] => 0.94
up_int_Z0 <- 0.94*scaleY+centerY

## (Figure_6a)
# Call density_grid_ret, density_grid_ret_Z0 first!!

{
  p_figure6a <- data.frame(
    x_grid = (rep(seq(-5, 5, by = 0.01), 2) * scaleY + centerY) / 1000,
    y_density = c(density_grid_ret %>% rowMeans, density_grid_ret_Z0 %>% rowMeans),
    treatment = rep(c("participate", "non participate"), each = 1001)
  ) %>%
    ggplot() +
    geom_line(aes(x = x_grid, y = y_density, col = treatment), size = 2.5) +
    geom_rug(
      data = data.frame(
        x_grid = (c(Y[length_9_row], Y[length_9_row_Z0]) * scaleY + centerY) / 1000,
        treatment = c(rep("participate", length(length_9_row)),
                      rep("non participate", length(length_9_row_Z0)))
      ),
      aes(x = x_grid, col = treatment),
      size = 2
    ) +
    geom_vline(xintercept = c(low_int_Z0, up_int_Z0) / 1000,
               col = "#F8766D", linetype = "dashed", linewidth = 1.5) +
    geom_vline(xintercept = c(low_int, up_int) / 1000,
               col = "#00BFC4", linetype = "dashed", linewidth = 1.5) +
    scale_x_continuous(limits = c(-50, 150)) +
    labs(
      x = "Net financial assets(1000 USD)",
      y = "Estimated density"
    ) +
    theme_bw() +
    theme(
      title = element_text(size = 30),
      axis.title.x = element_text(size = 40),
      axis.title.y = element_text(size = 30),
      axis.text.x = element_text(size = 30),
      axis.text.y = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = c(0.8, 0.5),
      legend.text = element_text(size = 40)
    )

  output_base_figure6a <- "figures/Figure_6a"

  cairo_ps(paste0(output_base_figure6a, ".eps"), width = 15, height = 10, family = "Times")
  print(p_figure6a)
  dev.off()

  cairo_pdf(paste0(output_base_figure6a, ".pdf"), width = 15, height = 10, family = "Times")
  print(p_figure6a)
  dev.off()
}

grid_seq <- seq(-5, 5, by = 0.01)

quantile_posterior <- apply(density_grid_ret, 2, function(col) {
  cumsum_col <- cumsum(col)
  indices <- c(
    which(cumsum_col > 10) %>% min,
    which(cumsum_col > 30) %>% min,
    which(cumsum_col > 50) %>% min,
    which(cumsum_col > 70) %>% min,
    which(cumsum_col < 90) %>% max
  )
  grid_seq[indices]
}) %>% t %>% as.data.frame

colnames(quantile_posterior) <- c(1, 3, 5, 7, 9)/10

quantile_posterior %>% apply(2, median)

quantile_posterior_Z0 <- apply(density_grid_ret_Z0, 2, function(col) {
  cumsum_col <- cumsum(col)
  indices <- c(
    which(cumsum_col > 10) %>% min,
    which(cumsum_col > 30) %>% min,
    which(cumsum_col > 50) %>% min,
    which(cumsum_col > 70) %>% min,
    which(cumsum_col < 90) %>% max
  )
  grid_seq[indices]
}) %>% t %>% as.data.frame

colnames(quantile_posterior_Z0) <- c(1, 3, 5, 7, 9)/10

quantile_posterior_Z0 %>% apply(2, median)

## (Figure_6b)
{
  p_figure6b <- (quantile_posterior - quantile_posterior_Z0) %>%
    gather(key = "key", value = "quantile") %>%
    mutate(alpha = as.factor(key)) %>%
    ggplot() +
    geom_violin(aes(x = key, y = quantile * scaleY / 1000, col = alpha, fill = alpha), adjust = 1.5) +
    labs(
      x = TeX("$alpha$"),
      y = TeX("QTE of $alpha$th quantile (1000 USD)")
    ) +
    theme_bw() +
    theme(
      axis.title.x = element_text(size = 40),
      axis.title.y = element_text(size = 40),
      axis.text.x = element_text(size = 30),
      axis.text.y = element_text(size = 30),
      legend.position = "none"
    )

  output_base_figure6b <- "figures/Figure_6b"

  cairo_ps(paste0(output_base_figure6b, ".eps"), width = 15, height = 10, family = "Times")
  print(p_figure6b)
  dev.off()

  cairo_pdf(paste0(output_base_figure6b, ".pdf"), width = 15, height = 10, family = "Times")
  print(p_figure6b)
  dev.off()
}
