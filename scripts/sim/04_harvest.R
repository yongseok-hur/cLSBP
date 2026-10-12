#!/usr/bin/env Rscript
# ============================================================
# Summaries over the R = 1,000 replications of Simulations 1 and 2 (cLSBP, BCF, CF), with Monte Carlo SEs.
#
# Usage (from the repository root):
#   Rscript scripts/sim/04_harvest.R Kang [max_seed]
#   Rscript scripts/sim/04_harvest.R Hahn [max_seed]
#   Rscript scripts/sim/04_harvest.R both [max_seed]      (all available seeds if max_seed is omitted)
#
# Output: results/kang_summary.csv, results/hahn_summary.csv
#
# Reads the per-replication summary files only (not the fits); seed, scenario, propensity score method, H and
# model are parsed from the file names. rmse = sqrt(mean(RMSE^2)), cover = mean(coverage), len = mean(length).
# ============================================================
suppressMessages({
  library(dplyr); library(stringr); library(tidyr); library(purrr)
})

args <- commandArgs(trailingOnly = TRUE)
SIM_ARG  <- ifelse(length(args) >= 1, args[1], "Kang")
MAX_SEED <- ifelse(length(args) >= 2, as.numeric(args[2]), Inf)
DATE <- "R1000_260617"

# ---- parse metadata from file name ----
parse_meta <- function(fname, sim, model){
  bn <- basename(fname)
  seed <- as.numeric(str_match(bn, "seed(\\d+)")[,2])
  # ps_method (3 options)
  ps <- dplyr::case_when(
    str_detect(bn, "plugin_BART") ~ "BART",
    str_detect(bn, "plugin_GLM")  ~ "GLM",
    str_detect(bn, "CF_BART")     ~ "BART",
    str_detect(bn, "CF_GLM")      ~ "GLM",
    str_detect(bn, "CF_default")  ~ "default",
    str_detect(bn, "noPS")        ~ "noPS",
    TRUE ~ NA_character_
  )
  # H (cLSBP only; NA for BCF/CF)
  H <- str_match(bn, "_H(\\d+)")[,2] |> as.numeric()
  # scenario
  if(sim == "Kang"){
    scen <- paste0("ts", str_match(bn, "tarsel(-?\\d+)")[,2])   # ts00, ts-0132, ... (prefix prevents numeric conversion in CSV)
  } else {
    lin <- str_match(bn, "_(lin|non)_")[,2]
    te  <- str_match(bn, "_(het|hom)_")[,2]
    n   <- str_match(bn, "_(250|500)_")[,2]
    scen <- paste(lin, te, n, sep="_")
  }
  tibble(seed=seed, scenario=scen, ps=ps, H=H, model=model)
}

# ---- read one folder (model) into a data frame ----
read_model <- function(sim, model, max_seed){
  simdir <- ifelse(sim=="Kang", "Kang_v4", "Hahn_v2")
  folder <- dplyr::case_when(
    model=="cLSBP" ~ "LSBP_1_mixture",
    model=="BCF"   ~ "BCF",
    model=="CF"    ~ "CF"
  )
  dir <- file.path("results", simdir, DATE, folder)
  if(!dir.exists(dir)){ message("folder not found: ", dir); return(NULL) }
  # summary .rds files only (prefix test_results_df_; .Rdata excluded)
  files <- list.files(dir, pattern="^test_results_df_.*\\.rds$", full.names=TRUE)
  if(length(files)==0){ message("no rds files: ", dir); return(NULL) }
  # max_seed filter (seed taken from the file name)
  if(is.finite(max_seed)){
    keep <- as.numeric(str_match(basename(files), "seed(\\d+)")[,2]) <= max_seed
    files <- files[keep]
  }
  message(sprintf("  %s/%s: %d files", sim, model, length(files)))
  # read + attach metadata
  purrr::map_dfr(files, function(f){
    d <- tryCatch(readRDS(f), error=function(e) NULL)
    if(is.null(d) || nrow(d)==0) return(NULL)
    meta <- parse_meta(f, sim, model)
    dplyr::bind_cols(d, meta[rep(1,nrow(d)), ])
  })
}

# ---- summarise one simulation ----
harvest_sim <- function(sim, max_seed){
  message("=== ", sim, " collecting (max_seed=", max_seed, ") ===")
  df <- dplyr::bind_rows(
    read_model(sim, "cLSBP", max_seed),
    read_model(sim, "BCF",   max_seed),
    read_model(sim, "CF",    max_seed)
  )
  if(is.null(df) || nrow(df)==0){ message("0 rows collected"); return(NULL) }

  # summaries by group = model/ps/H/scenario/target/confi/what_name
  # (H has values for cLSBP only; NA for BCF/CF is kept as a group key)
  # MCSE formulas, Morris, White & Crowther (2019):
  #   coverage: sqrt(cov*(1-cov)/R)         (SE of a binomial proportion)
  #   length:   sd(length)/sqrt(R)          (SE of a mean)
  #   RMSE:     RMSE/sqrt(2*R)              (standard normal-approximation formula)
  # Outlying replications are not removed (all are averaged); a diagnostic counter is reported separately.
  summ <- df %>%
    group_by(model, ps, H, scenario, target, confi, what_name) %>%
    summarise(
      R        = n(),
      rmse     = sqrt(mean(RMSE^2)),
      cover    = mean(coverage),
      len      = mean(length),
      mcse_rmse  = sqrt(mean(RMSE^2))/sqrt(2*n()),     # Morris: RMSE/sqrt(2R)
      mcse_cover = sqrt(cover*(1-cover)/n()),          # Morris: binomial SE
      mcse_len   = sd(length)/sqrt(n()),               # SE of the mean
      # diagnostic: number of replications whose interval length exceeds 10 x the group median
      n_unstable = sum(length > 10*median(length)),
      max_len    = max(length),
      .groups="drop"
    )
  summ
}

# ---- run ----
sims <- if(SIM_ARG=="both") c("Kang","Hahn") else SIM_ARG
for(sim in sims){
  summ <- harvest_sim(sim, MAX_SEED)
  if(is.null(summ)) next

  # save
  out_csv <- paste0("results/", tolower(sim), "_summary.csv")
  write.csv(summ, out_csv, row.names=FALSE)
  message("saved: ", out_csv, " (", nrow(summ), " rows)")

  # console preview in the paper's table format (tau, confi 95) -- same conditions as before
  cat("\n========== ", sim, " -- table preview (tau, confi=95) ==========\n")
  tbl <- summ %>%
    filter(what_name=="tau", confi==95) %>%
    mutate(across(c(rmse,cover,len,mcse_rmse,mcse_cover,mcse_len), ~round(.,3))) %>%
    arrange(scenario, model, ps, H, target)
  print(as.data.frame(tbl), row.names=FALSE)

  # instability diagnostic -- number of exploding replications by method x scenario
  cat("\n--- instability diagnostic (replications with length > 10*median) ---\n")
  unstab <- summ %>% filter(what_name=="tau", confi==95, target=="ATE", n_unstable>0) %>%
    select(model, ps, H, scenario, R, n_unstable, max_len) %>%
    arrange(desc(n_unstable))
  if(nrow(unstab)>0) print(as.data.frame(unstab), row.names=FALSE) else cat("  no unstable replications\n")

  # number of seeds collected
  cat("\n--- seed coverage (max R by model x ps) ---\n")
  summ %>% group_by(model, ps) %>% summarise(maxR=max(R), .groups="drop") %>%
    as.data.frame() %>% print(row.names=FALSE)
}
message("\ndone.")
