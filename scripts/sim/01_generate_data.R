## ============================================================
## Simulated datasets for Simulations 1 (5 scenarios) and 2 (8 scenarios): one csv file per scenario and seed.
## Usage: Rscript scripts/sim/01_generate_data.R <seed_start> <seed_end>
## ============================================================
source("R/dgp.R")   # dgp_Kang_v4, dgp_Hahn_v2, ROOM_NUMBER=533
suppressMessages({library(stringr); library(dplyr)})

args <- commandArgs(trailingOnly = TRUE)
seed_start <- if(length(args) >= 1) as.integer(args[1]) else 1
seed_end   <- if(length(args) >= 2) as.integer(args[2]) else 1000

dir.create("data/Kang_v4", recursive = TRUE, showWarnings = FALSE)
dir.create("data/Hahn_v2", recursive = TRUE, showWarnings = FALSE)

## --- Kang: 5 values of targeted_selection in problem_df ---
load("config/kang_problem_df.Rdata")
kang_df <- problem_df
cat("Kang scenarios:", nrow(kang_df), "| tar_sel:",
    paste(round(kang_df$targeted_selection,4), collapse=", "), "\n")

## --- Hahn: problem_df (linearity/TE_type/n_size Factor) ---
load("config/hahn_problem_df.Rdata")
hahn_df <- problem_df
cat("Hahn scenarios:", nrow(hahn_df), "\n")

t0 <- Sys.time()
for(seed in seed_start:seed_end){
  ## Kang 5
  for(i in seq_len(nrow(kang_df))){
    pinfo <- kang_df[i, ]
    dgp_Kang_v4(seed = seed,
                selection_option = pinfo$selection_option,
                tar_sel = pinfo$targeted_selection,
                n = 500)
  }
  ## Hahn 8 (same conversion as in the simulation script)
  for(i in seq_len(nrow(hahn_df))){
    pinfo <- hahn_df[i, ]
    dgp_Hahn_v2(seed = seed,
                linear = (pinfo$linearity == "lin"),
                hetero = (pinfo$TE_type == "het"),
                n = pinfo$n_size)   # c_0 = DGP default (1)
  }
  if(seed %% 50 == 0) cat("seed", seed, "done", format(Sys.time(), "%H:%M:%S"), "\n")
}
cat("=== done seed", seed_start, "~", seed_end, "|",
    round(difftime(Sys.time(), t0, units="mins"),1), "min ===\n")
