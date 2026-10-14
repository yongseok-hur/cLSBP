## ============================================================
## 2022 ACIC data challenge (Section 4.1): scoring against the official truths and the participants' results,
## and Figures 2-5. Sections were run interactively; later sections repeat earlier ones with updated inputs.
## Requires the per-dataset results of acic_run.R (not included). Figures 2-5 of the paper were drawn with
## acic_figures_revised.R.
## ============================================================

source("R/requirements.R")

## 240630, 240707, 240716 ##
## Check the difference of the results by "PS estimation"
# All 17 Scenarios / 4 methods for plug-in and no plug-in / prior id : 1

{
date <- "240626"
dir_result <- paste0("results/ACIC_2022/", date, "/")
load(file = paste0(dir_result, "problem_df.Rdata"))

ps_estimator <- list("CBPS" = "240610", "BART" = "240610", "LASSO" = "240626", "GBM" = "240626")

scoreboard <- rbind %>% do.call(
  names(ps_estimator)[1:2] %>% lapply(function(ps_method){
    rbind %>% do.call(
      problem_df$dataset.num[1:800] %>% lapply(function(d_n){
        dir_results <- paste0("results/ACIC_2022/", ps_estimator[[ps_method]],"/")
        readRDS(file = paste0(dir_results, "Strong_Large/LSBP_1_mixture/test_", str_sub(paste0("000", d_n), start = -4), "_LSBP_1_mixture_prior_", 1, "_", ps_method,"_answers.rds"))
      })
    )
  })
)

scoreboard$Scenario <- factor(scoreboard$Scenario, levels = scoreboard$Scenario %>% unique, labels = c("S/A/L/L", "S/A/L/S", "S/B/L/L", "S/B/L/S"))
scoreboard %>% dim # (4 Scenario) x (200 dataset) x (4 PS estimator) x (15 results) = 48000
scoreboard %>% head

scoreboard %>% group_by(Scenario, variable, level, year, plugin) %>%
  filter(variable != "Overall", variable != "Yearly") %>% summarise(
    n = n(),
    RMSE = sqrt(mean(bias^2)),
    coverage = mean(covered)
  ) %>%
  ggplot(aes(x = as.factor(plugin), y = RMSE, color = as.factor(plugin))) +
  geom_point(aes(shape = as.factor(plugin)), size = 3, alpha = 0.7)+ # , position = position_jitter(width = 0.05)
  facet_wrap(~as.factor(Scenario), nrow = 1) +
  ylim(c(10, 60))+
  labs(title = "RMSE for 12 SATTs by subgroups : Strong/Large 4 Scenarios, 200 realizations.",
       subtitle = "LSBP with 4 plug-in methods"
  )+
  theme_bw()+
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.ticks.x = element_blank()   # Remove x-axis ticks
  )

scoreboard %>% group_by(Scenario, variable, level, year, plugin) %>%
  filter(variable != "Overall", variable != "Yearly") %>% summarise(
    n = n(),
    RMSE = sqrt(mean(bias^2)),
    coverage = mean(covered)
  ) %>%
  ggplot(aes(x = as.factor(plugin), y = coverage, color = as.factor(plugin))) +
  geom_point(aes(shape = as.factor(plugin)), size = 3, alpha = 0.7)+ #, position = position_jitter(width = 0.05)
  facet_wrap(~Scenario) +
  ylim(c(0, 1))+
  labs(title = "Coverage for 12 SATTs by subgroups : Strong/Large 4 Scenarios, 200 realizations.",
       subtitle = "LSBP(plug-in : BART) with 6 prior cases")+
  theme_bw()+
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.ticks.x = element_blank()   # Remove x-axis ticks
  )
}
## 240702 ##

ps_estimator <- list("CBPS" = "240610", "BART" = "240610", "LASSO" = "240626", "GBM" = "240626")

scoreboard_Strong_Large <- rbind %>% do.call(
  names(ps_estimator) %>% lapply(function(ps_method){
    rbind %>% do.call(
      problem_df$dataset.num[1:800] %>% lapply(function(d_n){
        dir_results <- paste0("results/ACIC_2022/", ps_estimator[[ps_method]],"/")
        readRDS(file = paste0(dir_results, "Strong_Large/LSBP_1_mixture/test_", str_sub(paste0("000", d_n), start = -4), "_LSBP_1_mixture_prior_", 1, "_", ps_method,"_answers.rds"))
      })
    )
  })
)
333580 + 219360 + 10080 + 33850 + 50350
scoreboard_Strong_Large_2 <- scoreboard_Strong_Large %>% rbind(
  rbind %>% do.call(
    problem_df$dataset.num[1:800] %>% lapply(function(d_n){
      dir_results <- paste0("results/ACIC_2022/240626/")
      readRDS(file = paste0(dir_results, "Strong_Large/LSBP_1_mixture/test_", str_sub(paste0("000", d_n), start = -4), "_LSBP_1_mixture_prior_", 1, "_answers.rds"))
    })
  )
)

scoreboard_Strong_Large_2 %>% dim

scoreboard_Strong_Large_2$Scenario <- factor(scoreboard_Strong_Large_2$Scenario, levels = scoreboard_Strong_Large_2$Scenario %>% unique, labels = c("S/A/L/L", "S/A/L/S", "S/B/L/L", "S/B/L/S"))
scoreboard_Strong_Large_2 %>% dim # (4 Scenario) x (200 dataset) x (4 PS estimator) x (15 results) = 48000
scoreboard_Strong_Large_2$plugin <- ifelse(is.na(scoreboard_Strong_Large_2$plugin), "NA", scoreboard_Strong_Large_2$plugin)
scoreboard_Strong_Large_2 %>% tail

scoreboard_Strong_Large_2 %>% group_by(Scenario, variable, level, year, plugin) %>%
  filter(variable != "Overall", variable != "Yearly") %>% summarise(
    n = n(),
    RMSE = sqrt(mean(bias^2)),
    coverage = mean(covered)
  ) %>%
  ggplot(aes(x = as.factor(plugin), y = RMSE, color = as.factor(plugin))) +
  geom_point(aes(shape = as.factor(plugin)), size = 3, alpha = 0.7)+ # , position = position_jitter(width = 0.05)
  facet_wrap(~as.factor(Scenario), nrow = 1) +
  ylim(c(10, 60))+
  labs(title = "RMSE for 12 SATTs by subgroups : Strong/Large 4 Scenarios, 200 realizations.",
       subtitle = "LSBP with 4 plug-in methods"
  )+
  theme_bw()+
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.ticks.x = element_blank()   # Remove x-axis ticks
  )

scoreboard_Strong_Large_2 %>% group_by(Scenario, variable, level, year, plugin, is_LSBP) %>%
  filter(variable != "Overall", variable != "Yearly") %>% summarise(
    n = n(),
    RMSE = sqrt(mean(bias^2)),
    coverage = mean(covered)
  ) %>%
  ggplot(aes(x = as.factor(plugin), y = coverage, color = as.factor(plugin))) +
  geom_point(aes(shape = as.factor(plugin)), size = 3, alpha = 0.7)+ #, position = position_jitter(width = 0.05)
  facet_wrap(~Scenario, nrow = 1) +
  ylim(c(0, 1))+
  labs(title = "Coverage for 12 SATTs by subgroups : Strong/Large 4 Scenarios, 200 realizations.",
       subtitle = "LSBP(plug-in : BART) with 6 prior cases")+
  theme_bw()+
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.ticks.x = element_blank()   # Remove x-axis ticks
  )

## Other 3 Scenarios

date <- "240707"
dir_result <- paste0("results/ACIC_2022/", date, "/")
load(file = paste0(dir_result, "problem_df.Rdata"))

three_other_scenarios <- c("Strong_Small", "Weak_Large", "Weak_Small")

scoreboard_Other_Scenarios <- rbind %>% do.call(
  three_other_scenarios %>% lapply(function(scenarios_cat){
    split_scenarios_cat <- unlist(strsplit(scenarios_cat, "_", fixed = TRUE))
    scenarios_df <- problem_df %>% filter(Confounding.Strength == split_scenarios_cat[1], Impact.Heterogeneity == split_scenarios_cat[2])
    rbind %>% do.call(
      names(ps_estimator) %>% lapply(function(ps_method){
        rbind %>% do.call(
          scenarios_df$dataset.num[1:800] %>% lapply(function(d_n){
            readRDS(file = paste0("results/ACIC_2022/240707/", scenarios_cat, "/LSBP_1_mixture/test_", str_sub(paste0("000", d_n), start = -4), "_LSBP_1_mixture_prior_", 1, "_", ps_method,"_answers.rds"))
          })
        )
      })
    )
  })
)

scoreboard_Other_Scenarios_2 <- scoreboard_Other_Scenarios %>% rbind(
  rbind %>% do.call(
    three_other_scenarios %>% lapply(function(scenarios_cat){
      split_scenarios_cat <- unlist(strsplit(scenarios_cat, "_", fixed = TRUE))
      scenarios_df <- problem_df %>% filter(Confounding.Strength == split_scenarios_cat[1], Impact.Heterogeneity == split_scenarios_cat[2])
      rbind %>% do.call(
        scenarios_df$dataset.num[1:800] %>% lapply(function(d_n){
          readRDS(file = paste0("results/ACIC_2022/240707/", scenarios_cat, "/LSBP_1_mixture/test_", str_sub(paste0("000", d_n), start = -4), "_LSBP_1_mixture_prior_", 1 ,"_answers.rds"))
        })
      )
    })
  )
)

scoreboard_Other_Scenarios_2 %>% dim

scoreboard_final <- rbind(scoreboard_Strong_Large_2, scoreboard_Other_Scenarios_2)

scoreboard_final %>% dim # 17 x 200 x 5(PS) x 15(SATTs)

scoreboard_final %>% colnames

scoreboard_final$Scenario <- factor(
  scoreboard_final$Scenario, levels = scoreboard_final$Scenario %>% unique,
  labels = c("S/A/L/L", "S/A/L/S", "S/B/L/L", "S/B/L/S",
             "S/B/S/L", "S/A/S/S", "S/A/S/L", "S/B/S/S",
             "W/B/L/L", "W/A/L/S", "W/A/L/L", "W/B/L/S",
             "W/A/S/S", "W/B/S/L", "W/B/S/S", "W/A/S/L"))

save(scoreboard_final, file = "results/ACIC_2022/scoreboard_final.Rdata")

scoreboard_final <- scoreboard_final %>%
  separate(Scenario, into = c("Confounding.Strength", "Confounding.Source",
                              "Impact.Heterogeneity", "Idiosyncrasy.of.Impacts"), sep = "/")

# Map each column to its respective category
scoreboard_final <- scoreboard_final %>%
  mutate(
    Confounding.Strength = case_when(
      Confounding.Strength == "S" ~ "Strong",
      Confounding.Strength == "W" ~ "Weak"
    ),
    Confounding.Source = case_when(
      Confounding.Source == "A" ~ "Source A",
      Confounding.Source == "B" ~ "Source B"
    ),
    Impact.Heterogeneity = case_when(
      Impact.Heterogeneity == "L" ~ "Large",
      Impact.Heterogeneity == "S" ~ "Small"
    ),
    Idiosyncrasy.of.Impacts = case_when(
      Idiosyncrasy.of.Impacts == "L" ~ "Large",
      Idiosyncrasy.of.Impacts == "S" ~ "Small"
    )
  )

scoreboard_final %>% colnames
scoreboard_final$plugin <- ifelse(is.na(scoreboard_final$plugin), "NA", scoreboard_final$plugin)
# Run once (the summary file was saved here):
# save(scoreboard_final, file = "results/ACIC_2022/scoreboard_final.Rdata")

load(file = "results/ACIC_2022/scoreboard_final.Rdata")

scoreboard_final %>%
  mutate(Combined = paste(Confounding.Source, Idiosyncrasy.of.Impacts, sep = " & ")) %>%
  filter(variable == "Overall") %>%
  group_by(Confounding.Strength,Impact.Heterogeneity, Combined, plugin) %>%
  summarise(
  n = n(),
  RMSE = sqrt(mean(bias^2)),
  coverage = mean(covered)
) %>% ggplot(aes(x = Combined, y = RMSE, color = as.factor(plugin))) +
  geom_point(aes(shape = as.factor(plugin)), size = 3, alpha = 0.5) +
  facet_wrap(~Confounding.Strength+Impact.Heterogeneity, nrow = 1)+
  ylim(c(10, 50))+
  labs(title = "RMSE for Overall ATT : Strong/Large 4 Scenarios, 200 realizations.",
       subtitle = "LSBP with (4 plug-in methods & no plug-in)")+
  theme_bw()

{
  ## Plot ##
  test_priors_list

  prior_now <- prior_pool[["LSBP_1_mixture"]] %>% filter(plugin == "BART")

  scoreboard_priors <- scoreboard %>%
    mutate(DOF_XOUT = prior_now$DOF_XOUT[prior_id], SCALE_XOUT = prior_now$SCALE_XOUT[prior_id])

  scoreboard %>%
    mutate(DOF_XOUT = prior_now$DOF_XOUT[prior_id], SCALE_XOUT = prior_now$SCALE_XOUT[prior_id]) %>%
    group_by(Scenario, variable, level, year, DOF_XOUT, SCALE_XOUT) %>%
    filter(variable == "Overall") %>% summarise(
      n = n(),
      RMSE = sqrt(mean(bias^2)),
      # bias_quantile = quantile(abs(bias), c(0.05, 0.95)),
      coverage = mean(covered)
    ) %>%
    ggplot(aes(x = Scenario, y = RMSE, color = as.factor(DOF_XOUT))) +
    geom_point(aes(shape = as.factor(SCALE_XOUT)), size = 3, alpha = 0.5) +
    facet_wrap(~SCALE_XOUT, nrow = 2)+
    # ylim(c(0, 45))+
    ylim(c(15, 35))+
    labs(title = "RMSE for Overall ATT : Strong/Large 4 Scenarios, 200 realizations.",
         subtitle = "LSBP(plug-in : BART) with 6 prior cases")+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),  # Remove x-axis title
      axis.text.x = element_blank(),   # Remove x-axis text
      axis.ticks.x = element_blank()   # Remove x-axis ticks
    )

  scoreboard %>% dim

  scoreboard %>%
    mutate(DOF_XOUT = prior_now$DOF_XOUT[prior_id], SCALE_XOUT = prior_now$SCALE_XOUT[prior_id]) %>%
    group_by(Scenario, variable, level, year, DOF_XOUT, SCALE_XOUT) %>%
    filter(variable == "Overall") %>% summarise(
      n = n(),
      RMSE = sqrt(mean(bias^2)),
      coverage = mean(covered)
    ) %>%
    ggplot(aes(x = Scenario, y = coverage, color = as.factor(DOF_XOUT))) +
    geom_point(aes(shape = as.factor(SCALE_XOUT)), size = 3, alpha = 0.5) +
    #ylim(c(0, 1))+
    facet_wrap(~SCALE_XOUT, nrow = 2)+
    ylim(c(0.4, 0.8))+
    labs(title = "Coverage for Overall ATT : Strong/Large 4 Scenarios, 200 realizations.",
         subtitle = "LSBP(plug-in : BART) with 6 prior cases")+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),  # Remove x-axis title
      axis.text.x = element_blank(),   # Remove x-axis text
      axis.ticks.x = element_blank()   # Remove x-axis ticks
    )

  scoreboard_priors %>% group_by(Scenario, variable, level, year, DOF_XOUT, SCALE_XOUT) %>%
    filter(variable != "Overall", variable != "Yearly") %>% summarise(
      n = n(),
      RMSE = sqrt(mean(bias^2)),
      coverage = mean(covered)
    ) %>%
    ggplot(aes(x = as.factor(DOF_XOUT), y = RMSE, color = as.factor(DOF_XOUT))) +
    geom_point(size = 3, alpha = 0.7, position = position_jitter(width = 0.05))+
    facet_wrap(~as.factor(SCALE_XOUT), nrow = 1) +
    ylim(c(10, 60))+
    labs(title = "RMSE for 12 SATTs by subgroups : Strong/Large 4 Scenarios, 200 realizations.",
         subtitle = "LSBP(plug-in : BART) with 6 prior cases"
    )+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),  # Remove x-axis title
      axis.text.x = element_blank(),   # Remove x-axis text
      axis.ticks.x = element_blank()   # Remove x-axis ticks
    )

  scoreboard_priors %>% group_by(Scenario, variable, level, year, DOF_XOUT, SCALE_XOUT) %>%
    filter(variable != "Overall", variable != "Yearly", Scenario != "None/Large") %>% summarise(
      n = n(),
      RMSE = sqrt(mean(bias^2)),
      coverage = mean(covered)
    ) %>%
    ggplot(aes(x = as.factor(DOF_XOUT), y = coverage, color = as.factor(DOF_XOUT))) +
    geom_point(aes(shape = as.factor(SCALE_XOUT)), size = 3, alpha = 0.7, position = position_jitter(width = 0.05))+
    facet_grid(as.factor(SCALE_XOUT)~Scenario) +
    ylim(c(0, 1))+
    labs(title = "Coverage for 12 SATTs by subgroups : Strong/Large 4 Scenarios, 200 realizations.",
         subtitle = "LSBP(plug-in : BART) with 6 prior cases")+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),  # Remove x-axis title
      axis.text.x = element_blank(),   # Remove x-axis text
      axis.ticks.x = element_blank()   # Remove x-axis ticks
    )

  ## Plot - compare with Lab37! ##
  scoreboard_hard10 %>% group_by(Scenario, variable, level, year) %>%
    filter(variable == "Overall") %>% summarise(
      n = n(),
      RMSE = sqrt(mean(bias^2)),
      coverage = mean(covered)
    ) %>%
    ggplot(aes(x = Scenario, y = RMSE, color = variable)) +
    geom_point(size = 3, alpha = 0.7) +
    ylim(c(0, 45))+
    labs(title = "RMSE for Overall ATT",
         subtitle = "1 hard Scenario, first 10 realizations")+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),  # Remove x-axis title
      axis.text.x = element_blank(),   # Remove x-axis text
      axis.ticks.x = element_blank()   # Remove x-axis ticks
    )

  scoreboard_hard10 %>% group_by(Scenario, variable, level, year) %>%
    filter(variable == "Overall") %>% summarise(
      n = n(),
      RMSE = sqrt(mean(bias^2)),
      coverage = mean(covered)
    ) %>%
    ggplot(aes(x = Scenario, y = coverage)) +
    geom_point(aes(color = variable), size = 3, alpha = 0.7) +
    ylim(c(0, 1))+
    labs(title = "Coverage for Overall ATT",
         subtitle = "1 hard Scenario, first 10 realizations")+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),  # Remove x-axis title
      axis.text.x = element_blank(),   # Remove x-axis text
      axis.ticks.x = element_blank()   # Remove x-axis ticks
    )

  scoreboard_hard10 %>% group_by(Scenario, variable, level, year) %>%
    filter(variable != "Overall", variable != "Yearly", Scenario != "None/Large") %>% summarise(
      n = n(),
      RMSE = sqrt(mean(bias^2)),
      coverage = mean(covered)
    ) %>%
    ggplot(aes(x = Scenario, y = RMSE, color = variable)) +
    geom_point(size = 3, alpha = 0.7, position = position_jitter(width = 0.05))+
    ylim(c(10, 60))+
    labs(title = "RMSE for 12 SATTs by subgroups",
         subtitle = "1 hard Scenario, first 10 realizations. LSBP(plug-in : BART)"
    )+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),  # Remove x-axis title
      axis.text.x = element_blank(),   # Remove x-axis text
      axis.ticks.x = element_blank()   # Remove x-axis ticks
    )

  scoreboard_hard10 %>% group_by(Scenario, variable, level, year) %>%
    filter(variable != "Overall", variable != "Yearly", Scenario != "None/Large") %>% summarise(
      n = n(),
      RMSE = sqrt(mean(bias^2)),
      coverage = mean(covered)
    ) %>%
    ggplot(aes(x = Scenario, y = coverage, color = variable)) +
    geom_point(size = 3, alpha = 0.7, position = position_jitter(width = 0.05))+
    ylim(c(0, 1))+
    labs(title = "Coverage for 12 SATTs by subgroups",
         subtitle = "1 hard Scenario, first 10 realizations. LSBP(plug-in : BART)")+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),  # Remove x-axis title
      axis.text.x = element_blank(),   # Remove x-axis text
      axis.ticks.x = element_blank()   # Remove x-axis ticks
    )
  }

# Paragraph 1
scoreboard_final_new$variable %>% table
scoreboard_final_new %>% colnames
scoreboard_final_new %>% filter(
  Confounding.Strength == "None",
  variable == "Overall"
) %>% group_by(plugin) %>% summarise(
  RMSE = sqrt(mean(bias^2)),
  coverage = mean(covered),
  length = mean(`95%`-`5%`)
)
overall_conf_BART <- scoreboard_final_new %>% filter(
  Confounding.Strength != "None",
  variable == "Overall",
  plugin == "BART"
) %>% summarise(
  RMSE = sqrt(mean(bias^2)),
  coverage = mean(covered),
  length = mean(`95%`-`5%`)
)

(ACIC_results %>% filter(
  Confounding.Strength == "None",
  variable == "Overall"
) %>% group_by(Submission) %>% summarise(
  RMSE = sqrt(mean(rmse^2)),
  coverage = mean(cover),
  length = mean(width)
))
# confounded, overall ATT RMSE
((ACIC_results %>% filter(
  Confounding.Strength != "None",
  variable == "Overall"
) %>% group_by(Submission) %>% summarise(
  RMSE = sqrt(mean(rmse^2)),
  coverage = mean(cover),
  length = mean(width)
))$RMSE > 20.25) %>% length
22/58*100

## 2409 ##
## DRAW PLOTS (Figure_2, )

# prior info : SIGMA DOF = 10, SCALE = 1/5 (-> IG(5, 1) / original : IG(5/2, 5/2))
# Create scoreboard_final_new & forplot
{
date <- "240904"
dir_result <- paste0("results/ACIC_2022/", date, "/")
load(file = paste0(dir_result, "problem_df.Rdata"))

scenarios <- c("None_Large", "Strong_Large", "Strong_Small", "Weak_Large", "Weak_Small")
ps_estimator <- list("13_", "13_CBPS_", "13_BART_", "13_LASSO_", "13_GBM_")

scoreboard_final_new <- rbind %>% do.call(
  scenarios %>% lapply(function(scenarios_cat){
    split_scenarios_cat <- unlist(strsplit(scenarios_cat, "_", fixed = TRUE))
    scenarios_df <- problem_df %>% filter(Confounding.Strength == split_scenarios_cat[1], Impact.Heterogeneity == split_scenarios_cat[2])
    rbind %>% do.call(
      ps_estimator %>% lapply(function(ps_method){
        rbind %>% do.call(
          list.files(path = paste0("results/ACIC_2022/240904/", scenarios_cat, "/LSBP_1_mixture"), pattern = paste0("test_.*", ps_method, "answers\\.rds"), full.names = T) %>% lapply(readRDS)
        )
      })
    )
  })
)
scoreboard_final_new$Scenario %>% table
scoreboard_final_new$Scenario %>% unique
scoreboard_final_new$Scenario <- factor(
  scoreboard_final_new$Scenario, levels = scoreboard_final_new$Scenario %>% unique,
  labels = c("N/N/L/S",
             "S/A/L/L", "S/A/L/S", "S/B/L/L", "S/B/L/S",
             "S/B/S/L", "S/A/S/S", "S/A/S/L", "S/B/S/S",
             "W/B/L/L", "W/A/L/S", "W/A/L/L", "W/B/L/S",
             "W/A/S/S", "W/B/S/L", "W/B/S/S", "W/A/S/L"))

scoreboard_final_new <- scoreboard_final_new %>%
  separate(Scenario, into = c("Confounding.Strength", "Confounding.Source",
                              "Impact.Heterogeneity", "Idiosyncrasy.of.Impacts"), sep = "/")

# Map each column to its respective category
scoreboard_final_new <- scoreboard_final_new %>%
  mutate(
    Confounding.Strength = case_when(
      Confounding.Strength == "N" ~ "None",
      Confounding.Strength == "S" ~ "Strong",
      Confounding.Strength == "W" ~ "Weak"
    ),
    Confounding.Source = case_when(
      Confounding.Source == "N" ~ "None",
      Confounding.Source == "A" ~ "Source A",
      Confounding.Source == "B" ~ "Source B"
    ),
    Impact.Heterogeneity = case_when(
      Impact.Heterogeneity == "L" ~ "Large",
      Impact.Heterogeneity == "S" ~ "Small"
    ),
    Idiosyncrasy.of.Impacts = case_when(
      Idiosyncrasy.of.Impacts == "L" ~ "Large",
      Idiosyncrasy.of.Impacts == "S" ~ "Small"
    )
  )

scoreboard_final_new %>% colnames
scoreboard_final_new$plugin <- ifelse(is.na(scoreboard_final_new$plugin), "no plugin", scoreboard_final_new$plugin)
scoreboard_final_new$plugin %>% table
# Run once (the summary file was saved here):
# save(scoreboard_final_new, file = "results/ACIC_2022/scoreboard_final_new.Rdata")

scoreboard_final_new$Idiosyncrasy.of.Impacts %>% table
scoreboard_final_new_forplot <- scoreboard_final_new %>%
  mutate(Confounding.Strength = factor(Confounding.Strength, levels = c("Weak", "Strong")),
         Impact.Heterogeneity = factor(Impact.Heterogeneity, levels = c("Small", "Large")),
         Combined = paste(str_remove(Confounding.Source, "Source"), str_sub(Idiosyncrasy.of.Impacts, 1, 1), sep = " / "),
         model = paste0("cLSBP_", str_remove(plugin, " plugin"))) %>%
  filter(variable == "Overall") %>%
  group_by(Confounding.Strength,Impact.Heterogeneity, Combined, plugin, model) %>%
  summarise(
    n = n(),
    bias_overall = mean(bias),
    RMSE = sqrt(mean(bias^2)),
    coverage = mean(covered),
    length = mean(`95%`-`5%`)
  )
}

## overall ATT ##
# bias cLSBPs, 16 Scenarios #
{
scoreboard_final_new_forplot %>% colnames
png("results/ACIC_2022/240904/scoreboard_final_new_overall_ATT_bias.png", 1080, 720)
scoreboard_final_new_forplot %>% filter(Confounding.Strength != "None") %>%
  ggplot(aes(x = Combined, y = bias_overall, color = as.factor(plugin))) +
  geom_point(aes(shape = as.factor(plugin)), size = 3, alpha = 0.5) +
  facet_wrap(~Confounding.Strength+Impact.Heterogeneity, nrow = 1)+
  labs(title = "RMSE for Overall ATT : Strong/Large 4 Scenarios, 200 realizations.",
       subtitle = "LSBP with (4 plug-in methods & no plug-in)")+
  theme_bw() + theme(axis.text.x = element_blank(),
                     axis.text.y = element_text(size = 15),
                     text = element_text(size = 20))
dev.off()
}

# bias, coverage : 0.9847, 0.7069
(scoreboard_final_new_forplot %>% filter(Confounding.Strength != "None", plugin == "BART"))$bias_overall %>% mean # 9.847
(scoreboard_final_new_forplot %>% filter(Confounding.Strength != "None", plugin == "BART"))$coverage %>% mean # 0.7069

# RMSE/coverage/length cLSBPs, 16 Scenarios # -> Figure_2, Figure_3, Figure_4 (241023) (2508 revised: .eps, .pdf)
{
  # === RMSE === (Figure_2)
  p_figure2 <- scoreboard_final_new_forplot %>%
    filter(Confounding.Strength != "None") %>%
    ggplot(aes(x = Combined, y = RMSE, color = model)) +
    geom_point(aes(shape = model), size = 3, alpha = 0.5) +
    facet_wrap(~Confounding.Strength+Impact.Heterogeneity, nrow = 1) +
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500",
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3")) +
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 10, "cLSBP_no" = 7)) +
    ylim(c(10, 50)) +
    labs(x = "Scenario", y = "RMSE") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          text = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.position = "none")

  output_base_figure2 <- "results/ACIC_2022/240904/scoreboard_final_new_overall_ATT_RMSE_shorter"

  cairo_ps(paste0(output_base_figure2, ".eps"), width = 15, height = 4, family = "Times")
  print(p_figure2)
  dev.off()

  cairo_pdf(paste0(output_base_figure2, ".pdf"), width = 15, height = 4, family = "Times")
  print(p_figure2)
  dev.off()

  # === coverage === (Figure_3)
  p_figure3 <- scoreboard_final_new_forplot %>%
    filter(Confounding.Strength != "None") %>%
    ggplot(aes(x = Combined, y = coverage, color = model)) +
    geom_point(aes(shape = model), size = 3, alpha = 0.5) +
    geom_hline(yintercept = 0.9, col = "red") +
    facet_wrap(~Confounding.Strength+Impact.Heterogeneity, nrow = 1) +
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500",
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3")) +
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 10, "cLSBP_no" = 7)) +
    ylim(c(0, 1)) +
    labs(x = "Scenario", y = "Coverage") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          text = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.position = "none")

  output_base_figure3 <- "results/ACIC_2022/240904/scoreboard_final_new_overall_ATT_coverage_shorter"

  cairo_ps(paste0(output_base_figure3, ".eps"), width = 15, height = 4, family = "Times")
  print(p_figure3)
  dev.off()

  cairo_pdf(paste0(output_base_figure3, ".pdf"), width = 15, height = 4, family = "Times")
  print(p_figure3)
  dev.off()

  # === length === (Figure_4)
  p_figure4 <- scoreboard_final_new_forplot %>%
    filter(Confounding.Strength != "None") %>%
    ggplot(aes(x = Combined, y = length, color = model)) +
    geom_point(aes(shape = model), size = 3, alpha = 0.5) +
    facet_wrap(~Confounding.Strength+Impact.Heterogeneity, nrow = 1) +
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500",
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3")) +
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 10, "cLSBP_no" = 7)) +
    labs(x = "Scenario", y = "Length") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          text = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.position = "bottom")

  output_base_figure4 <- "results/ACIC_2022/240904/scoreboard_final_new_overall_ATT_length_shorter"

  cairo_ps(paste0(output_base_figure4, ".eps"), width = 15, height = 4.5, family = "Times")
  print(p_figure4)
  dev.off()

  cairo_pdf(paste0(output_base_figure4, ".pdf"), width = 15, height = 4.5, family = "Times")
  print(p_figure4)
  dev.off()
}
# cLSBP_BART single performance
{
scoreboard_final_new_forplot %>% filter(Confounding.Strength != "None") %>%
  select(plugin, RMSE, coverage, length) %>% group_by(plugin) %>% summarise(
    RMSE = sqrt(mean((RMSE)^2)),
    coverage = mean(coverage),
    length = mean(length)
  ) %>% filter(plugin == "BART")
}

# bias, ALL estimators # # -> Do not put in the paper!(241022)
# make df

### Call ACIC_results first!

{
overall_ATT_bias_forplot <- rbind(
scoreboard_final_new_forplot %>% filter(Confounding.Strength != "None") %>%
  select(plugin, RMSE, coverage, length, bias_overall) %>% group_by(plugin) %>% summarise(
    RMSE = sqrt(mean((RMSE)^2)),
    coverage = mean(coverage),
    length = mean(length),
    bias = mean(bias_overall)
  ) %>% mutate(Submission = paste0("cLSBP_", ifelse(plugin == "no plugin", "no", plugin))) %>% select(-plugin)
,
# Bring other estimators results
ACIC_results %>% filter(Confounding.Strength != "None", Confounding.Strength != "All", variable == "Overall") %>% group_by(
  Submission
) %>% summarise(
  RMSE = sqrt(mean((rmse)^2)),
  coverage = mean(cover),
  length = mean(width),
  bias = mean(bias)
)
)
modified_data <- overall_ATT_bias_forplot %>%
  mutate(
    # Replace outlier values with boundaries
    bias = case_when(
      bias > 30 ~ 30,
      bias < -30 ~ -30,
      TRUE ~ bias
    ),
    # Create color labels based on Submission
    submission_label = case_when(
      str_detect(Submission, "cLSBP") ~ "cLSBP",
      TRUE ~ "others"
    ),
    shape_label = case_when(
      str_detect(Submission, "\\*") ~ "unofficial",  # Use character labels
      TRUE ~ "official"  # Use character labels
    )
  )
cLSBP_BART_point <- modified_data %>%
  filter(Submission == "cLSBP_BART")

modified_data <- overall_ATT_bias_forplot %>%
  mutate(
    # Replace outlier values with boundaries
    bias = case_when(
      bias > 30 ~ 30,
      bias < -30 ~ -30,
      TRUE ~ bias
    ),
    # Create color labels based on Submission
    submission_label = case_when(
      str_detect(Submission, "cLSBP") ~ "cLSBP",
      TRUE ~ "others"
    ),
    shape_label = case_when(
      str_detect(Submission, "\\*") ~ "unofficial",  # Use character labels
      TRUE ~ "official"  # Use character labels
    )
  )
cLSBP_BART_point <- modified_data %>%
  filter(Submission == "cLSBP_BART")
}
# draw plot
{
png("results/ACIC_2022/240904/scoreboard_final_new_overall_ATT_bias_ALL.png", 840, 480)
p <- modified_data %>%
  ggplot(aes(x = coverage, y = bias, col = submission_label, shape = shape_label)) +  # Use new shape aesthetic
  geom_point(size = 3) +  # Increase point size if needed
  coord_cartesian(ylim = c(-30, 30)) +  # Set y-axis limits
  labs(x = "Coverage",  # Add x-axis title
       y = "Bias") +  # Add y-axis title
  theme_bw() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_blank(),
        # legend.position = "bottom",
        text = element_text(size = 20)) +
  scale_shape_manual(values = c("unofficial" = 17, "official" = 16)) +
  geom_text(data = cLSBP_BART_point, aes(label = "cLSBP_BART"), vjust = 2, col = "red", size = 7) +  # Label the point
  geom_vline(xintercept = 0.9, linetype = "dashed", color = "black")
print(p)
dev.off()
}

modified_data %>% colnames
modified_data %>% head

overall_ATT_bias_forplot %>% dim

# RMSE vs coverage v3(100+) (Figure_5)
{
  p_figure5 <- overall_ATT_bias_forplot %>% mutate(
    model = case_when(
      str_detect(Submission, "cLSBP") ~ Submission,
      str_detect(Submission, "\\*") ~ "unofficial",
      TRUE ~ "official"
    ),
    RMSE = ifelse(RMSE < 100, RMSE, 100)
  ) %>% ggplot(aes(x = coverage, y = RMSE)) +
    geom_point(
      data = . %>% filter(!str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_point(
      data = . %>% filter(str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_vline(xintercept = 0.9, linetype = "dashed", color = "black") +
    xlim(c(0, 1)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                       labels = c("0", "20", "40", "60", "80", "100+"),
                       limits = c(0, 100)) +
    labs(x = "Coverage", y = "RMSE") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30),
          legend.title = element_blank(),
          legend.text = element_text(size = 25),
          legend.position = "bottom",
          legend.margin = margin(r = 50),
          text = element_text(size = 40),
          plot.margin = margin(t = 15, l = 15, r = 30, b = 15)) +
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500",
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3",
                                  "unofficial" = "grey", "official" = "grey")) +
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 10, "cLSBP_no" = 7,
                                  "unofficial" = 17, "official" = 16)) +
    scale_size_manual(values = c("cLSBP_BART" = 7, "cLSBP_CBPS" = 7, "cLSBP_GBM" = 7, "cLSBP_LASSO" = 7, "cLSBP_no" = 7,
                                 "unofficial" = 5, "official" = 5)) +
    guides(col = guide_legend(nrow = 2, byrow = TRUE),
           shape = guide_legend(nrow = 2, byrow = TRUE))

  output_base_figure5 <- "results/ACIC_2022/240904/scoreboard_final_new_overall_ATT_RMSE_coverage_v3"

  cairo_ps(paste0(output_base_figure5, ".eps"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure5)
  dev.off()

  cairo_pdf(paste0(output_base_figure5, ".pdf"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure5)
  dev.off()
}

# Length vs coverage v3(100+) (Figure_6)
{
  p_figure6 <- overall_ATT_bias_forplot %>% mutate(
    model = case_when(
      str_detect(Submission, "cLSBP") ~ Submission,
      str_detect(Submission, "\\*") ~ "unofficial",
      TRUE ~ "official"
    ),
    length = ifelse(length < 100, length, 100)
  ) %>% ggplot(aes(x = coverage, y = length)) +
    geom_point(
      data = . %>% filter(!str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_point(
      data = . %>% filter(str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_vline(xintercept = 0.9, linetype = "dashed", color = "black") +
    xlim(c(0, 1)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                       labels = c("0", "20", "40", "60", "80", "100+"),
                       limits = c(0, 100)) +
    labs(x = "Coverage", y = "Length") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30),
          legend.title = element_blank(),
          legend.text = element_text(size = 25),
          legend.position = "bottom",
          legend.margin = margin(r = 50),
          text = element_text(size = 40),
          plot.margin = margin(t = 15, l = 15, r = 30, b = 15)) +
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500",
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3",
                                  "unofficial" = "grey", "official" = "grey")) +
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 10, "cLSBP_no" = 7,
                                  "unofficial" = 17, "official" = 16)) +
    scale_size_manual(values = c("cLSBP_BART" = 7, "cLSBP_CBPS" = 7, "cLSBP_GBM" = 7, "cLSBP_LASSO" = 7, "cLSBP_no" = 7,
                                 "unofficial" = 5, "official" = 5)) +
    guides(col = guide_legend(nrow = 2, byrow = TRUE),
           shape = guide_legend(nrow = 2, byrow = TRUE))

  output_base_figure6 <- "results/ACIC_2022/240904/scoreboard_final_new_overall_ATT_length_coverage_v3"

  cairo_ps(paste0(output_base_figure6, ".eps"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure6)
  dev.off()

  cairo_pdf(paste0(output_base_figure6, ".pdf"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure6)
  dev.off()
}

## DRMSE ##
# make df with subgroup sizes
{
scoreboard_DRMSEs <- scoreboard_final_new %>% group_by(dataset.num, plugin) %>% mutate(
  differential_error = ifelse(variable == "Overall", 0, bias-bias[variable == "Overall"])
) %>% ungroup %>% select(-c("Confounding.Strength", "Confounding.Source", "Impact.Heterogeneity", "Idiosyncrasy.of.Impacts")) %>% group_by(
  variable, level, plugin
) %>% summarise(
  DRMSE = sqrt(mean(differential_error^2))
) %>% ungroup %>% filter(!is.na(level))

## get subgroup sizes : ACIC2022_subgroup_size.R ##
# Run once (kept to document how all_dataset_subgroup_size.rds was created):
# {
# files <- list.files(path = "results/ACIC_2022/subgroup_size", pattern = "^subgroup_size_.*.rds$", full.names = TRUE)
# all_dataset_subgroup_size <- do.call(rbind, lapply(files, readRDS))
# saveRDS(all_dataset_subgroup_size, file = "results/ACIC_2022/all_dataset_subgroup_size.rds")
# }
all_dataset_subgroup_size <- readRDS(file = "results/ACIC_2022/all_dataset_subgroup_size.rds")

subgroup_CATT_df <- cbind(scoreboard_DRMSEs %>% mutate(model = paste0("cLSBP_", plugin)) %>% select(-"plugin"),
                          data.frame(sample_size = rep(all_dataset_subgroup_size %>% apply(2, mean), each = 5))
                          )
}

# DRMSE v1 (241023) (Figure_4)#

# Figure_4
{
  p_figure4 <- subgroup_CATT_df %>%
    mutate(
      model = ifelse(model == "cLSBP_no plugin", "cLSBP_no", model),
      sample_size = sample_size / 1000
    ) %>%
    ggplot(aes(x = sample_size, y = DRMSE, col = model)) +
    geom_point(aes(shape = model), size = 3, alpha = 0.4) +
    geom_line(alpha = 0.4) +
    xlim(c(0, 200)) +
    ylim(c(0, 50)) +
    labs(
      x = "Subgroup sample size(K)"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      legend.position = "bottom",
      text = element_text(size = 20)
    )

  output_base_figure4 <- "results/ACIC_2022/240904/subgroup_CATT_DRMSE"

  cairo_ps(paste0(output_base_figure4, ".eps"), width = 15, height = 8.33, family = "Times")
  print(p_figure4)
  dev.off()

  cairo_pdf(paste0(output_base_figure4, ".pdf"), width = 15, height = 8.33, family = "Times")
  print(p_figure4)
  dev.off()
}

subgroup_CATT_df <- subgroup_CATT_df %>% rbind(cbind(scoreboard_DRMSEs_old %>% mutate(model = paste0("cLSBP_old", plugin)) %>% select(-"plugin"), data.frame(sample_size = rep(all_dataset_subgroup_size %>% apply(2, mean), each = 5))))
subgroup_CATT_df %>% ggplot(aes(x = sample_size, y = DRMSE, col = model)) +
  geom_point(aes(shape = variable), alpha = 0.4, size = 3) +
  geom_line(alpha = 0.4) + xlim(c(0, 200000))

# DRMSE v2 (241023) : shape by the "subgroup variable", values X1~X5 #

{
  png("results/ACIC_2022/240904/subgroup_CATT_DRMSE_v2.png", 1080, 600)
  p <- subgroup_CATT_df %>%
    mutate(
      model = ifelse(model == "cLSBP_no plugin", "cLSBP_no", model),
      sample_size = sample_size/1000
    ) %>%
    ggplot(aes(x = sample_size, y = DRMSE, col = model)) + geom_point(aes(shape = variable), size = 3, alpha = 0.4) +
    geom_line(alpha = 0.4) + xlim(c(0, 200)) + ylim(c(0, 50)) +
    labs(
      x = "Subgroup sample size(K)"
    )+
    theme_bw() + theme(axis.text.x = element_text(size = 15),
                       axis.text.y = element_text(size = 15),
                       legend.position = "bottom",
                       text = element_text(size = 20))
  print(p)
  dev.off()
  }

scoreboard_final_new %>% group_by(dataset.num, plugin) %>% mutate(
  differential_error = ifelse(variable == "overall", 0, bias-bias[variable == "Overall"])
) %>% ungroup %>% select(-c("Confounding.Strength", "Confounding.Source", "Impact.Heterogeneity", "Idiosyncrasy.of.Impacts")) %>% group_by(
  variable, level
) %>% summarise(
  DRMSE = sqrt(mean(differential_error^2))
) %>% ungroup %>% filter(!is.na(level))

# (DRMSE is not computed for the other estimators)

## UTE ##

# Check the ACIC_results & ACIC_truths format for practice result.
{
# Scenario : All / variable : Subgroup, (Practices)
scoreboard_final_new %>% dim
scoreboard_final_new %>% colnames
ACIC_results %>% colnames
ACIC_results %>% dim
ACIC_results %>% filter(Submission == "BCFPS") %>% dim # 18 * 17 (17+1 Scenario, 15+2 estimand)

ACIC_results %>% filter(Submission == "BCFPS") %>% head(15)

ACIC_truths <- read.csv("data/ACIC_estimand_truths.csv")
}

ACIC_results$Submission %>% table

# exercise
{
  rbind %>% do.call(ps_estimator %>% lapply(function(ps_method){
    cbind(
      ACIC_truths %>% filter(dataset.num == 3397) %>% filter(is.na(variable)) %>%
        select(-c(variable, level, year)),
      readRDS(paste0("results/ACIC_2022/240904/", "None_Large", "/LSBP_1_mixture/test_", 3397, "_LSBP_1_mixture_prior_", ps_method, "answers_practice.rds")) %>% data.frame
    ) %>%
      mutate(model = paste0("cLSBP_", ps_method),
             bias = X50.-SATT,
             covered = (X5. < SATT) & (X95. > SATT),
             length = X95.-X5.)
  }))
}
# Call and create cLSBP practice result dataframe
{
scenarios <- c("None_Large", "Strong_Large", "Strong_Small", "Weak_Large", "Weak_Small")
ps_estimator <- list("13_", "13_CBPS_", "13_BART_", "13_LASSO_", "13_GBM_")

scoreboard_final_new_practice <- rbind %>% do.call(
  scenarios %>% lapply(function(scenario){
    split_scenarios_cat <- unlist(strsplit(scenario, "_", fixed = TRUE))
    dataset_indices <- ((ACIC_truths %>% filter(Confounding.Strength == split_scenarios_cat[1], Impact.Heterogeneity == split_scenarios_cat[2]))$dataset.num) %>% unique
    rbind %>% do.call(dataset_indices %>% lapply(function(dataset_index){
      d_n <- str_sub(paste0("000", dataset_index), start = -4)
      rbind %>% do.call(ps_estimator %>% lapply(function(ps_method){
        cbind(
          ACIC_truths %>% filter(dataset.num == dataset_index) %>% filter(is.na(variable)) %>%
            select(-c(variable, level, year)),
          readRDS(paste0("results/ACIC_2022/240904/", scenario, "/LSBP_1_mixture/test_", d_n, "_LSBP_1_mixture_prior_", ps_method, "answers_practice.rds")) %>% data.frame
        ) %>%
          mutate(model = paste0("cLSBP_", ps_method),
                 bias = X50.-SATT,
                 covered = (X5. < SATT) & (X95. > SATT),
                 length = X95.-X5.)
      }))
    }))
  })
)
}

# Make the final UTE result dataframes

UTE_scoreboard_cLSBP_overall <- scoreboard_final_new_practice %>% group_by(model) %>% summarise(
  RMSE = sqrt(mean((bias)^2)),
  coverage = mean(covered),
  length = mean(length)
) %>% mutate(Submission = sub("cLSBP_13_([^_]*).*", "\\1", model)) %>% mutate(Submission = paste0("cLSBP_", ifelse(Submission == "", "no_plugin", Submission))) %>% select(-model)

UTE_scoreboard_others_overall <- ACIC_results %>% filter(variable == "Practices", Confounding.Source == "All") %>%
  mutate(RMSE = rmse, coverage = cover, length = width) %>%
  select(Submission, RMSE, coverage, length)

UTE_scoreboard_cLSBP_overall$Submission <- str_remove(UTE_scoreboard_cLSBP_overall$Submission, "_plugin")

# Run once (the summary files were saved here):
# saveRDS(UTE_scoreboard_cLSBP_overall, "results/ACIC_2022/240904/UTE_scoreboard_cLSBP_overall.rds")
# saveRDS(UTE_scoreboard_others_overall, "results/ACIC_2022/240904/UTE_scoreboard_others_overall.rds")

UTE_scoreboard_cLSBP_overall <- readRDS("results/ACIC_2022/240904/UTE_scoreboard_cLSBP_overall.rds")
UTE_scoreboard_others_overall <- readRDS("results/ACIC_2022/240904/UTE_scoreboard_others_overall.rds")

# Draw the plot

UTE_results_total <- rbind(UTE_scoreboard_cLSBP_overall, UTE_scoreboard_others_overall %>% select(-shape_label))

# RMSE vs coverage v2(breaking 180~600)
{
png("results/ACIC_2022/240904/UTE_RMSE_coverage_v2.png", 840, 600)
p <- UTE_results_total %>% mutate(
  model = case_when(
    str_detect(Submission, "cLSBP") ~ Submission,
    str_detect(Submission, "\\*") ~ "unofficial",
    TRUE ~ "official"
  )
) %>% ggplot(aes(x = coverage, y = RMSE)) +
  geom_point(aes(col = model, shape = model), size = 5)+
  geom_vline(xintercept = 0.9, linetype = "dashed", color = "black") +
  xlim(c(0, 1)) +
  scale_y_continuous(breaks = c(0, 30, 60, 90, 120, 150, 180, 600, 630),
                     limits = c(0, 630))+
  scale_y_break(c(180, 600))+
  labs(x = "Coverage",  # Add x-axis title
       y = "RMSE") +  # Add y-axis title
  theme_bw() +
  theme(axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.position = "bottom",
        legend.margin = margin(r = 50),
        text = element_text(size = 40),
        plot.margin = margin(t = 15, l = 5, r = 30, b = 15))+
  scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                "cLSBP_CBPS" = "#A3A500" ,
                                "cLSBP_GBM" = "#00BF7D",
                                "cLSBP_LASSO" = "#00B0F6",
                                "cLSBP_no" = "#E76BF3",
                                "unofficial" = "grey", "official" = "grey"))+
  scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 3, "cLSBP_no" = 7,
                                "unofficial" = 17, "official" = 16))+
  guides(col = guide_legend(nrow = 2, byrow = T),
         shape = guide_legend(nrow = 2, byrow = T))
print(p)
dev.off()
}
# RMSE vs coverage v3(200+) (Figure_5a)
{
  p_figure5a <- UTE_results_total %>% mutate(
    model = case_when(
      str_detect(Submission, "cLSBP") ~ Submission,
      str_detect(Submission, "\\*") ~ "unofficial",
      TRUE ~ "official"
    ),
    RMSE = ifelse(RMSE < 200, RMSE, 200)
  ) %>% ggplot(aes(x = coverage, y = RMSE)) +
    geom_point(
      data = . %>% filter(!str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_point(
      data = . %>% filter(str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_vline(xintercept = 0.9, linetype = "dashed", color = "black") +
    xlim(c(0, 1)) +
    scale_y_continuous(breaks = c(0, 50, 100, 150, 200),
                       labels = c("0", "50", "100", "150", "200+"),
                       limits = c(0, 200)) +
    labs(x = "Coverage", y = "RMSE") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30),
          legend.title = element_blank(),
          legend.text = element_text(size = 25),
          legend.position = "bottom",
          legend.margin = margin(r = 50),
          text = element_text(size = 40),
          plot.margin = margin(t = 15, l = 15, r = 30, b = 15)) +
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500",
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3",
                                  "unofficial" = "grey", "official" = "grey")) +
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 10, "cLSBP_no" = 7,
                                  "unofficial" = 17, "official" = 16)) +
    scale_size_manual(values = c("cLSBP_BART" = 7, "cLSBP_CBPS" = 7, "cLSBP_GBM" = 7, "cLSBP_LASSO" = 7, "cLSBP_no" = 7,
                                 "unofficial" = 5, "official" = 5)) +
    guides(col = guide_legend(nrow = 2, byrow = TRUE),
           shape = guide_legend(nrow = 2, byrow = TRUE))

  output_base_figure5a <- "results/ACIC_2022/240904/UTE_RMSE_coverage_v3"

  cairo_ps(paste0(output_base_figure5a, ".eps"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure5a)
  dev.off()

  cairo_pdf(paste0(output_base_figure5a, ".pdf"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure5a)
  dev.off()
}
# Length vs coverage v3(500+) (Figure_5b)
{
  p_figure5b <- UTE_results_total %>% mutate(
    model = case_when(
      str_detect(Submission, "cLSBP") ~ Submission,
      str_detect(Submission, "\\*") ~ "unofficial",
      TRUE ~ "official"
    ),
    length = ifelse(length < 500, length, 500)
  ) %>% ggplot(aes(x = coverage, y = length)) +
    geom_point(
      data = . %>% filter(!str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_point(
      data = . %>% filter(str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_vline(xintercept = 0.9, linetype = "dashed", color = "black") +
    xlim(c(0, 1)) +
    scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500),
                       labels = c("0", "100", "200", "300", "400", "500+"),
                       limits = c(0, 500)) +
    labs(x = "Coverage", y = "Length") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30),
          legend.title = element_blank(),
          legend.text = element_text(size = 25),
          legend.position = "bottom",
          legend.margin = margin(r = 50),
          text = element_text(size = 40),
          plot.margin = margin(t = 15, l = 15, r = 30, b = 15)) +
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500",
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3",
                                  "unofficial" = "grey", "official" = "grey")) +
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 10, "cLSBP_no" = 7,
                                  "unofficial" = 17, "official" = 16)) +
    scale_size_manual(values = c("cLSBP_BART" = 7, "cLSBP_CBPS" = 7, "cLSBP_GBM" = 7, "cLSBP_LASSO" = 7, "cLSBP_no" = 7,
                                 "unofficial" = 5, "official" = 5)) +
    guides(col = guide_legend(nrow = 2, byrow = TRUE),
           shape = guide_legend(nrow = 2, byrow = TRUE))

  output_base_figure5b <- "results/ACIC_2022/240904/UTE_length_coverage_v3"

  cairo_ps(paste0(output_base_figure5b, ".eps"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure5b)
  dev.off()

  cairo_pdf(paste0(output_base_figure5b, ".pdf"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure5b)
  dev.off()
}

## 251203 (Recalculate UTE weighted mean by practice-year) ##

## Do it from here ##

## DRAW PLOTS (Figure_2, )

# Create scoreboard_final_new & forplot
{
  date <- "251203"
  dir_result <- paste0("results/ACIC_2022/", date, "/")
  load(file = paste0(dir_result, "problem_df.Rdata"))

  scenarios <- c("None_Large", "Strong_Large", "Strong_Small", "Weak_Large", "Weak_Small")
  ps_estimator <- list("13_", "13_CBPS_", "13_BART_", "13_LASSO_", "13_GBM_")

  scoreboard_final_new <- rbind %>% do.call(
    scenarios %>% lapply(function(scenarios_cat){
      split_scenarios_cat <- unlist(strsplit(scenarios_cat, "_", fixed = TRUE))
      scenarios_df <- problem_df %>% filter(Confounding.Strength == split_scenarios_cat[1], Impact.Heterogeneity == split_scenarios_cat[2])
      rbind %>% do.call(
        ps_estimator %>% lapply(function(ps_method){
          rbind %>% do.call(
            list.files(path = paste0("results/ACIC_2022/", date, "/", scenarios_cat, "/LSBP_1_mixture"), pattern = paste0("test_.*", ps_method, "answers\\.rds"), full.names = T) %>% lapply(readRDS)
          )
        })
      )
    })
  )
  scoreboard_final_new$Scenario %>% table
  scoreboard_final_new$Scenario %>% unique
  scoreboard_final_new$Scenario <- factor(
    scoreboard_final_new$Scenario, levels = scoreboard_final_new$Scenario %>% unique,
    labels = c("N/N/L/S",
               "S/A/L/L", "S/A/L/S", "S/B/L/L", "S/B/L/S",
               "S/B/S/L", "S/A/S/S", "S/A/S/L", "S/B/S/S",
               "W/B/L/L", "W/A/L/S", "W/A/L/L", "W/B/L/S",
               "W/A/S/S", "W/B/S/L", "W/B/S/S", "W/A/S/L"))

  scoreboard_final_new <- scoreboard_final_new %>%
    separate(Scenario, into = c("Confounding.Strength", "Confounding.Source",
                                "Impact.Heterogeneity", "Idiosyncrasy.of.Impacts"), sep = "/")

  # Map each column to its respective category
  scoreboard_final_new <- scoreboard_final_new %>%
    mutate(
      Confounding.Strength = case_when(
        Confounding.Strength == "N" ~ "None",
        Confounding.Strength == "S" ~ "Strong",
        Confounding.Strength == "W" ~ "Weak"
      ),
      Confounding.Source = case_when(
        Confounding.Source == "N" ~ "None",
        Confounding.Source == "A" ~ "Source A",
        Confounding.Source == "B" ~ "Source B"
      ),
      Impact.Heterogeneity = case_when(
        Impact.Heterogeneity == "L" ~ "Large",
        Impact.Heterogeneity == "S" ~ "Small"
      ),
      Idiosyncrasy.of.Impacts = case_when(
        Idiosyncrasy.of.Impacts == "L" ~ "Large",
        Idiosyncrasy.of.Impacts == "S" ~ "Small"
      )
    )

  scoreboard_final_new %>% colnames
  scoreboard_final_new$plugin <- ifelse(is.na(scoreboard_final_new$plugin), "no plugin", scoreboard_final_new$plugin)
  scoreboard_final_new$plugin %>% table
  # Run once (the summary file was saved here):
  # save(scoreboard_final_new, file = "results/ACIC_2022/scoreboard_final_new.Rdata")

  scoreboard_final_new$Idiosyncrasy.of.Impacts %>% table
  scoreboard_final_new_forplot <- scoreboard_final_new %>%
    mutate(Confounding.Strength = factor(Confounding.Strength, levels = c("Weak", "Strong")),
           Impact.Heterogeneity = factor(Impact.Heterogeneity, levels = c("Small", "Large")),
           Combined = paste(str_remove(Confounding.Source, "Source"), str_sub(Idiosyncrasy.of.Impacts, 1, 1), sep = " / "),
           model = paste0("cLSBP_", str_remove(plugin, " plugin"))) %>%
    filter(variable == "Overall") %>%
    group_by(Confounding.Strength,Impact.Heterogeneity, Combined, plugin, model) %>%
    summarise(
      n = n(),
      bias_overall = mean(bias),
      RMSE = sqrt(mean(bias^2)),
      coverage = mean(covered),
      length = mean(`95%`-`5%`)
    )
}

## overall ATT ##
# bias cLSBPs, 16 Scenarios #
{
  scoreboard_final_new_forplot %>% colnames
  png(paste0("results/ACIC_2022/", date ,"/scoreboard_final_new_overall_ATT_bias.png"), 1080, 720)
  scoreboard_final_new_forplot %>% filter(Confounding.Strength != "None") %>%
    ggplot(aes(x = Combined, y = bias_overall, color = as.factor(plugin))) +
    geom_point(aes(shape = as.factor(plugin)), size = 3, alpha = 0.5) +
    facet_wrap(~Confounding.Strength+Impact.Heterogeneity, nrow = 1)+
    labs(title = "RMSE for Overall ATT : Strong/Large 4 Scenarios, 200 realizations.",
         subtitle = "LSBP with (4 plug-in methods & no plug-in)")+
    theme_bw() + theme(axis.text.x = element_blank(),
                       axis.text.y = element_text(size = 15),
                       text = element_text(size = 20))
  dev.off()
}

# bias, coverage : 0.9847, 0.7069
(scoreboard_final_new_forplot %>% filter(Confounding.Strength != "None", plugin == "BART"))$bias_overall %>% mean # 9.847
(scoreboard_final_new_forplot %>% filter(Confounding.Strength != "None", plugin == "BART"))$coverage %>% mean # 0.7069

# RMSE/coverage/length cLSBPs, 16 Scenarios # -> Figure_2, Figure_3, Figure_4 (241023) (2508 revised: .eps, .pdf)
{
  # === RMSE === (Figure_2)
  p_figure2 <- scoreboard_final_new_forplot %>%
    filter(Confounding.Strength != "None") %>%
    ggplot(aes(x = Combined, y = RMSE, color = model)) +
    geom_point(aes(shape = model), size = 3, alpha = 0.5) +
    facet_wrap(~Confounding.Strength+Impact.Heterogeneity, nrow = 1) +
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500",
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3")) +
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 10, "cLSBP_no" = 7)) +
    ylim(c(10, 50)) +
    labs(x = "Scenario", y = "RMSE") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          text = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.position = "none")

  output_base_figure2 <- paste0("results/ACIC_2022/", date ,"/scoreboard_final_new_overall_ATT_RMSE_shorter")

  cairo_ps(paste0(output_base_figure2, ".eps"), width = 15, height = 4) #, family = "Times"
  print(p_figure2)
  dev.off()

  pdf(paste0(output_base_figure2, ".pdf"), width = 15, height = 4)
  print(p_figure2)
  dev.off()

  # === coverage === (Figure_3)
  p_figure3 <- scoreboard_final_new_forplot %>%
    filter(Confounding.Strength != "None") %>%
    ggplot(aes(x = Combined, y = coverage, color = model)) +
    geom_point(aes(shape = model), size = 3, alpha = 0.5) +
    geom_hline(yintercept = 0.9, col = "red") +
    facet_wrap(~Confounding.Strength+Impact.Heterogeneity, nrow = 1) +
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500",
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3")) +
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 10, "cLSBP_no" = 7)) +
    ylim(c(0, 1)) +
    labs(x = "Scenario", y = "Coverage") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          text = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.position = "none")

  output_base_figure3 <- paste0("results/ACIC_2022/", date ,"/scoreboard_final_new_overall_ATT_coverage_shorter")

  cairo_ps(paste0(output_base_figure3, ".eps"), width = 15, height = 4)# , family = "Times"
  print(p_figure3)
  dev.off()

  pdf(paste0(output_base_figure3, ".pdf"), width = 15, height = 4)
  print(p_figure3)
  dev.off()

  # === length === (Figure_4)
  p_figure4 <- scoreboard_final_new_forplot %>%
    filter(Confounding.Strength != "None") %>%
    ggplot(aes(x = Combined, y = length, color = model)) +
    geom_point(aes(shape = model), size = 3, alpha = 0.5) +
    facet_wrap(~Confounding.Strength+Impact.Heterogeneity, nrow = 1) +
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500",
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3")) +
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 10, "cLSBP_no" = 7)) +
    labs(x = "Scenario", y = "Length") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          text = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.position = "bottom")

  output_base_figure4 <- paste0("results/ACIC_2022/", date ,"/scoreboard_final_new_overall_ATT_length_shorter")

  cairo_ps(paste0(output_base_figure4, ".eps"), width = 15, height = 4.5)# , family = "Times"
  print(p_figure4)
  dev.off()

  cairo_pdf(paste0(output_base_figure4, ".pdf"), width = 15, height = 4.5)# , family = "Times"
  pdf(paste0(output_base_figure4, ".pdf"), width = 15, height = 4.5)# , family = "Times"
  print(p_figure4)
  dev.off()
}
# cLSBP_BART single performance
{
  scoreboard_final_new_forplot %>% filter(Confounding.Strength != "None") %>%
    select(plugin, RMSE, coverage, length) %>% group_by(plugin) %>% summarise(
      RMSE = sqrt(mean((RMSE)^2)),
      coverage = mean(coverage),
      length = mean(length)
    ) %>% filter(plugin == "BART")
}

# bias, ALL estimators # # -> Do not put in the paper!(241022)
# make df

### Call ACIC_results first!

{
  overall_ATT_bias_forplot <- rbind(
    scoreboard_final_new_forplot %>% filter(Confounding.Strength != "None") %>%
      select(plugin, RMSE, coverage, length, bias_overall) %>% group_by(plugin) %>% summarise(
        RMSE = sqrt(mean((RMSE)^2)),
        coverage = mean(coverage),
        length = mean(length),
        bias = mean(bias_overall)
      ) %>% mutate(Submission = paste0("cLSBP_", ifelse(plugin == "no plugin", "no", plugin))) %>% select(-plugin)
    ,
    # Bring other estimators results
    ACIC_results %>% filter(Confounding.Strength != "None", Confounding.Strength != "All", variable == "Overall") %>% group_by(
      Submission
    ) %>% summarise(
      RMSE = sqrt(mean((rmse)^2)),
      coverage = mean(cover),
      length = mean(width),
      bias = mean(bias)
    )
  )
  modified_data <- overall_ATT_bias_forplot %>%
    mutate(
      # Replace outlier values with boundaries
      bias = case_when(
        bias > 30 ~ 30,
        bias < -30 ~ -30,
        TRUE ~ bias
      ),
      # Create color labels based on Submission
      submission_label = case_when(
        str_detect(Submission, "cLSBP") ~ "cLSBP",
        TRUE ~ "others"
      ),
      shape_label = case_when(
        str_detect(Submission, "\\*") ~ "unofficial",  # Use character labels
        TRUE ~ "official"  # Use character labels
      )
    )
  cLSBP_BART_point <- modified_data %>%
    filter(Submission == "cLSBP_BART")

  modified_data <- overall_ATT_bias_forplot %>%
    mutate(
      # Replace outlier values with boundaries
      bias = case_when(
        bias > 30 ~ 30,
        bias < -30 ~ -30,
        TRUE ~ bias
      ),
      # Create color labels based on Submission
      submission_label = case_when(
        str_detect(Submission, "cLSBP") ~ "cLSBP",
        TRUE ~ "others"
      ),
      shape_label = case_when(
        str_detect(Submission, "\\*") ~ "unofficial",  # Use character labels
        TRUE ~ "official"  # Use character labels
      )
    )
  cLSBP_BART_point <- modified_data %>%
    filter(Submission == "cLSBP_BART")
}
# draw plot
{
  png(paste0("results/ACIC_2022/", date ,"/scoreboard_final_new_overall_ATT_bias_ALL.png"), 840, 480)
  p <- modified_data %>%
    ggplot(aes(x = coverage, y = bias, col = submission_label, shape = shape_label)) +  # Use new shape aesthetic
    geom_point(size = 3) +  # Increase point size if needed
    coord_cartesian(ylim = c(-30, 30)) +  # Set y-axis limits
    labs(x = "Coverage",  # Add x-axis title
         y = "Bias") +  # Add y-axis title
    theme_bw() +
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          legend.title = element_blank(),
          # legend.position = "bottom",
          text = element_text(size = 20)) +
    scale_shape_manual(values = c("unofficial" = 17, "official" = 16)) +
    geom_text(data = cLSBP_BART_point, aes(label = "cLSBP_BART"), vjust = 2, col = "red", size = 7) +  # Label the point
    geom_vline(xintercept = 0.9, linetype = "dashed", color = "black")
  print(p)
  dev.off()
}

modified_data %>% colnames
modified_data %>% head

overall_ATT_bias_forplot %>% dim

# RMSE vs coverage v3(100+) (Figure_5)
{
  p_figure5 <- overall_ATT_bias_forplot %>% mutate(
    model = case_when(
      str_detect(Submission, "cLSBP") ~ Submission,
      str_detect(Submission, "\\*") ~ "unofficial",
      TRUE ~ "official"
    ),
    RMSE = ifelse(RMSE < 100, RMSE, 100)
  ) %>% ggplot(aes(x = coverage, y = RMSE)) +
    geom_point(
      data = . %>% filter(!str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_point(
      data = . %>% filter(str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_vline(xintercept = 0.9, linetype = "dashed", color = "black") +
    xlim(c(0, 1)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                       labels = c("0", "20", "40", "60", "80", "100+"),
                       limits = c(0, 100)) +
    labs(x = "Coverage", y = "RMSE") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30),
          legend.title = element_blank(),
          legend.text = element_text(size = 25),
          legend.position = "bottom",
          legend.margin = margin(r = 50),
          text = element_text(size = 40),
          plot.margin = margin(t = 15, l = 15, r = 30, b = 15)) +
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500",
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3",
                                  "unofficial" = "grey", "official" = "grey")) +
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 10, "cLSBP_no" = 7,
                                  "unofficial" = 17, "official" = 16)) +
    scale_size_manual(values = c("cLSBP_BART" = 7, "cLSBP_CBPS" = 7, "cLSBP_GBM" = 7, "cLSBP_LASSO" = 7, "cLSBP_no" = 7,
                                 "unofficial" = 5, "official" = 5)) +
    guides(col = guide_legend(nrow = 2, byrow = TRUE),
           shape = guide_legend(nrow = 2, byrow = TRUE))

  output_base_figure5 <- "results/ACIC_2022/240904/scoreboard_final_new_overall_ATT_RMSE_coverage_v3"

  cairo_ps(paste0(output_base_figure5, ".eps"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure5)
  dev.off()

  cairo_pdf(paste0(output_base_figure5, ".pdf"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure5)
  dev.off()
}

# Length vs coverage v3(100+) (Figure_6)
{
  p_figure6 <- overall_ATT_bias_forplot %>% mutate(
    model = case_when(
      str_detect(Submission, "cLSBP") ~ Submission,
      str_detect(Submission, "\\*") ~ "unofficial",
      TRUE ~ "official"
    ),
    length = ifelse(length < 100, length, 100)
  ) %>% ggplot(aes(x = coverage, y = length)) +
    geom_point(
      data = . %>% filter(!str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_point(
      data = . %>% filter(str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_vline(xintercept = 0.9, linetype = "dashed", color = "black") +
    xlim(c(0, 1)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                       labels = c("0", "20", "40", "60", "80", "100+"),
                       limits = c(0, 100)) +
    labs(x = "Coverage", y = "Length") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30),
          legend.title = element_blank(),
          legend.text = element_text(size = 25),
          legend.position = "bottom",
          legend.margin = margin(r = 50),
          text = element_text(size = 40),
          plot.margin = margin(t = 15, l = 15, r = 30, b = 15)) +
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500",
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3",
                                  "unofficial" = "grey", "official" = "grey")) +
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 10, "cLSBP_no" = 7,
                                  "unofficial" = 17, "official" = 16)) +
    scale_size_manual(values = c("cLSBP_BART" = 7, "cLSBP_CBPS" = 7, "cLSBP_GBM" = 7, "cLSBP_LASSO" = 7, "cLSBP_no" = 7,
                                 "unofficial" = 5, "official" = 5)) +
    guides(col = guide_legend(nrow = 2, byrow = TRUE),
           shape = guide_legend(nrow = 2, byrow = TRUE))

  output_base_figure6 <- "results/ACIC_2022/240904/scoreboard_final_new_overall_ATT_length_coverage_v3"

  cairo_ps(paste0(output_base_figure6, ".eps"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure6)
  dev.off()

  cairo_pdf(paste0(output_base_figure6, ".pdf"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure6)
  dev.off()
}

## DRMSE ##
# make df with subgroup sizes
{
  scoreboard_DRMSEs <- scoreboard_final_new %>% group_by(dataset.num, plugin) %>% mutate(
    differential_error = ifelse(variable == "Overall", 0, bias-bias[variable == "Overall"])
  ) %>% ungroup %>% select(-c("Confounding.Strength", "Confounding.Source", "Impact.Heterogeneity", "Idiosyncrasy.of.Impacts")) %>% group_by(
    variable, level, plugin
  ) %>% summarise(
    DRMSE = sqrt(mean(differential_error^2))
  ) %>% ungroup %>% filter(!is.na(level))

  ## get subgroup sizes : ACIC2022_subgroup_size.R ##
  # Run once (kept to document how all_dataset_subgroup_size.rds was created):
  # {
  # files <- list.files(path = "results/ACIC_2022/subgroup_size", pattern = "^subgroup_size_.*.rds$", full.names = TRUE)
  # all_dataset_subgroup_size <- do.call(rbind, lapply(files, readRDS))
  # saveRDS(all_dataset_subgroup_size, file = "results/ACIC_2022/all_dataset_subgroup_size.rds")
  # }
  all_dataset_subgroup_size <- readRDS(file = "results/ACIC_2022/all_dataset_subgroup_size.rds")

  subgroup_CATT_df <- cbind(scoreboard_DRMSEs %>% mutate(model = paste0("cLSBP_", plugin)) %>% select(-"plugin"),
                            data.frame(sample_size = rep(all_dataset_subgroup_size %>% apply(2, mean), each = 5))
  )
}

# DRMSE v1 (241023) (Figure_4)#

# Figure_4
{
  p_figure4 <- subgroup_CATT_df %>%
    mutate(
      model = ifelse(model == "cLSBP_no plugin", "cLSBP_no", model),
      sample_size = sample_size / 1000
    ) %>%
    ggplot(aes(x = sample_size, y = DRMSE, col = model)) +
    geom_point(aes(shape = model), size = 3, alpha = 0.4) +
    geom_line(alpha = 0.4) +
    xlim(c(0, 200)) +
    ylim(c(0, 50)) +
    labs(
      x = "Subgroup sample size(K)"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      legend.position = "bottom",
      text = element_text(size = 20)
    )

  output_base_figure4 <- "results/ACIC_2022/240904/subgroup_CATT_DRMSE"

  cairo_ps(paste0(output_base_figure4, ".eps"), width = 15, height = 8.33, family = "Times")
  print(p_figure4)
  dev.off()

  cairo_pdf(paste0(output_base_figure4, ".pdf"), width = 15, height = 8.33, family = "Times")
  print(p_figure4)
  dev.off()
}

subgroup_CATT_df <- subgroup_CATT_df %>% rbind(cbind(scoreboard_DRMSEs_old %>% mutate(model = paste0("cLSBP_old", plugin)) %>% select(-"plugin"), data.frame(sample_size = rep(all_dataset_subgroup_size %>% apply(2, mean), each = 5))))
subgroup_CATT_df %>% ggplot(aes(x = sample_size, y = DRMSE, col = model)) +
  geom_point(aes(shape = variable), alpha = 0.4, size = 3) +
  geom_line(alpha = 0.4) + xlim(c(0, 200000))

# DRMSE v2 (241023) : shape by the "subgroup variable", values X1~X5 #

{
  png("results/ACIC_2022/240904/subgroup_CATT_DRMSE_v2.png", 1080, 600)
  p <- subgroup_CATT_df %>%
    mutate(
      model = ifelse(model == "cLSBP_no plugin", "cLSBP_no", model),
      sample_size = sample_size/1000
    ) %>%
    ggplot(aes(x = sample_size, y = DRMSE, col = model)) + geom_point(aes(shape = variable), size = 3, alpha = 0.4) +
    geom_line(alpha = 0.4) + xlim(c(0, 200)) + ylim(c(0, 50)) +
    labs(
      x = "Subgroup sample size(K)"
    )+
    theme_bw() + theme(axis.text.x = element_text(size = 15),
                       axis.text.y = element_text(size = 15),
                       legend.position = "bottom",
                       text = element_text(size = 20))
  print(p)
  dev.off()
  }

scoreboard_final_new %>% group_by(dataset.num, plugin) %>% mutate(
  differential_error = ifelse(variable == "overall", 0, bias-bias[variable == "Overall"])
) %>% ungroup %>% select(-c("Confounding.Strength", "Confounding.Source", "Impact.Heterogeneity", "Idiosyncrasy.of.Impacts")) %>% group_by(
  variable, level
) %>% summarise(
  DRMSE = sqrt(mean(differential_error^2))
) %>% ungroup %>% filter(!is.na(level))

# (DRMSE is not computed for the other estimators)

## UTE ##

# Check the ACIC_results & ACIC_truths format for practice result.
{
  # Scenario : All / variable : Subgroup, (Practices)
  scoreboard_final_new %>% dim
  scoreboard_final_new %>% colnames
  ACIC_results %>% colnames
  ACIC_results %>% dim
  ACIC_results %>% filter(Submission == "BCFPS") %>% dim # 18 * 17 (17+1 Scenario, 15+2 estimand)

  ACIC_results %>% filter(Submission == "BCFPS") %>% head(15)

  ACIC_truths <- read.csv("data/ACIC_estimand_truths.csv")
}

ACIC_results$Submission %>% table

# exercise
{
  rbind %>% do.call(ps_estimator %>% lapply(function(ps_method){
    cbind(
      ACIC_truths %>% filter(dataset.num == 3397) %>% filter(is.na(variable)) %>%
        select(-c(variable, level, year)),
      readRDS(paste0("results/ACIC_2022/240904/", "None_Large", "/LSBP_1_mixture/test_", 3397, "_LSBP_1_mixture_prior_", ps_method, "answers_practice.rds")) %>% data.frame
    ) %>%
      mutate(model = paste0("cLSBP_", ps_method),
             bias = X50.-SATT,
             covered = (X5. < SATT) & (X95. > SATT),
             length = X95.-X5.)
  }))
}
# Call and create cLSBP practice result dataframe
{
  scenarios <- c("None_Large", "Strong_Large", "Strong_Small", "Weak_Large", "Weak_Small")
  ps_estimator <- list("13_", "13_CBPS_", "13_BART_", "13_LASSO_", "13_GBM_")

  scoreboard_final_new_practice <- rbind %>% do.call(
    scenarios %>% lapply(function(scenario){
      split_scenarios_cat <- unlist(strsplit(scenario, "_", fixed = TRUE))
      dataset_indices <- ((ACIC_truths %>% filter(Confounding.Strength == split_scenarios_cat[1], Impact.Heterogeneity == split_scenarios_cat[2]))$dataset.num) %>% unique
      rbind %>% do.call(dataset_indices %>% lapply(function(dataset_index){
        d_n <- str_sub(paste0("000", dataset_index), start = -4)
        rbind %>% do.call(ps_estimator %>% lapply(function(ps_method){
          cbind(
            ACIC_truths %>% filter(dataset.num == dataset_index) %>% filter(is.na(variable)) %>%
              select(-c(variable, level, year)),
            readRDS(paste0("results/ACIC_2022/240904/", scenario, "/LSBP_1_mixture/test_", d_n, "_LSBP_1_mixture_prior_", ps_method, "answers_practice.rds")) %>% data.frame
          ) %>%
            mutate(model = paste0("cLSBP_", ps_method),
                   bias = X50.-SATT,
                   covered = (X5. < SATT) & (X95. > SATT),
                   length = X95.-X5.)
        }))
      }))
    })
  )
}

# Make the final UTE result dataframes

UTE_scoreboard_cLSBP_overall <- scoreboard_final_new_practice %>% group_by(model) %>% summarise(
  RMSE = sqrt(mean((bias)^2)),
  coverage = mean(covered),
  length = mean(length)
) %>% mutate(Submission = sub("cLSBP_13_([^_]*).*", "\\1", model)) %>% mutate(Submission = paste0("cLSBP_", ifelse(Submission == "", "no_plugin", Submission))) %>% select(-model)

UTE_scoreboard_others_overall <- ACIC_results %>% filter(variable == "Practices", Confounding.Source == "All") %>%
  mutate(RMSE = rmse, coverage = cover, length = width) %>%
  select(Submission, RMSE, coverage, length)

UTE_scoreboard_cLSBP_overall$Submission <- str_remove(UTE_scoreboard_cLSBP_overall$Submission, "_plugin")

# Run once (the summary files were saved here):
# saveRDS(UTE_scoreboard_cLSBP_overall, "results/ACIC_2022/240904/UTE_scoreboard_cLSBP_overall.rds")
# saveRDS(UTE_scoreboard_others_overall, "results/ACIC_2022/240904/UTE_scoreboard_others_overall.rds")

UTE_scoreboard_cLSBP_overall <- readRDS("results/ACIC_2022/240904/UTE_scoreboard_cLSBP_overall.rds")
UTE_scoreboard_others_overall <- readRDS("results/ACIC_2022/240904/UTE_scoreboard_others_overall.rds")

# Draw the plot

UTE_results_total <- rbind(UTE_scoreboard_cLSBP_overall, UTE_scoreboard_others_overall %>% select(-shape_label))

# RMSE vs coverage v2(breaking 180~600)
{
  png("results/ACIC_2022/240904/UTE_RMSE_coverage_v2.png", 840, 600)
  p <- UTE_results_total %>% mutate(
    model = case_when(
      str_detect(Submission, "cLSBP") ~ Submission,
      str_detect(Submission, "\\*") ~ "unofficial",
      TRUE ~ "official"
    )
  ) %>% ggplot(aes(x = coverage, y = RMSE)) +
    geom_point(aes(col = model, shape = model), size = 5)+
    geom_vline(xintercept = 0.9, linetype = "dashed", color = "black") +
    xlim(c(0, 1)) +
    scale_y_continuous(breaks = c(0, 30, 60, 90, 120, 150, 180, 600, 630),
                       limits = c(0, 630))+
    scale_y_break(c(180, 600))+
    labs(x = "Coverage",  # Add x-axis title
         y = "RMSE") +  # Add y-axis title
    theme_bw() +
    theme(axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30),
          legend.title = element_blank(),
          legend.text = element_text(size = 25),
          legend.position = "bottom",
          legend.margin = margin(r = 50),
          text = element_text(size = 40),
          plot.margin = margin(t = 15, l = 5, r = 30, b = 15))+
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500" ,
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3",
                                  "unofficial" = "grey", "official" = "grey"))+
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 3, "cLSBP_no" = 7,
                                  "unofficial" = 17, "official" = 16))+
    guides(col = guide_legend(nrow = 2, byrow = T),
           shape = guide_legend(nrow = 2, byrow = T))
  print(p)
  dev.off()
}
# RMSE vs coverage v3(200+) (Figure_5a)
{
  p_figure5a <- UTE_results_total %>% mutate(
    model = case_when(
      str_detect(Submission, "cLSBP") ~ Submission,
      str_detect(Submission, "\\*") ~ "unofficial",
      TRUE ~ "official"
    ),
    RMSE = ifelse(RMSE < 200, RMSE, 200)
  ) %>% ggplot(aes(x = coverage, y = RMSE)) +
    geom_point(
      data = . %>% filter(!str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_point(
      data = . %>% filter(str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_vline(xintercept = 0.9, linetype = "dashed", color = "black") +
    xlim(c(0, 1)) +
    scale_y_continuous(breaks = c(0, 50, 100, 150, 200),
                       labels = c("0", "50", "100", "150", "200+"),
                       limits = c(0, 200)) +
    labs(x = "Coverage", y = "RMSE") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30),
          legend.title = element_blank(),
          legend.text = element_text(size = 25),
          legend.position = "bottom",
          legend.margin = margin(r = 50),
          text = element_text(size = 40),
          plot.margin = margin(t = 15, l = 15, r = 30, b = 15)) +
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500",
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3",
                                  "unofficial" = "grey", "official" = "grey")) +
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 10, "cLSBP_no" = 7,
                                  "unofficial" = 17, "official" = 16)) +
    scale_size_manual(values = c("cLSBP_BART" = 7, "cLSBP_CBPS" = 7, "cLSBP_GBM" = 7, "cLSBP_LASSO" = 7, "cLSBP_no" = 7,
                                 "unofficial" = 5, "official" = 5)) +
    guides(col = guide_legend(nrow = 2, byrow = TRUE),
           shape = guide_legend(nrow = 2, byrow = TRUE))

  output_base_figure5a <- "results/ACIC_2022/240904/UTE_RMSE_coverage_v3"

  cairo_ps(paste0(output_base_figure5a, ".eps"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure5a)
  dev.off()

  cairo_pdf(paste0(output_base_figure5a, ".pdf"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure5a)
  dev.off()
}
# Length vs coverage v3(500+) (Figure_5b)
{
  p_figure5b <- UTE_results_total %>% mutate(
    model = case_when(
      str_detect(Submission, "cLSBP") ~ Submission,
      str_detect(Submission, "\\*") ~ "unofficial",
      TRUE ~ "official"
    ),
    length = ifelse(length < 500, length, 500)
  ) %>% ggplot(aes(x = coverage, y = length)) +
    geom_point(
      data = . %>% filter(!str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_point(
      data = . %>% filter(str_detect(model, "cLSBP")),
      aes(col = model, shape = model, size = model)
    ) +
    geom_vline(xintercept = 0.9, linetype = "dashed", color = "black") +
    xlim(c(0, 1)) +
    scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500),
                       labels = c("0", "100", "200", "300", "400", "500+"),
                       limits = c(0, 500)) +
    labs(x = "Coverage", y = "Length") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30),
          legend.title = element_blank(),
          legend.text = element_text(size = 25),
          legend.position = "bottom",
          legend.margin = margin(r = 50),
          text = element_text(size = 40),
          plot.margin = margin(t = 15, l = 15, r = 30, b = 15)) +
    scale_color_manual(values = c("cLSBP_BART" = "#F8766D",
                                  "cLSBP_CBPS" = "#A3A500",
                                  "cLSBP_GBM" = "#00BF7D",
                                  "cLSBP_LASSO" = "#00B0F6",
                                  "cLSBP_no" = "#E76BF3",
                                  "unofficial" = "grey", "official" = "grey")) +
    scale_shape_manual(values = c("cLSBP_BART" = 16, "cLSBP_CBPS" = 17, "cLSBP_GBM" = 15, "cLSBP_LASSO" = 10, "cLSBP_no" = 7,
                                  "unofficial" = 17, "official" = 16)) +
    scale_size_manual(values = c("cLSBP_BART" = 7, "cLSBP_CBPS" = 7, "cLSBP_GBM" = 7, "cLSBP_LASSO" = 7, "cLSBP_no" = 7,
                                 "unofficial" = 5, "official" = 5)) +
    guides(col = guide_legend(nrow = 2, byrow = TRUE),
           shape = guide_legend(nrow = 2, byrow = TRUE))

  output_base_figure5b <- "results/ACIC_2022/240904/UTE_length_coverage_v3"

  cairo_ps(paste0(output_base_figure5b, ".eps"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure5b)
  dev.off()

  cairo_pdf(paste0(output_base_figure5b, ".pdf"), width = 11.67, height = 8.33, family = "Times")
  print(p_figure5b)
  dev.off()
}
