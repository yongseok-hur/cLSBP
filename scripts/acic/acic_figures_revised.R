## ============================================================
## 2022 ACIC data challenge (Section 4.1): Figures 2-5 as published.
## The summaries are those of acic_score_figures.R; this script only draws them.
## Usage: Rscript scripts/acic/acic_figures_revised.R   (output: figures/Figure_2a.pdf ... figures/Figure_5b.pdf)
## Inputs (not included, except the last one):
##   results/ACIC_2022/<date>/<scenario>/LSBP_1_mixture/*_answers.rds   Figures 2-4 (summarised once into
##     results/ACIC_2022/<date>/scoreboard.Rdata); <date> is the output folder set in acic_run.R
##   results/ACIC_2022/all_dataset_subgroup_size.rds                      Figure 4
##   results/ACIC_2022/<date>/UTE_scoreboard_cLSBP_overall.rds, UTE_scoreboard_others_overall.rds   Figure 5
##   ACIC_results (participants' results, loaded as in acic_score_figures.R)   Figure 3; if it is not loaded,
##     results/acic_participants_satt_figure3.csv is used: the participants' points read from the published Figure 3.
## Needs ggplot2, dplyr, tidyr, stringr, scales and ggrepel.
## ============================================================
suppressMessages({ library(dplyr); library(tidyr); library(stringr); library(ggplot2); library(ggrepel) })
dir.create("figures", showWarnings = FALSE)
FONT <- if (exists("FONT")) FONT else "Times"
RUN <- "251203"                                   # output folder of acic_run.R
dir_run <- file.path("results/ACIC_2022", RUN)

lev   <- c("cLSBP-BART", "cLSBP-CBPS", "cLSBP-GBM", "cLSBP-LASSO", "cLSBP-no")
fills <- c("cLSBP-BART" = "#D55E00", "cLSBP-CBPS" = "#E69F00", "cLSBP-GBM" = "#009E73", "cLSBP-LASSO" = "#0072B2", "cLSBP-no" = "#CC79A7")
shps  <- c("cLSBP-BART" = 21, "cLSBP-CBPS" = 24, "cLSBP-GBM" = 22, "cLSBP-LASSO" = 23, "cLSBP-no" = 25)
lab_model <- function(x) {                       # "BART" / "no plugin" / "cLSBP_BART" / "cLSBP_no" -> "cLSBP-BART" / "cLSBP-no"
  x <- str_remove(str_remove(x, "^cLSBP_"), " plugin$")
  factor(paste0("cLSBP-", x), levels = lev)
}
save_pdf <- function(p, file, w, h) { cairo_pdf(file.path("figures", file), width = w, height = h, family = FONT); print(p); invisible(dev.off()) }
theme_fig <- function(base) theme_bw(base_size = base, base_family = FONT) +
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), strip.background = element_rect(fill = "grey92"),
        legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(base / 3, "pt"))

## ---- Figures 2(a)-(c): SATT RMSE, coverage and length of cLSBP over the 16 confounded scenarios ----
sb_file <- file.path(dir_run, "scoreboard.Rdata")
if (!file.exists(sb_file)) {                      # per-dataset answers -> scoreboard_final_new (as in acic_score_figures.R)
  scenarios <- c("None_Large", "Strong_Large", "Strong_Small", "Weak_Large", "Weak_Small")
  ps_estimator <- c("13_", "13_CBPS_", "13_BART_", "13_LASSO_", "13_GBM_")
  scoreboard_final_new <- do.call(rbind, lapply(scenarios, function(sc) do.call(rbind, lapply(ps_estimator, function(ps)
    do.call(rbind, lapply(list.files(file.path(dir_run, sc, "LSBP_1_mixture"), pattern = paste0("test_.*", ps, "answers\\.rds"),
                                     full.names = TRUE), readRDS))))))
  scoreboard_final_new$Scenario <- factor(scoreboard_final_new$Scenario, levels = unique(scoreboard_final_new$Scenario),
    labels = c("N/N/L/S", "S/A/L/L", "S/A/L/S", "S/B/L/L", "S/B/L/S", "S/B/S/L", "S/A/S/S", "S/A/S/L", "S/B/S/S",
               "W/B/L/L", "W/A/L/S", "W/A/L/L", "W/B/L/S", "W/A/S/S", "W/B/S/L", "W/B/S/S", "W/A/S/L"))
  scoreboard_final_new <- scoreboard_final_new %>%
    separate(Scenario, into = c("Confounding.Strength", "Confounding.Source", "Impact.Heterogeneity", "Idiosyncrasy.of.Impacts"), sep = "/") %>%
    mutate(Confounding.Strength = recode(Confounding.Strength, N = "None", S = "Strong", W = "Weak"),
           Confounding.Source = recode(Confounding.Source, N = "None", A = "Source A", B = "Source B"),
           Impact.Heterogeneity = recode(Impact.Heterogeneity, L = "Large", S = "Small"),
           Idiosyncrasy.of.Impacts = recode(Idiosyncrasy.of.Impacts, L = "Large", S = "Small"),
           plugin = ifelse(is.na(plugin), "no plugin", plugin))
  save(scoreboard_final_new, file = sb_file)
}
load(sb_file)                                     # scoreboard_final_new
fp <- scoreboard_final_new %>%
  filter(variable == "Overall", Confounding.Strength != "None") %>%
  mutate(Confounding.Strength = factor(Confounding.Strength, levels = c("Weak", "Strong")),
         Impact.Heterogeneity = factor(Impact.Heterogeneity, levels = c("Small", "Large")),
         Combined = paste(str_remove(Confounding.Source, "Source "), str_sub(Idiosyncrasy.of.Impacts, 1, 1), sep = " / "),
         model = lab_model(plugin)) %>%
  group_by(Confounding.Strength, Impact.Heterogeneity, Combined, plugin, model) %>%
  summarise(RMSE = sqrt(mean(bias^2)), coverage = mean(covered), length = mean(`95%` - `5%`), .groups = "drop") %>%
  arrange(Confounding.Strength, Impact.Heterogeneity) %>%
  mutate(panel = paste0(Confounding.Strength, " confounding\n", tolower(Impact.Heterogeneity), " heterogeneity"),
         panel = factor(panel, levels = unique(panel)))

fig2 <- function(yvar, ylab, legend = FALSE, ylim = NULL, hline = NULL) {
  p <- ggplot(fp, aes(x = Combined, y = .data[[yvar]], fill = model, shape = model)) +
    { if (!is.null(hline)) geom_hline(yintercept = hline, linetype = "dashed", colour = "grey30") } +
    geom_point(size = 4.2, stroke = 0.6, colour = "grey15", position = position_dodge(width = 0.72)) +
    facet_wrap(~ panel, nrow = 1) +
    scale_fill_manual(values = fills, drop = FALSE) +
    scale_shape_manual(values = shps, drop = FALSE) +
    labs(x = "Source of confounding / source of heterogeneity", y = ylab) +
    theme_fig(22) +
    theme(axis.text = element_text(size = 17), strip.text = element_text(size = 17), legend.text = element_text(size = 20),
          legend.position = if (legend) "bottom" else "none")
  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
  p
}
save_pdf(fig2("RMSE", "RMSE", ylim = c(10, 50)),                   "Figure_2a.pdf", 15, 4)
save_pdf(fig2("coverage", "Coverage", ylim = c(0, 1), hline = 0.9), "Figure_2b.pdf", 15, 4)
save_pdf(fig2("length", "Length", legend = TRUE),                   "Figure_2c.pdf", 15, 4.5)

## ---- Figures 3 and 5: cLSBP versus the challenge participants ----
## cl: cLSBP summaries (Submission, coverage, value); pa: participants (group = Official/Unofficial, coverage, value).
## inset = list(x = zoom range of coverage, y = zoom range of the metric, at = c(xmin, xmax, ymin, ymax) of the inset
##              in data units). Place the inset where no point lies; the values below fit the published results.
vs_participants <- function(cl, pa, ylab, cap, step, lab_band = NULL, inset = NULL) {
  cl <- cl %>% mutate(y = pmin(value, cap), group = lab_model(Submission), lab = str_remove(group, "cLSBP-"))
  pa <- pa %>% mutate(y = pmin(value, cap))
  cl_layer <- function(d, s) geom_point(data = d, aes(coverage, y, fill = group), shape = shps[as.character(d$group)],
                                        colour = "black", size = s, stroke = 1.3, inherit.aes = FALSE)
  pa_layer <- function(d) geom_point(data = d, aes(coverage, y, shape = group), colour = "grey50", size = 4.5, stroke = 1.3, inherit.aes = FALSE)
  labs_layer <- function(d, s, obst, band = c(NA, NA)) geom_text_repel(data = bind_rows(d, mutate(obst, lab = "")), aes(coverage, y, label = lab),
                                                     size = s, family = FONT, seed = 1, box.padding = 0.6, point.padding = 0.6, ylim = band,
                                                     direction = if (all(is.na(band))) "both" else "x", min.segment.length = 0,
                                                     segment.size = 0.7, segment.colour = "grey25", max.overlaps = Inf, inherit.aes = FALSE)
  p <- ggplot() +
    geom_vline(xintercept = 0.9, linetype = "dashed", colour = "grey35", linewidth = 0.8) +
    pa_layer(pa) + cl_layer(cl, if (is.null(inset)) 9 else 6) +
    scale_shape_manual(values = c(Official = 1, Unofficial = 2), labels = c(Official = "Challenge entry (official)", Unofficial = "Challenge entry (unofficial)")) +
    scale_fill_manual(values = fills, guide = "none") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(breaks = seq(0, cap, step), labels = c(seq(0, cap - step, step), paste0(cap, "+")), limits = c(0, cap * 1.02)) +
    labs(x = "Coverage", y = ylab) +
    theme_fig(40) +
    theme(axis.text = element_text(size = 30), legend.text = element_text(size = 28), legend.position = "bottom",
          plot.margin = margin(t = 15, l = 15, r = 30, b = 15))
  if (is.null(inset)) return(p + labs_layer(cl, 10, pa, if (is.null(lab_band)) c(NA, NA) else lab_band))
  zx <- inset$x; zy <- inset$y
  pa_in <- filter(pa, between(coverage, zx[1], zx[2]), between(y, zy[1], zy[2]))
  # labels beside the markers: left of the cluster's median coverage on the left, the rest on the right
  side_lab <- cl %>% mutate(left = coverage <= median(coverage), hj = ifelse(left, 1, 0),
                            nudge = ifelse(left, -1, 1) * 0.06 * diff(zx))
  pin <- ggplot() +
    geom_vline(xintercept = 0.9, linetype = "dashed", colour = "grey35", linewidth = 0.8) +
    pa_layer(pa_in) + cl_layer(cl, 7.5) +
    geom_text(data = side_lab, aes(coverage + nudge, y, label = lab, hjust = hj), size = 9, family = FONT, inherit.aes = FALSE) +
    scale_shape_manual(values = c(Official = 1, Unofficial = 2), guide = "none") +
    scale_fill_manual(values = fills, guide = "none") +
    coord_cartesian(xlim = zx, ylim = zy, expand = FALSE) + scale_x_continuous(breaks = scales::breaks_pretty(3)) +
    theme_fig(22) + theme(axis.title = element_blank(), axis.text = element_text(size = 22), axis.ticks.length = unit(2, "pt"),
                          plot.margin = margin(4, 8, 2, 2), plot.background = element_rect(colour = "grey30", linewidth = 1))
  p + annotate("rect", xmin = zx[1], xmax = zx[2], ymin = zy[1], ymax = zy[2], fill = NA, colour = "grey20", linetype = "dashed", linewidth = 0.9) +
    annotation_custom(ggplotGrob(pin), xmin = inset$at[1], xmax = inset$at[2], ymin = inset$at[3], ymax = inset$at[4])
}

satt_cl <- fp %>% group_by(plugin) %>%
  summarise(RMSE = sqrt(mean(RMSE^2)), coverage = mean(coverage), length = mean(length), .groups = "drop") %>%
  mutate(Submission = paste0("cLSBP_", ifelse(plugin == "no plugin", "no", plugin)))
if (exists("ACIC_results")) {
  satt_pa <- ACIC_results %>% filter(Confounding.Strength != "None", Confounding.Strength != "All", variable == "Overall") %>%
    group_by(Submission) %>% summarise(RMSE = sqrt(mean(rmse^2)), coverage = mean(cover), length = mean(width), .groups = "drop") %>%
    mutate(group = ifelse(str_detect(Submission, "\\*"), "Unofficial", "Official")) %>%
    pivot_longer(c(RMSE, length), names_to = "metric", values_to = "value")
} else {
  satt_pa <- read.csv("results/acic_participants_satt_figure3.csv")
}
save_pdf(vs_participants(satt_cl %>% mutate(value = RMSE), filter(satt_pa, metric == "RMSE"), "RMSE", 100, 20, lab_band = c(52, 66)),
         "Figure_3a.pdf", 11.67, 8.33)
save_pdf(vs_participants(satt_cl %>% mutate(value = length), filter(satt_pa, metric == "length"), "Length", 100, 20, lab_band = c(80, 94)),
         "Figure_3b.pdf", 11.67, 8.33)

ute_cl <- readRDS(file.path(dir_run, "UTE_scoreboard_cLSBP_overall.rds"))
ute_pa <- readRDS(file.path(dir_run, "UTE_scoreboard_others_overall.rds")) %>%
  mutate(group = ifelse(str_detect(Submission, "\\*"), "Unofficial", "Official"))
zoom_box <- function(v, pad) { r <- range(v); r + pad * max(diff(r), 1e-8) }   # pad = c(left/below, right/above), in units of the range
save_pdf(vs_participants(ute_cl %>% mutate(value = RMSE), ute_pa %>% mutate(value = RMSE), "RMSE", 200, 50,
                         inset = list(x = zoom_box(ute_cl$coverage, c(-0.45, 0.7)), y = zoom_box(ute_cl$RMSE, c(-0.3, 0.3)), at = c(0.215, 0.715, 106, 207))),
         "Figure_5a.pdf", 11.67, 8.33)
save_pdf(vs_participants(ute_cl %>% mutate(value = length), ute_pa %>% mutate(value = length), "Length", 500, 100,
                         inset = list(x = zoom_box(ute_cl$coverage, c(-0.45, 0.7)), y = zoom_box(ute_cl$length, c(-0.3, 0.3)), at = c(0.0, 0.69, 195, 507))),
         "Figure_5b.pdf", 11.67, 8.33)

## ---- Figure 4: DRMSE of the 12 subgroup CATTs against subgroup size ----
dr <- scoreboard_final_new %>%
  group_by(dataset.num, plugin) %>%
  mutate(differential_error = ifelse(variable == "Overall", 0, bias - bias[variable == "Overall"])) %>%
  ungroup() %>%
  group_by(variable, level, plugin) %>%
  summarise(DRMSE = sqrt(mean(differential_error^2)), .groups = "drop") %>%
  filter(!is.na(level))
ss <- readRDS("results/ACIC_2022/all_dataset_subgroup_size.rds")
dr$sample_size <- rep(colMeans(ss), each = 5) / 1000          # same alignment as acic_score_figures.R
dr$model <- lab_model(dr$plugin)
dr$x <- dr$sample_size + c(-2, -1, 0, 1, 2)[as.integer(dr$model)] * 1.2   # side by side within each subgroup
p4 <- ggplot(dr, aes(x = x, y = DRMSE, fill = model, shape = model)) +
  geom_point(size = 4.2, stroke = 0.6, colour = "grey15") +
  scale_fill_manual(values = fills) + scale_shape_manual(values = shps) +
  coord_cartesian(xlim = c(0, 205), ylim = c(0, 30)) +
  labs(x = "Subgroup size (thousands)", y = "DRMSE") +
  theme_fig(24) +
  theme(axis.text = element_text(size = 20), legend.text = element_text(size = 22), legend.position = "bottom")
save_pdf(p4, "Figure_4.pdf", 15, 6)
cat("written: figures/Figure_2a-c.pdf, Figure_3a-b.pdf, Figure_4.pdf, Figure_5a-b.pdf\n")
