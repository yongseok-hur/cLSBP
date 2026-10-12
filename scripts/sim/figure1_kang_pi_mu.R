# Figure 1: true propensity score vs. prognostic function in Simulation 1 (seed 1, t = -2,...,2).
# Requires data/Kang_v4/d0001_*.csv (scripts/sim/01_generate_data.R 1 1).
suppressMessages({ library(dplyr); library(ggplot2); library(latex2exp) })
dir.create("figures/paper_final", recursive = TRUE, showWarnings = FALSE)

data_seed1 <- rbind %>% do.call(
  c("-0264", "-0132", "00", "0132", "0264") %>% lapply(function(tar_sel){
    data_tarsel <- read.csv(paste0("data/Kang_v4/d0001_tarsel", tar_sel, "_500.csv"))
    data_tarsel %>% mutate(tar_sel = tar_sel %>% as.numeric / 1000)
  })
)

output_base_figure1 <- "figures/paper_final/Kang_mu_pi"

p_figure1 <- data_seed1 %>%
  mutate(tar_sel = factor(paste0("t = ", as.character(tar_sel/0.132)), levels = c("t = -2", "t = -1", "t = 0", "t = 1", "t = 2"))) %>%
  ggplot(aes(x = mu, y = pi)) + geom_point(size = 1) +
  facet_wrap( ~ tar_sel, nrow = 1)+
  labs(
    x = TeX("$mu$"),
    y = TeX("$pi$")
  )+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    text = element_text(size = 25)
  )

# PNG

png("figures/paper_final/Kang_mu_pi.png", 1080, 360)
print(p_figure1)
dev.off()

# EPS
cairo_ps(paste0(output_base_figure1, ".eps"), width = 15, height = 5, family = "Times")
print(p_figure1)
dev.off()

# PDF
cairo_pdf(paste0(output_base_figure1, ".pdf"), width = 15, height = 5, family = "Times")
print(p_figure1)
dev.off()
