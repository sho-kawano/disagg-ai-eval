# Benchmark RMSE waterfall from HT through taxonomy smoothing.
# 1. Load cached benchmark results.
# 2. Summarize RMSE at the 10% budget and draw the waterfall panels.
# 3. Save the benchmark waterfall figure.
# Run from code/. Writes ../paper/figures/bench_waterfall.pdf.
# Values use absolute RMSE, matching the benchmark tables.

library(dplyr)
library(ggplot2)
rd <- function(f) readRDS(file.path("../data", f))

# ---- 1. Load cached benchmark results. ----
arms <- c("weak", "math", "strong"); sfr <- c("10", "20", "30")
sm <- list()
for (a in arms) for (p in sfr) sm[[paste(a, p)]] <- rd(sprintf("paper_smoothing_%s_%s.rds", p, a))

FRAC <- "10"

# ---- 2. Summarize RMSE at the 10% budget and draw the waterfall panels. ----
STEPS <- c(
  HT = "direct", `FH, intercept only` = "fh_intercept",
  `+ GREG input` = "pp_s_intercept",
  `+ covariate` = "pp_s_judge",
  `+ taxonomy` = "ts_greg_judge"
)
WARM_LAB <- c(math = "specialist model", weak = "generalist model",
              strong = "historical difficulty")
wat <- lapply(c("math", "weak", "strong"), function(a) {
  o <- sm[[paste(a, FRAC)]]
  mean_rmse <- o$metrics |>
    filter(metric == "rmse") |>
    group_by(estimator) |>
    summarise(value = mean(value), .groups = "drop") |>
    pull(value, name = estimator)
  v <- mean_rmse[STEPS]
  data.frame(
    arm = sprintf("%s (r = %.2f)", WARM_LAB[[a]], o$judge_corr),
    step = factor(names(STEPS), names(STEPS)),
    ratio = as.numeric(v)
  )
}) |>
  bind_rows() |>
  mutate(arm = factor(arm, unique(arm))) |>
  group_by(arm) |>
  mutate(prev = lag(ratio), x = row_number()) |>
  ungroup()
pw <- ggplot(wat, aes(x, ratio)) +
  geom_rect(
    data = filter(wat, !is.na(prev)),
    aes(xmin = x - 0.3, xmax = x + 0.3, ymin = prev, ymax = ratio),
    fill = "grey85", colour = "grey60", linewidth = 0.2, inherit.aes = FALSE
  ) +
  geom_segment(
    data = filter(wat, !is.na(prev)),
    aes(x = x - 0.3, xend = x + 0.3, y = ratio, yend = ratio),
    colour = "#0072B2", linewidth = 0.7, inherit.aes = FALSE
  ) +
  geom_point(data = filter(wat, is.na(prev)), colour = "#0072B2", size = 2.2) +
  geom_text(aes(label = sprintf("%.3f", ratio)), vjust = -0.8, size = 2.8) +
  facet_wrap(~arm) +
  scale_x_continuous(breaks = seq_along(STEPS), labels = names(STEPS),
                     expand = expansion(mult = 0.12)) +
  scale_y_continuous(expand = expansion(mult = 0.12)) +
  labs(x = NULL, y = "RMSE") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        panel.grid.minor = element_blank())
# ---- 3. Save the benchmark waterfall figure. ----
ggsave("../paper/figures/bench_waterfall.pdf", pw, width = 6.5, height = 3.3)
cat("written\n")
