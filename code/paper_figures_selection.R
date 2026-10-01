# Bias and coverage under informative sampling: sample mean versus PPI.
# 1. Load informative-sampling results at the 5%, 10%, 20%, and 30% budgets.
# 2. Average bias and coverage across replications for sample mean and PPI.
# 3. Plot against zero bias and 95% coverage, and save the figure.
# Run from code/. Writes ../paper/figures/informative_prism.pdf.

library(dplyr)
library(ggplot2)
rd <- function(f) readRDS(file.path("../data", f))

# ---- 1. Load informative-sampling results at the 5%, 10%, 20%, and 30% budgets. ----
fr <- c(5, 10, 20, 30)
inf <- lapply(fr, function(p) rd(sprintf("paper_informative_%02d.rds", p)))
names(inf) <- as.character(fr)
# ---- 2. Average bias and coverage across replications for sample mean and PPI. ----
mean_metric_by_budget <- function(metric, arm, tg) {
  M <- sapply(inf, function(o) {
    means <- o$metrics |>
      filter(.data$arm == .env$arm, .data$metric == .env$metric) |>
      group_by(estimator) |>
      summarise(value = mean(value), .groups = "drop") |>
      pull(value, name = estimator)
    means[tg]
  })
  matrix(M, nrow = length(tg), dimnames = list(names(tg), names(inf)))
}

tagsC <- c(`sample mean` = "direct", `PPI` = "ppi_unweighted")
datD <- lapply(c("bias", "coverage"), function(met)
  data.frame(
    frac = rep(fr, each = length(tagsC)), estimator = names(tagsC),
    value = as.vector(mean_metric_by_budget(met, "informative", tagsC)), metric = met
  )) |>
  bind_rows()
PLAB <- c(bias = "Bias", coverage = "Coverage")
datD <- datD |>
  mutate(
    metric = factor(PLAB[metric], PLAB),
    estimator = factor(estimator, names(tagsC))
  )
# ---- 3. Plot against zero bias and 95% coverage, and save the figure. ----
hrefD <- data.frame(metric = factor(PLAB, PLAB), y = c(0, 0.95))
p <- ggplot(datD, aes(frac, value, colour = estimator, shape = estimator)) +
  geom_hline(data = hrefD, aes(yintercept = y), linetype = 3, colour = "grey45",
             linewidth = 0.45) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~metric, scales = "free_y") +
  scale_x_continuous(breaks = fr) +
  scale_y_continuous(expand = expansion(mult = 0.15)) +
  scale_colour_manual(values = c("#0077BB", "#CC3311")) +
  scale_shape_manual(values = c(16, 15)) +
  labs(x = "sampling budget (%)", y = NULL, colour = NULL, shape = NULL) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "right",
        legend.key.spacing.y = unit(0.4, "lines"), legend.margin = margin(l = 0),
        strip.background = element_blank())
ggsave("../paper/figures/informative_prism.pdf", p, width = 6.5, height = 2.4)
