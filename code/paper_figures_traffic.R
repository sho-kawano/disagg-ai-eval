# Single-sample PRISM estimates and interval widths.
# 1. Load cached fits, the selected candidate, and plot labels.
# 2. Order domains by HT standard error and plot the selected estimates.
# 3. Compare interval widths for four candidates.
# 4. Combine the panels and save the traffic figure.
# Run from code/. Writes ../paper/figures/traffic_single_sample.pdf.

library(dplyr)
library(ggplot2)
library(patchwork)
rd <- function(f) readRDS(file.path("../data", f))

# ---- 1. Load cached fits, the selected candidate, and plot labels. ----
ss <- rd("paper_single_sample_10.rds")
e <- ss$est
sel <- ss$selected
TAG <- c(direct = "HT", greg_judge = "GREG (judge)",
         greg_full = "GREG (judge + content)", pps_judge = "PP-S (judge)",
         pps_full = "PP-S (judge + content)", ppts_judge = "PP-TS (judge)",
         ppts_full = "PP-TS (judge + content)")
COLS <- c("grey45", "grey15", "#D55E00", "#009E73", "#0072B2", "#E69F00", "#CC79A7")
names(COLS) <- TAG
w_ <- function(m) e[[paste0(m, "_hi")]] - e[[paste0(m, "_lo")]]

# ---- 2. Order domains by HT standard error and plot the selected estimates. ----
e <- e |>
  mutate(se_dir = (direct_hi - direct_lo) / (2 * 1.96)) |>
  arrange(se_dir) |>
  mutate(rank = row_number())
XLAB <- "domain, ordered by the HT standard error"
TH <- theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
p1 <- ggplot(e, aes(rank, .data[[sel]])) +
  geom_hline(yintercept = ss$gm, linetype = 2, colour = "grey60") +
  geom_linerange(aes(ymin = .data[[paste0(sel, "_lo")]],
                     ymax = .data[[paste0(sel, "_hi")]]),
                 colour = COLS[[TAG[[sel]]]], alpha = 0.55) +
  geom_point(size = 0.9, colour = COLS[[TAG[[sel]]]]) +
  labs(x = XLAB, y = "mean satisfaction",
       subtitle = sprintf("selected: %s", TAG[[sel]])) +
  TH

# ---- 3. Compare interval widths for four candidates. ----
SHOW <- c("direct", "greg_judge", "greg_full", "pps_full")
d <- lapply(SHOW, function(m)
  data.frame(rank = e$rank, w = w_(m), kind = TAG[[m]])) |>
  bind_rows() |>
  mutate(kind = factor(kind, TAG[SHOW]))
p2 <- ggplot(d, aes(rank, w, colour = kind)) +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(values = COLS[TAG[SHOW]]) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = XLAB, y = "95% interval width", colour = NULL,
       subtitle = "interval width, four of the candidates") +
  guides(colour = guide_legend(nrow = 1)) +
  TH

# ---- 4. Combine the panels and save the traffic figure. ----
# one shared x title and one single-row legend, both centred under the pair
ggsave("../paper/figures/traffic_single_sample.pdf",
       p2 + p1 + plot_layout(guides = "collect", axis_titles = "collect_x") &
         theme(legend.position = "bottom",
               legend.box.spacing = unit(4, "pt")),
       width = 6.5, height = 2.7)
cat("written\n")
