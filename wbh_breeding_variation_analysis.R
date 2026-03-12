# ==============================================================================
# Script: breeding_analysis_2016_2025.R
# Description: Analysis of breeding phenology and performance metrics.
# Outputs: Monthly/Annual performance figures and summary tables.
# ==============================================================================

# 1. SETUP AND LIBRARIES -------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse) 
  library(lubridate) 
  library(patchwork) 
  library(gt)        
})

# Global Constants
INPUT_FILE  <- "wbh_2016_to_2025_breedingdata.csv"
OUTPUT_DIR  <- "plots_maps"
RESOLUTION  <- 1200
FIG_WIDTH   <- 7    # inches
THEME_SIZE  <- 11   # base font size
LEGEND_SIZE <- 8    # legend font size

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# 2. DATA LOADING & PREPROCESSING ---------------------------------------------
breeding_data <- read_csv(INPUT_FILE, show_col_types = FALSE) %>%
  mutate(
    location = str_squish(as.character(location)),
    outcome  = factor(str_to_title(str_squish(outcome)), levels = c("Success", "Fail")),
    date     = mdy(first_egg_day),
    year     = year(date),
    julian   = yday(date),
    month    = factor(month(date, label = TRUE, abbr = FALSE), 
                      levels = c("February", "March", "April", "May", "June")),
    success  = outcome == "Success"
  ) %>%
  filter(!is.na(date))

# 3. PERFORMANCE SUMMARIES (TRANSPOSED COMPARISON) -----------------------------

# Calculate Overall metrics
perf_overall <- breeding_data %>%
  summarise(
    n            = n(),
    success_rate = (sum(success, na.rm = TRUE) / n) * 100,
    across(c(clutch_size, no_hatchling, no_fledgling), 
           list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  ) %>%
  mutate(group = "Overall")

# Calculate specific Clutch (1 & 2) metrics
perf_clutch <- breeding_data %>%
  filter(clutch_no %in% c(1, 2)) %>%
  group_by(clutch_no) %>%
  summarise(
    n            = n(),
    success_rate = (sum(success, na.rm = TRUE) / n) * 100,
    across(c(clutch_size, no_hatchling, no_fledgling), 
           list(median = ~median(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE)),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  ) %>%
  mutate(group = paste("Clutch", clutch_no)) %>%
  select(-clutch_no)

# Combine, Transpose (Metrics as rows), and Export
summary_comparison <- bind_rows(perf_overall, perf_clutch) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(-group, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = group, values_from = Value) %>%
  mutate(Metric = str_to_title(str_replace_all(Metric, "_", " ")))

write_csv(summary_comparison, file.path(OUTPUT_DIR, "performance_comparison_table.csv"))

# 4. FIGURE 01: MONTHLY PERFORMANCE -------------------------------------------
month_stats <- breeding_data %>%
  group_by(month) %>%
  summarise(
    n           = n(),
    pct_success = 100 * sum(success, na.rm = TRUE) / n,
    clutch_mean = mean(clutch_size, na.rm = TRUE), clutch_sd = sd(clutch_size, na.rm = TRUE),
    hatch_mean  = mean(no_hatchling, na.rm = TRUE),  hatch_sd = sd(no_hatchling, na.rm = TRUE),
    fledg_mean  = mean(no_fledgling, na.rm = TRUE),  fledg_sd = sd(no_fledgling, na.rm = TRUE),
    .groups     = "drop"
  )

# Panel A: Success Rate Bars
p1a <- ggplot(month_stats, aes(x = month, y = pct_success)) +
  geom_col(fill = "skyblue2", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", pct_success, n)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 75, by = 15)) +
  labs(title = "(A)", x = NULL, y = "% Successful Nests") +
  theme_grey(base_size = THEME_SIZE) + 
  theme(panel.grid = element_line(color = "white"))

# Panel B: Productivity Line Plot
p1b_data <- month_stats %>%
  select(month, contains("mean"), contains("sd")) %>%
  pivot_longer(-month, names_to = c("metric", ".value"), names_pattern = "(clutch|hatch|fledg)_(mean|sd)") %>%
  mutate(metric = factor(recode(metric, clutch = "Clutch size", hatch = "Hatchlings", fledg = "Fledglings"),
                         levels = c("Clutch size", "Hatchlings", "Fledglings")))

p1b <- ggplot(p1b_data, aes(x = month, y = mean, group = metric)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, linetype = metric), width = 0.1, linewidth = 0.2) +
  geom_line(aes(linetype = metric), linewidth = 0.5) +
  geom_point(aes(shape = metric), size = 2.2) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  labs(title = "(B)", x = NULL, y = "Mean (±SD)", linetype = NULL, shape = NULL) +
  theme_grey(base_size = THEME_SIZE) + 
  theme(
    panel.grid = element_line(color = "white"),
    legend.position   = c(0.85, 0.88),
    legend.text       = element_text(size = LEGEND_SIZE),
    legend.key.size   = unit(3, "mm"),
    legend.background = element_blank()
  )

ggsave(file.path(OUTPUT_DIR, "Fig_01_Breeding_performance.png"), 
       p1a + p1b, width = FIG_WIDTH, units = "in", dpi = RESOLUTION)

# 5. FIGURE 02: NEST OUTCOMES BY LOCATION --------------------------------------
fig_02_data <- breeding_data %>%
  count(location, outcome) %>%
  complete(location, outcome, fill = list(n = 0)) %>%
  group_by(location) %>%
  mutate(total_n = sum(n)) %>%
  ungroup() %>%
  mutate(location = fct_reorder(location, total_n, .desc = TRUE))

fig_02 <- ggplot(fig_02_data, aes(x = location, y = n, fill = outcome)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.9) +
  scale_fill_manual(values = c("Success" = "skyblue2", "Fail" = "steelblue4")) +
  labs(x = "Nest Location", y = "No. Of Nest", fill = NULL) +
  theme_grey(base_size = THEME_SIZE) + 
  theme(
    panel.grid = element_line(color = "white"),
    legend.position   = c(0.85, 0.85),
    legend.text       = element_text(size = LEGEND_SIZE),
    legend.key.size   = unit(3, "mm"),
    legend.background = element_blank()
  )

ggsave(file.path(OUTPUT_DIR, "Fig_02_nest_outcomes.png"), 
       fig_02, width = FIG_WIDTH, units = "in", dpi = RESOLUTION)

# 6. FIGURE 03: ANNUAL VARIATION & ATTEMPT TYPE --------------------------------
outcome_colors <- c("Success" = "deepskyblue3", "Fail" = "#FF6103")

# Panel A: Boxplot by Year
p3a <- ggplot(breeding_data, aes(x = factor(year), y = julian)) +
  geom_boxplot(fill = "grey70", outlier.shape = NA, linewidth = 0.3) +
  geom_point(aes(color = outcome), position = position_jitter(width = 0.15), size = 2, alpha = 0.8) +
  scale_color_manual(values = outcome_colors) +
  labs(title = "(A)", x = "Year", y = "First Egg Laying Date (Julian day)", color = NULL) +
  theme_grey(base_size = THEME_SIZE) + 
  theme(
    panel.grid = element_line(color = "white"), 
    legend.position   = c(0.25, 0.85), 
    legend.text       = element_text(size = LEGEND_SIZE),
    legend.key.size   = unit(3, "mm"),
    legend.background = element_rect(fill = "white", color = "grey40")
  )

# Panel B: Boxplot by Attempt Type
p3b <- breeding_data %>%
  filter(!is.na(clutch_no)) %>%
  mutate(type = factor(ifelse(clutch_no == 1, "First nests", "Re-nests"), levels = c("First nests", "Re-nests"))) %>%
  ggplot(aes(x = type, y = julian)) +
  geom_boxplot(fill = "grey70", width = 0.6, linewidth = 0.3) +
  geom_point(aes(color = outcome), position = position_jitter(width = 0.12), size = 2) +
  scale_color_manual(values = outcome_colors) +
  labs(title = "(B)", x = NULL, y = NULL) +
  theme_grey(base_size = THEME_SIZE) + 
  theme(panel.grid = element_line(color = "white"), legend.position = "none")

ggsave(file.path(OUTPUT_DIR, "Fig_03_annual_variation.png"), 
       p3a + p3b + plot_layout(widths = c(2.8, 1.2)), width = FIG_WIDTH, units = "in", dpi = RESOLUTION)

# 7. TABLE 01: YEARLY SUMMARY TABLE --------------------------------------------
yearly_summary_gt <- breeding_data %>%
  group_by(year) %>%
  summarise(
    n        = n(),
    mean_sd  = sprintf("%.1f ± %.1f", mean(julian, na.rm = TRUE), sd(julian, na.rm = TRUE)),
    earliest = sprintf("%d (%s)", min(julian), format(date[which.min(julian)], "%b %d")),
    latest   = sprintf("%d (%s)", max(julian), format(date[which.max(julian)], "%b %d"))
  ) %>%
  gt() %>%
  tab_header(title = "Summary of First Egg Dates by Year (2016-2025)") %>%
  cols_label(
    year     = "Year", 
    n        = "Nests (n)", 
    mean_sd  = "Mean Julian Day ± SD", 
    earliest = "Earliest (Min)", 
    latest   = "Latest (Max)"
  ) %>%
  tab_options(table.font.size = 12, data_row.padding = px(4))

gtsave(yearly_summary_gt, file.path(OUTPUT_DIR, "Table_01_Yearly_Summary.png"))

message("Analysis complete. All publication-quality outputs saved to: ", normalizePath(OUTPUT_DIR))

