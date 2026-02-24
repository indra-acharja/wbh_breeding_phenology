# ============================================================
# Breeding phenology & performance figures and summary table
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(patchwork)
  library(gt)
})

# ----------------------------
# Paths
# ----------------------------
input_file <- "2016_to_2025_breedingdates.csv"
output_dir <- "figs_tables"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Export sizes (mm)
fig_width_double_mm <- 175

# ----------------------------
# Helpers
# ----------------------------
stop_if_missing <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

# Theme: theme_grey + WHITE grid lines + TRANSPARENT legend box
figure_theme <- function(base_size = 10) {
  theme_grey(base_size = base_size) +
    theme(
      panel.grid.major = element_line(colour = "white", linewidth = 0.2),
      panel.grid.minor = element_line(colour = "white", linewidth = 0.2),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key        = element_rect(fill = "transparent", colour = NA),
      panel.border      = element_blank()
    )
}

save_fig <- function(plot_obj, filename_base, width_mm, height_mm, dpi = 600) {
  pdf_path <- file.path(output_dir, paste0(filename_base, ".pdf"))
  png_path <- file.path(output_dir, paste0(filename_base, ".png"))
  ggsave(pdf_path, plot_obj, width = width_mm, height = height_mm, units = "mm", device = cairo_pdf)
  ggsave(png_path, plot_obj, width = width_mm, height = height_mm, units = "mm", dpi = dpi, type = "cairo-png")
  message("Saved: ", pdf_path)
  message("Saved: ", png_path)
}

# ----------------------------
# Load & prepare data
# ----------------------------
dat <- read_csv(input_file, show_col_types = FALSE)

required_cols <- c(
  "first_egg_day", "outcome", "location",
  "clutch_size", "no_hatchling", "no_fledgling"
)
stop_if_missing(dat, required_cols)

dat <- dat %>%
  mutate(
    location = str_squish(as.character(location)),
    outcome  = str_squish(as.character(outcome)),
    date     = mdy(first_egg_day),
    year     = year(date),
    julian   = yday(date),
    month    = month(date, label = TRUE, abbr = FALSE),
    success  = tolower(outcome) == "success"
  )

month_levels <- c("February", "March", "April", "May", "June")

br_perform_overall <- dat %>%
  summarise(
    mean_clutch = mean(clutch_size, na.rm = TRUE),
    sd_clutch = sd(clutch_size,na.rm = TRUE),
    mean_hatch = mean(no_hatchling, na.rm = TRUE),
    sd_hatch = sd(no_hatchling,na.rm = TRUE),
    mean_fledge = mean(no_fledgling,na.rm = TRUE),
    sd_fledge = sd(no_fledgling,na.rm = TRUE),
  )

br_perform_by_clutch_no1 <- dat %>% 
  filter(clutch_no == 1) %>%
    summarise(
      n = n(),
    success = sum(outcome == "Success", na.rm = TRUE),
    fail = sum(outcome == "Fail", na.rm = TRUE),
    success_rate = (success/n)*100,
    median_clutch = median(clutch_size, na.rm = TRUE),
    max_clutch = max(clutch_size, na.rm = TRUE),
    min_clutch = min(clutch_size, na.rm = TRUE),
    median_hatch = median(no_hatchling, na.rm = TRUE),
    max_hatch = max(no_hatchling, na.rm = TRUE),
    min_hatch = min(no_hatchling, na.rm = TRUE),
    median_fledge = median(no_fledgling, na.rm = TRUE),
    max_fledge = max(no_fledgling, na.rm = TRUE),
    min_fledge = min(no_fledgling, na.rm = TRUE),
  )
br_perform_by_clutch_no2 <- dat %>% 
  filter(clutch_no == 2) %>%
  summarise(
    n = n(),
    success = sum(outcome == "Success", na.rm = TRUE),
    fail = sum(outcome == "Fail", na.rm = TRUE),
    success_rate = (success/n)*100,
    median_clutch = median(clutch_size, na.rm = TRUE),
    max_clutch = max(clutch_size, na.rm = TRUE),
    min_clutch = min(clutch_size, na.rm = TRUE),
    median_hatch = median(no_hatchling, na.rm = TRUE),
    max_hatch = max(no_hatchling, na.rm = TRUE),
    min_hatch = min(no_hatchling, na.rm = TRUE),
    median_fledge = median(no_fledgling, na.rm = TRUE),
    max_fledge = max(no_fledgling, na.rm = TRUE),
    min_fledge = min(no_fledgling, na.rm = TRUE),
  )

print(br_perform_overall)
print(br_perform_by_clutch_no1)
print(br_perform_by_clutch_no2)


# ============================================================
# Figure 01: Monthly success (%) + monthly productivity (mean ± SD)
# ============================================================

# Panel A
month_success <- dat %>%
  mutate(month = factor(as.character(month), levels = month_levels)) %>%
  filter(month %in% month_levels) %>%
  group_by(month) %>%
  summarise(
    n = n(),
    n_success = sum(success, na.rm = TRUE),
    pct_success = 100 * n_success / n,
    .groups = "drop"
  ) %>%
  mutate(label = sprintf("%.1f%%\n(n=%d)", pct_success, n))

pA <- ggplot(month_success, aes(x = month, y = pct_success)) +
  geom_col(fill = "skyblue2", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = label), vjust = -0.5, size = 3.5) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = seq(0, 75, by = 15),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = NULL, y = "% Successful Nests") +
  ggtitle("(A)") +
  figure_theme(11)

# Panel B
month_means <- dat %>%
  mutate(month = factor(as.character(month), levels = month_levels)) %>%
  filter(month %in% month_levels) %>%
  group_by(month) %>%
  summarise(
    clutch_mean = mean(clutch_size, na.rm = TRUE),
    clutch_sd   = sd(clutch_size, na.rm = TRUE),
    hatch_mean  = mean(no_hatchling, na.rm = TRUE),
    hatch_sd    = sd(no_hatchling, na.rm = TRUE),
    fledg_mean  = mean(no_fledgling, na.rm = TRUE),
    fledg_sd    = sd(no_fledgling, na.rm = TRUE),
    .groups = "drop"
  )

month_long <- month_means %>%
  pivot_longer(
    cols = -month,   # keep month as ID column
    names_to = c("metric", ".value"),
    names_pattern = "(clutch|hatch|fledg)_(mean|sd)"
  ) %>%
  mutate(
    metric = recode(metric,
                    clutch = "Clutch size",
                    hatch  = "Hatchlings",
                    fledg  = "Fledglings"),
    metric = factor(metric, levels = c("Clutch size", "Hatchlings", "Fledglings"))
  )

pB <- ggplot(month_long, aes(x = month, y = mean, group = metric)) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd, linetype = metric),
    width = 0.1, linewidth = 0.2,
    show.legend = FALSE
  ) +
  geom_line(aes(linetype = metric), linewidth = 0.5, color = "black") +
  geom_point(aes(shape = metric), size = 2.2, color = "black") +
  scale_linetype_manual(values = c("Clutch size" = "solid",
                                   "Hatchlings"  = "dotted",
                                   "Fledglings"  = "dashed")) +
  scale_shape_manual(values = c("Clutch size" = 16, "Hatchlings" = 17, "Fledglings" = 15)) +
  labs(x = NULL, y = "Mean (±SD)", linetype = NULL, shape = NULL) +
  ggtitle("(B)") +
  figure_theme(11) +
  theme(
    legend.position = c(0.82, 0.92),
    legend.key.height = unit(3.2, "mm"),
    legend.key.width  = unit(4.0, "mm"),
    legend.spacing.y  = unit(4, "mm"),
    legend.text       = element_text(margin = margin(t = 0, b = 0)),
    legend.margin     = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0)
  ) +
  guides(
    linetype = guide_legend(byrow = TRUE,
                            keyheight = unit(4.2, "mm"),
                            keywidth  = unit(3.0, "mm"))
  )

fig_01 <- pA + pB + plot_layout(ncol = 2, widths = c(1, 1))
print(fig_01)

save_fig(fig_01, "Fig_01_Monthly_Success_and_Productivity", width_mm = fig_width_double_mm, height_mm = 90)

# ============================================================
# Figure 02: Success vs failure counts by nest location
# ============================================================

loc_counts <- dat %>%
  mutate(outcome = factor(outcome, levels = c("Success", "Fail"))) %>%
  count(location, outcome, name = "n") %>%
  complete(location, outcome, fill = list(n = 0))

# Order locations by TOTAL nests (Success + Fail), descending
loc_order <- loc_counts %>%
  group_by(location) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  arrange(desc(total_n)) %>%
  pull(location)

loc_counts <- loc_counts %>%
  mutate(location = factor(location, levels = loc_order))

fig_02 <- ggplot(loc_counts, aes(x = location, y = n, fill = outcome)) +
  geom_col(
    position = position_dodge2(width = 0.90, padding = 0),  # <- attached bars
    width = 0.90                                            # <- wide bars
  ) +
  labs(x = "Nest Location", y = "No. Of Nest", fill = NULL) +
  scale_fill_manual(values = c("Success" = "skyblue2", "Fail" = "steelblue4")) +
  figure_theme(11) +
  theme(legend.position = c(0.80, 0.85))
print(fig_02)

save_fig(fig_02, "Fig_02_Success_Failure_by_NestLocation", width_mm = fig_width_double_mm, height_mm = 75)

# ============================================================
# Figure 03A + 03B combined with panel labels (A) and (B)
# ============================================================

# Consistent colors
outcome_cols <- c("Success" = "deepskyblue3", "Fail" = "#FF6103")

# --- enforce factor levels (prevents manual-scale warnings) ---
dat <- dat %>%
  mutate(
    outcome = str_squish(as.character(outcome)),
    outcome = factor(outcome, levels = c("Success", "Fail"))
  )

# ----------------------------
# Figure 03A (Panel A): by year
# ----------------------------
fig_03A <- ggplot(dat, aes(x = factor(year), y = julian)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.3, fill = "grey70") +
  geom_point(
    aes(color = outcome),
    position = position_jitter(width = 0.10, height = 0),
    size = 2.0,
    alpha = 0.95
  ) +
  scale_color_manual(values = outcome_cols, drop = FALSE) +
  labs(
    x = "Year",
    y = "First Egg Laying Date (Julian day)",
    color = NULL
  ) +
  figure_theme(11) +
  ggtitle("(A)")+
  theme(
    legend.position = c(0.25, 0.85),
    legend.key.height = unit(2.2, "mm"),
    legend.spacing.y  = unit(0.6, "mm"),
    
    # white legend box
    legend.background = element_rect(fill = "white", colour = "grey40", linewidth = 0.3),
    legend.box.background = element_rect(fill = "white", colour = "grey40", linewidth = 0.3),
    
    # keep legend keys transparent (no grey tiles)
    legend.key = element_rect(fill = "transparent", colour = NA),
    
    legend.margin = margin(2, 2, 2, 2),
    legend.box.margin = margin(0, 0, 0, 0)
  )

# ----------------------------
# Figure 03B (Panel B): first attempt vs re-nest (clutch_no)
# ----------------------------
dat_attempt <- dat %>%
  mutate(
    attempt_type = case_when(
      is.na(clutch_no) ~ NA_character_,
      clutch_no == 1   ~ "First nests",
      clutch_no >= 2   ~ "Re-nests"
    ),
    attempt_type = factor(attempt_type, levels = c("First nests", "Re-nests"))
  ) %>%
  filter(!is.na(attempt_type), !is.na(outcome))

fig_03B <- ggplot(dat_attempt, aes(x = attempt_type, y = julian)) +
  geom_boxplot(
    outlier.shape = NA,
    linewidth = 0.3,
    fill = "grey70",
    color = "black",
    width = 0.55
  ) +
  geom_point(
    aes(color = outcome),
    position = position_jitter(width = 0.12, height = 0),
    size = 2.0,
    alpha = 0.95
  ) +
  scale_color_manual(values = outcome_cols, drop = FALSE) +
  labs(x = NULL, y = NULL, color = NULL) +
  figure_theme(11) +
  ggtitle("(B)")+
  theme(
    legend.position = "none"   # keep only one legend (from panel A)
  )

# ----------------------------
# Combined Figure 03 with panel labels
# ----------------------------
tight_panel_gap <- theme(panel.spacing = unit(1, "mm"))  # try 1–3 mm

fig_03 <- ((fig_03A + tight_panel_gap) + (fig_03B + tight_panel_gap)) +
  patchwork::plot_layout(widths = c(2.8, 1.2))

print(fig_03)

# Save combined (adjust height if needed)
save_fig(fig_03, "Fig_03_Combined_Year_and_AttemptType", width_mm = fig_width_double_mm, height_mm = 95)

# ============================================================
# Table 01: Year summary (n, mean±SD, earliest, latest)
# ============================================================

year_tbl <- dat %>%
  group_by(year) %>%
  summarise(
    `Nests (n)` = n(),
    mean_jul = mean(julian, na.rm = TRUE),
    sd_jul   = sd(julian, na.rm = TRUE),
    earliest_jul = min(julian, na.rm = TRUE),
    latest_jul   = max(julian, na.rm = TRUE),
    earliest_date = format(date[which.min(julian)], "%b %d"),
    latest_date   = format(date[which.max(julian)], "%b %d"),
    .groups = "drop"
  ) %>%
  mutate(
    `Mean Julian Day±SD` = sprintf("%.1f±%.1f", mean_jul, sd_jul),
    `Earliest (Min)`     = sprintf("%d (%s)", earliest_jul, earliest_date),
    `Latest (Max)`       = sprintf("%d (%s)", latest_jul, latest_date)
  ) %>%
  select(
    Year = year,
    `Nests (n)`,
    `Mean Julian Day±SD`,
    `Earliest (Min)`,
    `Latest (Max)`
  ) %>%
  arrange(Year)

write_csv(year_tbl, file.path(output_dir, "Table_01_FirstEgg_Summary_by_Year.csv"))

