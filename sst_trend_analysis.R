# ======================================================================
# Linear trend analysis of SST time series, grouped by Country
# ======================================================================


#### Packages #####
required_pkgs <- c("dplyr", "tidyr", "lubridate", "ggplot2")
to_install <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(required_pkgs, library, character.only = TRUE))


#### Read data ####
df <- read.csv(
  "SST_timeseries.csv",
  header = TRUE,
  na.strings = c("", "NA"),
  stringsAsFactors = FALSE
)

#### Parse time ####
# Handles both "2002-07-04 0:00" and "2002-07-04 12:00:00" style timestamps
df$time <- lubridate::parse_date_time(df$time, orders = c("Ymd HM", "Ymd HMS"))

#### Drop rows with missing SST or unparseable time ####
df <- df %>% filter(!is.na(SST), !is.na(time))

#### Numeric time in years since the first observation (slope -> degC/year)
t0 <- min(df$time)
df$t_years <- as.numeric(difftime(df$time, t0, units = "days")) / 365.25

#### Fit a linear trend per country ####
trend_results <- df %>%
  group_by(Country) %>%
  group_modify(~ {
    fit <- lm(SST ~ t_years, data = .x)
    s <- summary(fit)
    ci <- confint(fit)
    tibble(
      n_obs          = nrow(.x),
      slope_C_per_yr = coef(fit)[["t_years"]],
      se_slope       = s$coefficients["t_years", "Std. Error"],
      p_value        = s$coefficients["t_years", "Pr(>|t|)"],
      r_squared      = s$r.squared,
      ci_low_95      = ci["t_years", 1],
      ci_high_95     = ci["t_years", 2]
    )
  }) %>%
  ungroup() %>%
  arrange(desc(slope_C_per_yr))

print(trend_results, n = Inf)


columnas_num <- sapply(trend_results, is.numeric)
trend_results[columnas_num] <- round(trend_results[columnas_num], digits = 3)
as.data.frame(trend_results)

#### pooled trend across all countries combined (Optional) ####
overall_fit <- lm(SST ~ t_years, data = df)
cat("\nPooled trend across all locations:\n")
print(summary(overall_fit))

#### visualize each trend (optional) ####
p <- ggplot(df, aes(x = time, y = SST)) +
  geom_point(alpha = 0.15, size = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick", linewidth = 1) +
  facet_wrap(~ Country, scales = "free_y") +
  labs(title = "SST linear trend by location", x = NULL, y = "SST (\u00B0C)") +
  theme_minimal()

print(p)

# ggsave("sst_trend_plot.png", p, width = 11, height = 7, dpi = 150)
# 
# # ---- 6. Save the results table ----
# write.csv(trend_results, "sst_trend_results.csv", row.names = FALSE)
# 
# cat("\nDone. Results saved to sst_trend_results.csv and sst_trend_plot.png\n")
