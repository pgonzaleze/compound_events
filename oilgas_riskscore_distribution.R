# ==========================================================
# RiskScore Distribution: Operating vs Discovered/In Development
# Data: Oil_Gas_Risk_Scores.csv
# ==========================================================

library(tidyverse)   # loads ggplot2 + dplyr

#### Load data ####
df <- read.csv("Oil_Gas_Risk_Scores.csv", stringsAsFactors = FALSE)

#### Create the comparison group ####
# Status has 3 levels: "operating", "discovered", "in development".
# We collapse the latter two into one group, per your comparison of interest.
df <- df %>%
  mutate(Group = if_else(Status == "operating",
                          "Operating",
                          "Discovered / In Development"))

##### Choose risk score to plot ###
risk_col <- "RiskScore"

#### Histogram ####
ggplot(df, aes(x = .data[[risk_col]], fill = Group)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.85) +
  facet_wrap(~ Group, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(
    "Operating" = "#660033",
    "Discovered / In Development" = "#003366"
  )) +
  labs(
    x = risk_col,
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#### Boxplot ####
ggplot(df, aes(x = Group, y = .data[[risk_col]], fill = Group)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(
    #title = paste(risk_col, "by Operational Status"),
    x = "",
    y = risk_col
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#### Summary stats to accompany the plots ####
summary_table <- df %>%
  group_by(Group) %>%
  summarise(
    n      = n(),
    mean   = mean(.data[[risk_col]], na.rm = TRUE),
    median = median(.data[[risk_col]], na.rm = TRUE),
    sd     = sd(.data[[risk_col]], na.rm = TRUE),
    min    = min(.data[[risk_col]], na.rm = TRUE),
    max    = max(.data[[risk_col]], na.rm = TRUE)
  )

print(summary_table)

# ==========================================================
# % SUMMARY STATS 
# ==========================================================

#### Define risk bands (defaulting to tertiles of the overall distribution) ####
# Swap these breaks for fixed/domain-specific cutoffs if you have them.
breaks <- quantile(df[[risk_col]], probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
df <- df %>%
  mutate(RiskBand = cut(.data[[risk_col]],
                        breaks = breaks,
                        labels = c("Low", "Medium", "High"),
                        include.lowest = TRUE))

#### % of sites per Group that fall in each Risk Band ####
pct_table <- df %>%
  count(Group, RiskBand) %>%
  group_by(Group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

print(pct_table)


#### Stacked % bar chart (great for sharing with non-technical audiences) ####
ggplot(pct_table, aes(x = Group, y = pct, fill = RiskBand)) +
  geom_col(position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("Low" = "#666699",      # green
                               "Medium" = "#996699",   # amber
                               "High" = "#CC3399")) +  # red
  labs(
    #title = paste("% of Sites by Risk Band -", risk_col),
    x = "", y = "% of Sites", fill = "Risk Band"
  ) +
  coord_flip() +
  theme_minimal()

##### % difference in mean RiskScore, Operating vs Discovered/In Development ####
means <- summary_table$mean
pct_diff <- round(100 * (means[summary_table$Group == "Operating"] -
                           means[summary_table$Group == "Discovered / In Development"]) /
                    means[summary_table$Group == "Discovered / In Development"], 1)
cat("Operating sites have a", pct_diff, "% higher mean", risk_col,
    "than Discovered / In Development sites.\n")

