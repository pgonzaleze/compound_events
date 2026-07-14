#### ================================================================= ####
# Composite Hotspot Risk Score — Salmon Aquaculture (Potential) Farm Sites
# Variables used: MHW (marine heatwave), TS_AEP (tropical storm AEP),
#                 bathymetry_class
#### ================================================================= ####

library(dplyr)
library(readr)
library(leaflet)
library(htmlwidgets)

#### Read data ####
df <- read_csv("Salmon_Aquaculture_Compund_Events.csv")
cat("Total rows in file:", nrow(df), "\n")

#### Keep only actual or potential farm locations ("1") ####
df <- df %>% filter(Potential_Farm_Location == 1)
cat("Rows flagged as farm / potential-farm location:", nrow(df), "\n")

#### Drop rows with no storm/heat data at all ####
# bathymetry_class alone isn't enough to call it a risk index -- a row needs
# at least one of MHW or TS_AEP to be scored.
n_before <- nrow(df)
df <- df %>% filter(!(is.na(MHW) & is.na(TS_AEP)))
cat("Dropped", n_before - nrow(df), "rows missing both MHW and TS_AEP\n")
cat("Rows remaining:", nrow(df), "\n")

#### Percentile-rank normalization to 1-10 ####
# Direction: higher raw value = higher risk for all three variables
#   - MHW: higher marine-heatwave intensity = more risk
#   - TS_AEP: higher tropical-storm exceedance probability = more risk
#   - bathymetry_class: higher value = shallower = more exposed = more risk
pct_rank_norm <- function(x) {
  r <- rank(x, na.last = "keep", ties.method = "average")
  pct <- (r - 1) / (sum(!is.na(x)) - 1)
  1 + pct * 9
}

df <- df %>%
  mutate(
    MHW_norm   = pct_rank_norm(MHW),
    TS_norm    = pct_rank_norm(TS_AEP),
    Bathy_norm = pct_rank_norm(bathymetry_class)
  )

cat("\nPairwise correlations (complete obs only):\n")
cat("MHW vs TS_AEP:   ", cor(df$MHW, df$TS_AEP, use = "complete.obs"), "\n")
cat("MHW vs bathy:    ", cor(df$MHW, df$bathymetry_class, use = "complete.obs"), "\n")
cat("TS_AEP vs bathy: ", cor(df$TS_AEP, df$bathymetry_class, use = "complete.obs"), "\n")

#### Composite score: average of whichever components are present ####
# Rule:
#   - MHW + TS_AEP + Bathymetry   (all three present)
#   - MHW + Bathymetry            (TS_AEP missing)
#   - TS_AEP + Bathymetry         (MHW missing)
# Rows missing both MHW and TS_AEP were already dropped in step 2b, so every
# remaining row has at least 2 of the 3 components.
comp_cols <- c("MHW_norm", "TS_norm", "Bathy_norm")
df$RiskScore_raw <- rowMeans(df[, comp_cols], na.rm = TRUE)
df$RiskScore <- pct_rank_norm(df$RiskScore_raw)

### Track how many of the 3 components fed each row's score ####
df$N_components <- rowSums(!is.na(df[, comp_cols]))

cat("\nNumber of components used per row:\n")
print(table(df$N_components))

#### Save results ####
out <- df %>%
  select(LON, LAT, EEZID, Potential_Farm_Location,
         MHW, TS_AEP, bathymetry_class, N_components, RiskScore)
#write_csv(out, "Salmon_Farm_Risk_Scores.csv")
#cat("\nSaved Salmon_Farm_Risk_Scores.csv with", nrow(out), "rows\n")

#### Build interactive map (for inspection) ####
pal <- colorNumeric(
  palette = c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c"),
  domain = c(1, 10)
)

df$popup_html <- paste0(
  "<b>Risk score:</b> ", round(df$RiskScore, 1), " / 10<br>",
  "MHW: ", ifelse(is.na(df$MHW), "no data", round(df$MHW, 1)), "<br>",
  "TS_AEP: ", ifelse(is.na(df$TS_AEP), "no data", round(df$TS_AEP, 3)), "<br>",
  "Bathymetry class: ", df$bathymetry_class, "<br>",
  "Components used: ", df$N_components, "/3"
)

m <- leaflet(df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng = ~LON, lat = ~LAT,
    radius = 3,
    color = "#000000", opacity = 0.2, weight = 0.3,
    fillColor = ~pal(RiskScore), fillOpacity = 0.85,
    popup = ~popup_html
  ) %>%
  addLegend(
    pal = pal, values = c(1, 10), title = "Risk score (1-10)",
    position = "bottomright"
  )

m
#saveWidget(m, "Salmon_Farm_Risk_Hotspot_Map.html", selfcontained = TRUE)
#cat("Saved Salmon_Farm_Risk_Hotspot_Map.html\n")
