#### ============================================================== ####
# Composite Hotspot Risk Score — Offshore Oil & Gas Infrastructure
# Variables used: Cat_3 (Category-3 storm AEP), Bathymetry_class
#### ============================================================== ####

library(dplyr)
library(readr)
library(leaflet)
library(htmlwidgets)

#### Read data ####
df <- read_csv("Oil_Gas_Compound_Events_Full.csv")
cat("Total sites:", nrow(df), "\n")

# Filrter by status
df <- df %>% filter(Status %in%c("operating", "discovered", "in development"))

#### Percentile-rank normalization to 1-10 ####
pct_rank_norm <- function(x) {
  r <- rank(x, na.last = "keep", ties.method = "average")
  pct <- (r - 1) / (sum(!is.na(x)) - 1)
  1 + pct * 9
}

df <- df %>%
  mutate(
    Cat3_norm  = pct_rank_norm(Cat_3),
    Bathy_norm = pct_rank_norm(Bathymetry_class)
  )

cat("Correlation Cat_3 vs Bathymetry_class:",
    cor(df$Cat_3, df$Bathymetry_class, use = "complete.obs"), "\n")

#### Composite score ####
df$RiskScore_Equal_raw <- rowMeans(df[, c("Cat3_norm", "Bathy_norm")], na.rm = TRUE)
df$RiskScore_Equal <- pct_rank_norm(df$RiskScore_Equal_raw)

cat("\nRows with Cat_3 present:", sum(!is.na(df$Cat_3)), "of", nrow(df), "\n")

#### Save results if needed ##### 
out <- df %>%
  select(Unit_ID, Unit_Name, Country_Area, Status, Production_start_year, Operator,
         Latitude, Longitude, Cat_3, Bathymetry_class,
         RiskScore_Equal)
#write_csv(out, "Oil_Gas_Risk_Scores.csv")
#cat("Saved OilGas_Risk_Scores.csv with", nrow(out), "rows\n")

#### Build interactive map to make inspections ####
pal <- colorNumeric(
  palette = c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c"),
  domain = c(1, 10)
)

df$popup_html <- paste0(
  "<b>", df$Unit_Name, "</b><br>",
  "Operator: ", df$Operator, "<br>",
  "Status: ", df$Status, "<br>",
  "<b>Risk score:</b> ", round(df$RiskScore_Equal, 1), " / 10<br>",
  "Cat_3 AEP: ", ifelse(is.na(df$Cat_3), "no data", round(df$Cat_3, 3)), "<br>",
  "Bathymetry class: ", df$Bathymetry_class
)

m <- leaflet(df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    radius = ~3 + (RiskScore_Equal - 1) * 0.45,
    color = "#000000",
    opacity = 0.25,
    weight = 0.5,
    fillColor = ~pal(RiskScore_Equal),
    fillOpacity = 0.85,
    popup = ~popup_html
  ) %>%
  addLegend(
    pal = pal,
    values = c(1, 10),
    title = "Risk score (1-10)",
    position = "bottomright"
  )
m
#saveWidget(m, "OilGas_Risk_Hotspot_Map.html", selfcontained = TRUE)
#cat("Saved OilGas_Risk_Hotspot_Map.html\n")
