# STAT 442: 2024 U.S. Traffic Laws & Fatal Crashes

**R Shiny Dashboard | November 2026**

🔗 [View the live app](https://maddisononken.shinyapps.io/GroupProject/)

---

## Project Overview

This dashboard examines how 2024 state traffic laws relate to fatal crash rates (crashes per 100,000 people) across all 50 U.S. states. It combines five interactive choropleth maps and top-10 bar charts, one for each law category, into a single multi-tab Shiny app.

---

## Team Contributions

| Name | Role |
|---|---|
| **Celia** | Minimum driving age section — built the interactive U.S. map filtered by age group, added the toggle between overall and minor fatality rate, and sourced the minor population data from HDPulse. |
| **Faith** | Seatbelt laws section — built the map filtered by front- and rear-seat enforcement level (Primary, Secondary, None). |
| **Madisyn** | Distracted driving section — built the map filtered by handheld phone ban and texting ban enforcement type. |
| **Sydney** | Speed limits section — built the map filtered by maximum posted speed limit category. |
| **Ashmi** | Motorcycle safety section — built the map filtered by helmet law type (Universal vs. Age-Based) and the Top 10 States bar chart. |
| **Maddison** | Data — compiled and cleaned the Excel dataset, corrected the fatal crash columns to crashes per 100,000 population, and assembled all five sections into the final combined Shiny app. |

---

## App Structure

The app has six tabs:

| Tab | Description |
|---|---|
| **Overview** | Project summary and team credits. |
| **Driving Age** | Map and top-10 chart filtered by minimum driving age group; toggle between overall and minor fatality rate. |
| **Seatbelt Laws** | Map and top-10 chart filtered by front or rear seat enforcement level. |
| **Distracted Driving** | Map and top-10 chart filtered by handheld phone ban or texting ban type. |
| **Speed Limits** | Map and top-10 chart filtered by maximum speed limit category. |
| **Motorcycle Safety** | Map and top-10 chart filtered by helmet law requirement. |

All maps use the same turquoise-to-purple color scale. Gray states do not match the selected filter. Hovering over any state shows its law details and crash rate.

---

## How to Run

**Requirements:** R with the following packages installed:

```r
install.packages(c("shiny", "readxl", "dplyr", "plotly", "bslib"))
```

**Steps:**

1. Place `traffic_laws_app.R` and `2024StateTrafficLawsSummary.xlsx` in the same folder.
2. Open `traffic_laws_app.R` in RStudio.
3. Click **Run App**, or run from the console:

```r
shiny::runApp("traffic_laws_app.R")
```

---

## Data

- **2024StateTrafficLawsSummary.xlsx** — compiled by the team; fatal crash columns represent crashes per 100,000 population.
