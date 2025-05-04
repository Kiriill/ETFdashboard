# ASX ETF Dashboard

**Author:** Kiriill Butler  
**Status:** Under development  
**Live dashboard:** [https://kiriillb.shinyapps.io/ASXETFdashboard/](https://kiriillb.shinyapps.io/ASXETFdashboard/)

---

## Purpose

This dashboard identifies the **top-performing ASX-listed exchange-traded funds (ETFs)** across different time horizons using **total shareholder return (TSR)** as the performance metric. Returns are calculated assuming dividends are reinvested, but do **not** account for management or transaction fees.

---

## Key Features

- **Automated data refresh**: Pulls the most recent ASX ETF listings and price data from Yahoo Finance.
- **Time horizons**: Ranks ETFs by TSR over the past:
  - 1 month
  - 1 year
  - 5 years
  - 10 years
- **Interactive dashboard**: Built in R Shiny with sortable tables and medal icons for top 3 performers.
- **Manual refresh**: One-click button to force a refresh of data on demand.
- **Timestamped updates**: Displays the last time data was refreshed.

---

## Technologies Used

- **R / Shiny**
- **tidyverse** (data wrangling)
- **readxl** (reading ASX Excel reports)
- **rvest** (web scraping)
- **lubridate** (date handling)
- **yfR** (financial data from Yahoo Finance)
- **DT** (interactive tables)

---

## Live Site

> 📍 [kiriillb.shinyapps.io/ASXETFdashboard](https://kiriillb.shinyapps.io/ASXETFdashboard)

_This site is still under development. UI and functionality may change._

---

## Disclaimer

This dashboard is provided for **informational purposes only** and does **not** constitute financial, investment, tax, or other professional advice. **Past performance is not indicative of future results.** You should conduct your own research and/or consult a qualified professional before making any investment decisions. Financial data quality has **not been independently audited**.
