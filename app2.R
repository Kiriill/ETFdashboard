# app.R
# ASX ETFs: Top-10 TSR (price-only, dividends reinvested)
# Four periods: Last month, Last year, Last 5 years, Last 10 years

library(shiny)
library(tidyquant)  # for tq_get()
library(dplyr)
library(tidyr)
library(purrr)

# 1) ETF universe
etf_symbols <- c(
  "VAS.AX","IVV.AX","IOZ.AX","NDQ.AX","VDHG.AX","VGS.AX","A200.AX","ASIA.AX",
  "VTS.AX","STW.AX","ETHI.AX","VEU.AX","VHY.AX","BNDS.AX","IJR.AX","IEM.AX",
  "IZZ.AX","RBTZ.AX","FUEL.AX","QOZ.AX"
)

# 2) Fetch 11 years of adjusted prices
price_data <- tq_get(
  x      = etf_symbols,
  get    = "stock.prices",
  from   = Sys.Date() - 365*11,
  to     = Sys.Date()
) %>%
  select(symbol, date, adjusted) %>%
  group_by(symbol) %>%
  arrange(date) %>%
  
  # ensure every calendar date is present
  complete(date = seq.Date(min(date), max(date), by = "day")) %>%
  fill(adjusted, .direction = "downup") %>%
  
  # compute daily returns and TSR
  mutate(
    ret = adjusted / lag(adjusted) - 1,
    ret = replace_na(ret, 0),
    tsr = 100 * cumprod(1 + ret)
  ) %>%
  ungroup()

# 3) Define horizons and precompute top-10 tables
horizons <- list(
  LastMonth   = 30,
  LastYear    = 365,
  Last5Years  = 365*5,
  Last10Years = 365*10
)

top10_tables <- map(horizons, function(days_back) {
  cutoff <- Sys.Date() - days_back
  price_data %>%
    filter(date >= cutoff) %>%
    group_by(symbol) %>%
    summarize(
      start_tsr = first(tsr),
      end_tsr   = last(tsr),
      .groups   = "drop"
    ) %>%
    mutate(
      totalRet = (end_tsr / start_tsr - 1) * 100,
      years    = days_back / 365,
      CAGR     = ((end_tsr / start_tsr)^(1/years) - 1) * 100
    ) %>%
    arrange(desc(CAGR)) %>%
    slice(1:10) %>%
    select(symbol, totalRet, CAGR)
})

# 4) Shiny UI: simple tables for sanity check
ui <- fluidPage(
  titlePanel("Top 10 ASX ETFs by TSR (Before Fees)"),
  p("Dividends reinvested; returns shown before management fees."),
  
  # one table per horizon
  map(names(top10_tables), function(id) {
    fluidRow(
      column(12, h3(gsub("([a-z])([A-Z])", "\\1 \\2", id))),
      column(12, tableOutput(id))
    )
  })
)

# 5) Shiny server: render each precomputed table
server <- function(input, output, session) {
  for (id in names(top10_tables)) {
    local({
      tbl <- top10_tables[[id]]
      output[[id]] <- renderTable({
        tbl
      }, digits = 2, rownames = FALSE)
    })
  }
}

shinyApp(ui, server)