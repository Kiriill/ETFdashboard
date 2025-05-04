# Load libraries
library(shiny)
library(tidyverse)
library(readxl)
library(rvest)
library(lubridate)
library(yfR)
library(DT)

# ---- Constants ----
cache_file <- "cached_etf_data.rds"
meta_file  <- "cached_etf_meta.rds"

# ---- Helper function ----
get_etf_data <- function() {
  message("Downloading latest ETF data...")
  
  # Scrape latest ASX report
  page <- read_html("https://www.asx.com.au/issuers/investment-products/asx-investment-products-monthly-report")
  links <- page %>% html_nodes("a") %>% html_attr("href") %>% discard(is.na)
  xlsx_links <- links %>% keep(~ str_detect(.x, regex("asx-investment-products-.*-abs\\.xlsx$", ignore_case = TRUE)))
  full_links <- unique(map_chr(xlsx_links, ~ if (str_detect(.x, "^http")) .x else paste0("https://www.asx.com.au", .x)))
  mth_year <- str_extract(full_links, "(?<=products-)[A-Za-z]{3}-\\d{4}")
  report_dates <- parse_date_time(mth_year, "b-Y")
  latest_url <- full_links[which.max(report_dates)]
  
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(latest_url, destfile = temp_file, mode = "wb")
  etp_list <- read_excel(temp_file, sheet = "Spotlight ETP List", skip = 9, col_types = "text")
  colnames(etp_list) <- str_replace_all(colnames(etp_list), "[\r\n]", " ") %>% str_squish()
  
  etf_symbols <- etp_list %>%
    pull("ASX Code") %>% str_subset("^[A-Z0-9]+$") %>% unique() %>% paste0(".AX")
  fund_info <- etp_list %>% transmute(symbol = paste0(`ASX Code`, ".AX"), Fund = `Fund Name`, Issuer = `Issuer`)
  
  periods <- tibble(
    period     = c("1m", "1y", "5y", "10y"),
    start_date = as_date(c(
      Sys.Date() %m-% months(1),
      Sys.Date() %m-% years(1),
      Sys.Date() %m-% years(5),
      Sys.Date() %m-% years(10)
    ))
  )
  
  buffer_days <- 5
  earliest_buffer <- min(periods$start_date) %m-% days(buffer_days)
  
  daily_raw <- yf_get(
    tickers         = etf_symbols,
    first_date      = (Sys.Date() %m-% months(1)) %m-% days(buffer_days),
    last_date       = Sys.Date(),
    freq_data       = "daily",
    do_parallel     = FALSE,
    thresh_bad_data = 0
  ) %>% select(symbol = ticker, date = ref_date, adjusted = price_adjusted)
  
  monthly_raw <- yf_get(
    tickers         = etf_symbols,
    first_date      = earliest_buffer,
    last_date       = Sys.Date(),
    freq_data       = "monthly",
    do_parallel     = FALSE,
    thresh_bad_data = 0
  ) %>% select(symbol = ticker, date = ref_date, adjusted = price_adjusted)
  
  all_prices <- bind_rows(daily_raw, monthly_raw) %>%
    distinct(symbol, date, .keep_all = TRUE) %>%
    arrange(symbol, date)
  
  results <- tibble()
  for (i in seq_len(nrow(periods))) {
    per <- periods$period[i]
    sd  <- periods$start_date[i]
    period_results <- tibble()
    
    for (sym in etf_symbols) {
      df_sym <- all_prices %>% filter(symbol == sym)
      if (nrow(df_sym) == 0) next
      
      start_price <- df_sym %>%
        filter(date <= sd, date >= sd %m-% days(buffer_days)) %>%
        slice_max(date, n = 1) %>%
        pull(adjusted)
      
      end_price <- df_sym %>%
        filter(date <= Sys.Date()) %>%
        slice_max(date, n = 1) %>%
        pull(adjusted)
      
      if (length(start_price) == 0 || length(end_price) == 0) next
      tsr <- (end_price / start_price - 1) * 100
      period_results <- bind_rows(period_results, tibble(symbol = sym, tsr = tsr))
    }
    
    if (nrow(period_results) > 0) {
      period_results <- period_results %>% mutate(period = per)
      results <- bind_rows(results, period_results)
    }
  }
  
  summary_table <- results %>%
    group_by(period) %>% arrange(desc(tsr), .by_group = TRUE) %>%
    mutate(rank = row_number()) %>% ungroup() %>%
    pivot_wider(names_from = period, values_from = c(tsr, rank), names_glue = "{.value}_{period}") %>%
    left_join(fund_info, by = "symbol")
  
  top10_list <- map(periods$period, function(per) {
    col <- paste0("tsr_", per)
    summary_table %>%
      select(symbol, Fund, Issuer, tsr = !!sym(col)) %>%
      arrange(desc(tsr)) %>%
      slice_head(n = 10) %>%
      mutate(
        TSR = sprintf("%.1f%%", tsr),
        Annual = case_when(
          per == "1m"  ~ sprintf("%.1f%%", ((1 + tsr/100)^12 - 1) * 100),
          per == "1y"  ~ sprintf("%.1f%%", tsr),
          per == "5y"  ~ sprintf("%.1f%%", ((1 + tsr/100)^(1/5) - 1) * 100),
          per == "10y" ~ sprintf("%.1f%%", ((1 + tsr/100)^(1/10) - 1) * 100)
        )
      )
  })
  names(top10_list) <- periods$period
  
  saveRDS(top10_list, cache_file)
  saveRDS(Sys.time(), meta_file)
  return(top10_list)
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("Top Performing ASX ETFs"),
  p("List of top performing ASX-listed ETFs by total shareholder return (TSR) assuming dividends are re-invested. Note this does not take into account management and transaction fees."),
  actionButton("refresh", "🔄 Refresh Data", class = "btn-primary", style = "margin-bottom: 10px;"),
  textOutput("last_updated"),
  tags$hr(),
  uiOutput("tables_ui"),
  tags$hr(),
  p(em("This dashboard is provided for informational purposes only and does not constitute financial, investment, tax, or other professional advice. Past performance is not indicative of future results. You should conduct your own research and/or consult a qualified professional before making any investment decisions. Financial data quality has not been independently audited."))
)

# ---- Server ----
server <- function(input, output, session) {
  etf_data <- reactiveVal(NULL)
  last_updated <- reactiveVal(NULL)
  
  observe({
    if (is.null(etf_data())) {
      if (file.exists(cache_file)) {
        etf_data(readRDS(cache_file))
        if (file.exists(meta_file)) {
          last_updated(readRDS(meta_file))
        }
      } else {
        etf_data(get_etf_data())
        last_updated(Sys.time())
      }
    }
  })
  
  observeEvent(input$refresh, {
    etf_data(get_etf_data())
    last_updated(Sys.time())
  })
  
  output$last_updated <- renderText({
    req(last_updated())
    paste("Last updated:", format(last_updated(), "%Y-%m-%d %H:%M %Z"))
  })
  
  output$tables_ui <- renderUI({
    req(etf_data())
    lapply(rev(names(etf_data())), function(per) {
      fluidRow(
        style = "margin-bottom: 40px;",
        column(
          width = 12,
          h4(switch(per,
                    "1m" = "Last Month",
                    "1y" = "Last Year",
                    "5y" = "Last 5 Years",
                    "10y" = "Last 10 Years"
          )),
          DTOutput(outputId = paste0("tbl_", per))
        )
      )
    })
  })
  
  observe({
    req(etf_data())
    lapply(names(etf_data()), function(per) {
      df <- etf_data()[[per]] %>%
        mutate(
          Medal = case_when(
            row_number() == 1 ~ "🥇",
            row_number() == 2 ~ "🥈",
            row_number() == 3 ~ "🥉",
            TRUE               ~ ""
          )
        ) %>%
        select(Medal, Fund, Issuer, TSR, Annual)
      
      output[[paste0("tbl_", per)]] <- renderDT({
        datatable(
          df,
          rownames = FALSE,
          colnames = c("", "Fund", "Issuer", "TSR", "Annual"),
          options = list(dom = 't', pageLength = 10)
        )
      })
    })
  })
}

# ---- Run App ----
shinyApp(ui, server)
