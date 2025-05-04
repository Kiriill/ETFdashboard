# app.R
# ASX ETFs: Fee-adjusted TSR, scrollable checkboxes, black text + colored squares
# No label overlap: we place final labels in a vertical column, sorted by TSR descending.

library(shiny)
library(quantmod)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(RColorBrewer)

# -------------------------------------------------------------------------
# 1) LIST OF 20 POPULAR ASX ETFS & FRIENDLY NAMES
# -------------------------------------------------------------------------
etf_symbols <- c(
  "VAS.AX",   # Vanguard Australian Shares Index
  "IVV.AX",   # iShares S&P 500 (AU)
  "IOZ.AX",   # iShares Core S&P/ASX 200
  "NDQ.AX",   # BetaShares NASDAQ 100
  "VDHG.AX",  # Vanguard Diversified High Growth
  "VGS.AX",   # Vanguard Intl Shares
  "A200.AX",  # BetaShares Australia 200
  "ASIA.AX",  # BetaShares Asia Tech
  "VTS.AX",   # Vanguard US Total
  "STW.AX",   # SPDR S&P/ASX 200
  "ETHI.AX",  # BetaShares Sustainability
  "VEU.AX",   # Vanguard All-World ex US
  "VHY.AX",   # Vanguard Aust High Yield
  "BNDS.AX",  # BetaShares Aus Bond
  "IJR.AX",   # iShares S&P Small Cap (AU)
  "IEM.AX",   # iShares MSCI EM (AU)
  "IZZ.AX",   # iShares China Large-Cap (AU)
  "RBTZ.AX",  # BetaShares Robotics & AI
  "FUEL.AX",  # BetaShares Global Energy
  "QOZ.AX"    # BetaShares FTSE RAFI 200
)

etf_names <- c(
  "VAS.AX"  = "Vanguard Aust Shares (VAS)",
  "IVV.AX"  = "iShares S&P500 (IVV)",
  "IOZ.AX"  = "iShares Core ASX200 (IOZ)",
  "NDQ.AX"  = "BetaShares NASDAQ 100 (NDQ)",
  "VDHG.AX" = "Vanguard High Growth (VDHG)",
  "VGS.AX"  = "Vanguard Intl Shares (VGS)",
  "A200.AX" = "BetaShares Australia 200 (A200)",
  "ASIA.AX" = "BetaShares Asia Tech (ASIA)",
  "VTS.AX"  = "Vanguard US Total (VTS)",
  "STW.AX"  = "SPDR S&P/ASX 200 (STW)",
  "ETHI.AX" = "BetaShares Sustainability (ETHI)",
  "VEU.AX"  = "Vanguard All-World ex US (VEU)",
  "VHY.AX"  = "Vanguard Aust High Yield (VHY)",
  "BNDS.AX" = "BetaShares Aus Bond (BNDS)",
  "IJR.AX"  = "iShares S&P Small Cap (IJR)",
  "IEM.AX"  = "iShares MSCI EM (IEM)",
  "IZZ.AX"  = "iShares China Large-Cap (IZZ)",
  "RBTZ.AX" = "BetaShares Robotics & AI (RBTZ)",
  "FUEL.AX" = "BetaShares Global Energy (FUEL)",
  "QOZ.AX"  = "BetaShares FTSE RAFI (QOZ)"
)

# -------------------------------------------------------------------------
# 2) DEFINE COLOR PALETTE FOR EACH SYMBOL
# -------------------------------------------------------------------------
colors1     <- brewer.pal(12, "Paired")  # 12 colors
colors2     <- brewer.pal(8,  "Dark2")   # 8  colors
all_colors  <- c(colors1, colors2)       # total 20
symbol_colors <- setNames(all_colors, etf_symbols)

# -------------------------------------------------------------------------
# 3) DOWNLOAD ~10 YEARS OF DATA
# -------------------------------------------------------------------------
global_start_date <- Sys.Date() - 365*10
for (sym in etf_symbols) {
  suppressWarnings(
    getSymbols(sym, src="yahoo", from=global_start_date, auto.assign=TRUE)
  )
}
etf_data_list <- lapply(etf_symbols, function(sym){
  if (exists(sym)) get(sym) else NULL
})
names(etf_data_list) <- etf_symbols

# -------------------------------------------------------------------------
# HELPER: build TSR data (daily fee)
# -------------------------------------------------------------------------
build_tsr_df <- function(sym, annual_fee_percent, date1, date2){
  xts_data <- etf_data_list[[sym]]
  if (is.null(xts_data) || nrow(xts_data)<2) return(NULL)
  
  # sort & subset
  xts_data <- xts_data[order(index(xts_data)), ]
  trimmed  <- xts_data[paste0(date1,"::",date2)]
  if (nrow(trimmed)<2) return(NULL)
  
  prices <- Ad(trimmed)
  df <- data.frame(
    date      = index(prices),
    adj_close = as.numeric(prices),
    Symbol    = sym
  )
  df <- df[order(df$date), ]
  if (nrow(df)<2) return(NULL)
  
  df$daily_ret <- c(0, diff(df$adj_close)/df$adj_close[-nrow(df)])
  
  fee_decimal  <- annual_fee_percent/100
  daily_fee    <- fee_decimal / 252
  
  df$net_ret   <- df$daily_ret - daily_fee
  df$tsr_index <- 100 * cumprod(1 + df$net_ret)
  df$Name      <- etf_names[sym]
  
  df
}

# -------------------------------------------------------------------------
# 4) SHINY UI
# -------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(
    div(
      "ASX ETFs: Interactive TSR Dashboard",
      tags$h5("Assumes daily pro-rata fees & Yahoo Adjusted Close for dividends/splits")
    )
  ),
  
  fluidRow(
    column(
      width = 3,
      # Scrollable checkboxes
      div(
        style="height:300px; overflow-y:auto; border:1px solid #ccc; padding:5px;",
        checkboxGroupInput(
          "etfSelect", "Select ETFs:",
          choices  = etf_symbols,
          selected = c("VAS.AX","IVV.AX","IOZ.AX","NDQ.AX")
        )
      ),
      sliderInput("annualFee", "Annual Fee (%)", 
                  min=0, max=2, step=0.05, value=0.10),
      dateRangeInput("dateRange","Date Range:",
                     start=Sys.Date()-365*2,
                     end=Sys.Date(),
                     min=global_start_date,
                     max=Sys.Date(),
                     format="dd M yyyy"),
      helpText("TSR starts at 100 on the earliest date in your chosen range.")
    ),
    column(
      width = 9,
      h3("TSR Results"),
      h4(textOutput("plainEnglishCallout")),
      
      plotlyOutput("tsrPlot", height="550px"), # <--- reduced height
      
      br(),
      h4("Year-over-Year Returns"),
      tableOutput("yearlyTsrTable")
    )
  )
)

# -------------------------------------------------------------------------
# 5) SHINY SERVER
# -------------------------------------------------------------------------
server <- function(input, output, session){
  
  # Reactive TSR
  tsr_data <- reactive({
    req(input$etfSelect, input$dateRange)
    sdate <- input$dateRange[1]
    edate <- input$dateRange[2]
    if (is.na(sdate) || is.na(edate) || sdate >= edate) return(NULL)
    
    out_list <- lapply(input$etfSelect, function(sym){
      build_tsr_df(sym, input$annualFee, sdate, edate)
    })
    out_list <- Filter(Negate(is.null), out_list)
    if(length(out_list)==0) return(NULL)
    
    do.call(rbind, out_list)
  })
  
  # Plain-English best
  output$plainEnglishCallout <- renderText({
    data_tsr <- tsr_data()
    if (is.null(data_tsr) || nrow(data_tsr)<2) {
      return("No valid data for the selected ETFs/date range.")
    }
    
    final_vals <- data_tsr %>%
      group_by(Symbol) %>%
      summarize(
        Name=first(Name),
        final_tsr=last(tsr_index),
        .groups="drop"
      )
    best <- final_vals %>% slice_max(final_tsr, n=1)
    if(nrow(best)<1) return("No data to compare.")
    
    best_name <- best$Name
    best_tsr  <- best$final_tsr
    
    sdate <- input$dateRange[1]
    edate <- input$dateRange[2]
    sdate_str <- format(sdate, "%d %b %Y")
    edate_str <- format(edate, "%d %b %Y")
    days_diff <- as.numeric(difftime(edate, sdate,units="days"))
    years_diff<- days_diff/365
    
    worth_str <- formatC(best_tsr, format="f", digits=2)
    
    cagr_val <- if(years_diff>0.01) {
      (best_tsr/100)^(1/years_diff)-1
    } else NA
    
    cagr_str <- if(!is.na(cagr_val)){
      paste0(round(cagr_val*100,2), "%")
    } else "N/A"
    
    paste0(
      "Between ", sdate_str, " and ", edate_str,
      ", the best-performing ETF was ", best_name,
      ". $100 became $", worth_str,
      ". Implied CAGR: ", cagr_str,
      " (Fee: ", input$annualFee, "%)."
    )
  })
  
  # Plotly with no overlap labels
  output$tsrPlot <- renderPlotly({
    data_tsr <- tsr_data()
    if (is.null(data_tsr) || nrow(data_tsr)<2) return(NULL)
    
    p <- plot_ly()
    
    # lines
    syms <- unique(data_tsr$Symbol)
    for(sym in syms){
      df_sym <- data_tsr %>% filter(Symbol==sym)
      if(nrow(df_sym)<2) next
      color_here <- symbol_colors[sym]
      
      p <- p %>%
        add_trace(
          data=df_sym,
          x=~date,
          y=~tsr_index,
          name=unique(df_sym$Name),
          type='scatter',
          mode='lines',
          line=list(color=color_here)
        )
    }
    
    # final points (sorted by TSR desc to avoid overlap)
    # We'll place labels in a vertical column, top = highest TSR
    final_info <- data_tsr %>%
      group_by(Symbol) %>%
      summarize(
        Name    = first(Name),
        x_final = last(date),
        y_final = last(tsr_index),
        .groups="drop"
      ) %>%
      arrange(desc(y_final))
    
    date_start <- input$dateRange[1]
    date_end   <- input$dateRange[2]
    ddays      <- as.numeric(difftime(date_end, date_start, units="days"))
    years_diff <- ddays / 365
    
    final_info <- final_info %>%
      mutate(
        cagr = ifelse(years_diff>0.01, (y_final/100)^(1/years_diff)-1, NA),
        label_text = ifelse(
          is.na(cagr),
          paste0("TSR: ", round(y_final,2)),
          paste0("TSR: ", round(y_final,2), "\nCAGR: ", round(cagr*100,2), "%")
        )
      )
    
    # We'll offset them by i * 30 (downwards)
    # so top TSR is near top, next is below it, etc.
    n <- nrow(final_info)
    for(i in seq_len(n)){
      row <- final_info[i,]
      color_here <- symbol_colors[row$Symbol]
      # offset for label i (top = 0, next = -30, etc.)
      yshift_val <- (i-1)*(-30)
      
      p <- p %>%
        add_trace(
          x            = row$x_final,
          y            = row$y_final,
          mode         = "markers+text",
          marker       = list(symbol="square", color=color_here, size=10),
          text         = row$label_text,
          textfont     = list(color="black", size=12),
          textposition = "middle right",
          yshift       = yshift_val,
          hoverinfo    = "none",
          showlegend   = FALSE,
          name         = paste0(row$Name, " end-label"),
          cliponaxis   = FALSE
        )
    }
    
    p <- p %>%
      layout(
        title     = "Interactive TSR Chart (Fee-Adjusted)",
        xaxis     = list(title="Date"),
        yaxis     = list(title="TSR Index"),
        hovermode = "x unified"
      )
    p
  })
  
  # Year-over-Year table
  output$yearlyTsrTable <- renderTable({
    data_tsr <- tsr_data()
    if(is.null(data_tsr) || nrow(data_tsr)<2){
      return(data.frame("No data"=character(0)))
    }
    
    data_tsr <- data_tsr %>%
      mutate(Year=as.integer(year(date)))
    
    last_in_year <- data_tsr %>%
      group_by(Symbol, Name, Year) %>%
      summarize(
        last_tsr=last(tsr_index),
        .groups="drop"
      ) %>%
      arrange(Symbol, Year)
    
    yoy_table <- last_in_year %>%
      group_by(Symbol) %>%
      mutate(
        prev_tsr=lag(last_tsr),
        yoy=(last_tsr/prev_tsr)-1
      ) %>%
      ungroup()
    
    yoy_wide <- yoy_table %>%
      mutate(
        yoy_str = ifelse(is.na(yoy), NA, paste0(round(yoy*100,2), "%"))
      ) %>%
      select(Name, Year, yoy_str) %>%
      pivot_wider(
        names_from=Name,
        values_from=yoy_str
      ) %>%
      arrange(Year)
    
    yoy_wide
  }, striped=TRUE, hover=TRUE)
}

shinyApp(ui, server)
