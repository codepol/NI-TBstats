# Load packages used by the app. Install missing packages, if needed.
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(gitlink)
library(lubridate)
library(plotly)

# Read data from CSV files
TBAni <- read_csv("data/TB_Animal_SummaryLong20250820.csv")
TBAniPIV <- read_csv("data/TB_Animal_SummaryPivot20250820.csv")
TBHerds <- read_csv("data/TB_ReactorHerds_SummaryAll20250820.csv")
TBHerdsPIV <- read_csv("data/TB_ReactorHerds_SummaryPivot20250820.csv")

# Shiny UI ----
# UI
ui <- fluidPage(
  titlePanel("NI TB Cases Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("areaInput", "Select Area(s):", 
                  choices = unique(TBAni$Area), 
                  selected = "Armagh", multiple = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("TB Reactor Animals",
                 plotlyOutput("tsPlot"),
                 br(),
                 plotlyOutput("pctChangePlot"),
                 br(),
                 h4("Count of TB Reactor Animals over the past 3 calendar years"),
                 DT::dataTableOutput("TBHPivot")
                 ),
        tabPanel("TB Reactor Herds",
                 plotlyOutput("tsPlotHerds"),
                 br(),
                 plotlyOutput("pctChangePlotHerds"),
                 br(),
                 h4("Count of TB Reactor Herds over the past 3 calendar years"),
                 DT::dataTableOutput("TBAPivot")
                 )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    TBAni %>% filter(Area %in% input$areaInput)
  })
  
  # TB Reactor Animals Time Series Plot
  output$tsPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Date, y = Value, color = Area)) +
      geom_line(linewidth = 1) +
      geom_point() +
      labs(title = "TB Reactor Animals Time Series by Area", x = "Date", y = "Value") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  filtered_data2 <- reactive({
    TBHerds %>% filter(Area %in% input$areaInput)
  })
  
  # TB Reactor Hers Time Series Plot
  output$tsPlotHerds <- renderPlotly({
    p <- ggplot(filtered_data2(), aes(x = Date, y = Value, color = Area)) +
      geom_line(linewidth = 1) +
      geom_point() +
      labs(title = "TB Reactor Herds Time Series by Area", x = "Date", y = "Value") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # === TB Reactor Animals % Change Chart ===
  output$pctChangePlot <- renderPlotly({
    latest_date <- max(TBAni$Date)
    prev_year_date <- latest_date %m-% years(1)
    
    yoy_data <- TBAni %>%
      filter(Date %in% c(latest_date, prev_year_date)) %>%
      mutate(period = case_when(
        Date == latest_date ~ "latest",
        Date == prev_year_date ~ "previous"
      )) %>%
      select(Area, period, Value) %>%
      pivot_wider(names_from = period, values_from = Value) %>%
      filter(!is.na(latest), !is.na(previous)) %>%
      mutate(
        pct_change = 100 * (latest - previous) / previous
      )
    
    if (nrow(yoy_data) == 0) {
      message("No data to display for YoY change.")
      return(NULL)
    }
    
    p <- ggplot(yoy_data, aes(x = reorder(Area, pct_change), y = pct_change, fill = pct_change > 0,
                              text = paste0("Area: ", Area,
                                            "<br>Latest: ", latest,
                                            "<br>Previous: ", previous,
                                            "<br>% Change: ", round(pct_change, 1), "%"))) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "forestgreen")) +
      labs(
        title = paste("TB Reactor Animals YoY % Change —", format(latest_date, "%B %Y"), " vs ", 
                      format(prev_year_date, "%B %Y")),
        x = "Area", y = "% Change"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })

  # === TB Reactor Herds % Change Chart ===
  output$pctChangePlotHerds <- renderPlotly({
    latest_date <- max(TBHerds$Date)
    prev_year_date <- latest_date %m-% years(1)
    
    yoy_data <- TBHerds %>%
      filter(Date %in% c(latest_date, prev_year_date)) %>%
      mutate(period = case_when(
        Date == latest_date ~ "latest",
        Date == prev_year_date ~ "previous"
      )) %>%
      select(Area, period, Value) %>%
      pivot_wider(names_from = period, values_from = Value) %>%
      filter(!is.na(latest), !is.na(previous)) %>%
      mutate(
        pct_change = 100 * (latest - previous) / previous
      )
    
    if (nrow(yoy_data) == 0) {
      message("No data to display for YoY change.")
      return(NULL)
    }
    
    p <- ggplot(yoy_data, aes(x = reorder(Area, pct_change), y = pct_change, fill = pct_change > 0,
                              text = paste0("Area: ", Area,
                                            "<br>Latest: ", latest,
                                            "<br>Previous: ", previous,
                                            "<br>% Change: ", round(pct_change, 1), "%"))) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "forestgreen")) +
      labs(
        title = paste("TB Reactor Herds YoY % Change —", format(latest_date, "%B %Y"), " vs ", 
                      format(prev_year_date, "%B %Y")),
        x = "Area", y = "% Change"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })

  # === YOY TB Animal Data Table === #  
  output$yoyTable <- DT::renderDataTable({
    latest_date <- max(TBAni$Date)
    prev_year_date <- latest_date %m-% years(1)
    
    yoy_data <- TBAni %>%
      filter(Date %in% c(latest_date, prev_year_date)) %>%
      mutate(period = case_when(
        Date == latest_date ~ "latest",
        Date == prev_year_date ~ "previous"
      )) %>%
      select(Area, period, Value) %>%
      pivot_wider(names_from = period, values_from = Value) %>%
      filter(!is.na(latest), !is.na(previous)) %>%
      mutate(
        `Same Period Last Year` = previous,
        `Latest Period` = latest,
        `% Change` = round(100 * (latest - previous) / previous, 1)
      ) %>%
      select(Area, `Latest Period`, `Same Period Last Year`, `% Change`) %>%
      arrange(desc(`% Change`))
    
    DT::datatable(yoy_data, options = list(pageLength = 15))
  })
  
  # === YOY TB Herds Data Table === # 
  output$yoyTableHerds <- DT::renderDataTable({
    latest_date <- max(TBHerds$Date)
    prev_year_date <- latest_date %m-% years(1)
    
    yoy_data <- TBHerds %>%
      filter(Date %in% c(latest_date, prev_year_date)) %>%
      mutate(period = case_when(
        Date == latest_date ~ "latest",
        Date == prev_year_date ~ "previous"
      )) %>%
      select(Area, period, Value) %>%
      pivot_wider(names_from = period, values_from = Value) %>%
      filter(!is.na(latest), !is.na(previous)) %>%
      mutate(
        `Same Period Last Year` = previous,
        `Latest Period` = latest,
        `% Change` = round(100 * (latest - previous) / previous, 1)
      ) %>%
      select(Area, `Latest Period`, `Same Period Last Year`, `% Change`) %>%
      arrange(desc(`% Change`))
    
    DT::datatable(yoy_data, options = list(pageLength = 15))
  })
  
  # === TB Animals Headline Pivot Table === # 
  output$TBHPivot <- DT::renderDataTable({
    
    piv_data <- TBAniPIV %>%
      select(-Year, -Month) %>%
      arrange(desc(Date)) %>%
      mutate(
        block = ceiling(row_number() / 12),
        Date = as.character(Date)   # <-- force to character
      )
    
    # --- Step 1: compute block subtotals with nicer labels ---
    block_sums <- piv_data %>%
      group_by(block) %>%
      summarise(across(where(is.numeric), sum), .groups = "drop") %>%
      mutate(
        Date = case_when(
          block == 1 ~ "Subtotal – Previous 12 months",
          block == 2 ~ "Subtotal – 13–24 months",
          TRUE       ~ "Subtotal – 25+ months"
        )
      )
    
    # --- Step 2: split both data and subtotals by block ---
    split_data <- split(piv_data, piv_data$block)
    split_sums <- split(block_sums, block_sums$block)
    
    # --- Step 3: bind each block with its subtotal ---
    piv_with_subs <- purrr::map2_dfr(
      split_data,
      split_sums,
      ~ bind_rows(select(.x, -block), select(.y, -block))
    )
    
    # --- Step 4: add % change row (if at least 2 blocks) ---
    # Select numeric columns only for calculation
    numeric_cols <- names(block_sums)[sapply(block_sums, is.numeric)]
    
    # Compute % change only for numeric columns
    if (n_distinct(piv_data$block) >= 2) {
      latest <- block_sums %>% filter(block == 1)
      prior  <- block_sums %>% filter(block == 2)
      
      pct_change <- latest
      pct_change[numeric_cols] <- round(
        100 * (latest[numeric_cols] - prior[numeric_cols]) / prior[numeric_cols],
        1
      )
      pct_change$Date <- "% CHANGE vs PREV 12MTHS"
      
      piv_with_subs <- bind_rows(select(pct_change, -block),piv_with_subs) %>%
        relocate(Date, .before = everything()) 
    }
    
    # --- Step 5: style the table ---
    DT::datatable(
      piv_with_subs,
      options = list(pageLength = 36, dom = 't', ordering=FALSE),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "Date",
        target = "row",
        fontWeight = DT::styleEqual(
          c("Subtotal – Previous 12 months",
            "Subtotal – 13–24 months",
            "Subtotal – 25+ months",
            "% CHANGE vs PREV 12MTHS"),
          c("bold", "bold", "bold", "bold")
        ),
        color = DT::styleEqual(
          c("% CHANGE vs PREV 12MTHS"),
          c("blue")
        ),
        backgroundColor = DT::styleEqual(
          c("Subtotal – Previous 12 months",
            "Subtotal – 13–24 months",
            "Subtotal – 25+ months"),
          c("#f0f0f0", "#f0f0f0", "#f0f0f0")
        )
      )
  })
  
  # === TB Animals Headline Pivot Table === # 
  output$TBAPivot <- DT::renderDataTable({
    
    piv_data <- TBHerdsPIV %>%
      select(-Year, -Month) %>%
      arrange(desc(Date)) %>%
      mutate(
        block = ceiling(row_number() / 12),
        Date = as.character(Date)   # <-- force to character
      )
    
    # --- Step 1: compute block subtotals with nicer labels ---
    block_sums <- piv_data %>%
      group_by(block) %>%
      summarise(across(where(is.numeric), sum), .groups = "drop") %>%
      mutate(
        Date = case_when(
          block == 1 ~ "Subtotal – Previous 12 months",
          block == 2 ~ "Subtotal – 13–24 months",
          TRUE       ~ "Subtotal – 25+ months"
        )
      )
    
    # --- Step 2: split both data and subtotals by block ---
    split_data <- split(piv_data, piv_data$block)
    split_sums <- split(block_sums, block_sums$block)
    
    # --- Step 3: bind each block with its subtotal ---
    piv_with_subs <- purrr::map2_dfr(
      split_data,
      split_sums,
      ~ bind_rows(select(.x, -block), select(.y, -block))
    )
    
    # --- Step 4: add % change row (if at least 2 blocks) ---
    # Select numeric columns only for calculation
    numeric_cols <- names(block_sums)[sapply(block_sums, is.numeric)]
    
    # Compute % change only for numeric columns
    if (n_distinct(piv_data$block) >= 2) {
      latest <- block_sums %>% filter(block == 1)
      prior  <- block_sums %>% filter(block == 2)
      
      pct_change <- latest
      pct_change[numeric_cols] <- round(
        100 * (latest[numeric_cols] - prior[numeric_cols]) / prior[numeric_cols],
        1
      )
      pct_change$Date <- "% CHANGE vs PREV 12MTHS"
      
      piv_with_subs <- bind_rows(select(pct_change, -block),piv_with_subs) %>%
        relocate(Date, .before = everything()) 
    }
    
    # --- Step 5: style the table ---
    DT::datatable(
      piv_with_subs,
      options = list(pageLength = 36, dom = 't', ordering=FALSE),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "Date",
        target = "row",
        fontWeight = DT::styleEqual(
          c("Subtotal – Previous 12 months",
            "Subtotal – 13–24 months",
            "Subtotal – 25+ months",
            "% CHANGE vs PREV 12MTHS"),
          c("bold", "bold", "bold", "bold")
        ),
        color = DT::styleEqual(
          c("% CHANGE vs PREV 12MTHS"),
          c("blue")
        ),
        backgroundColor = DT::styleEqual(
          c("Subtotal – Previous 12 months",
            "Subtotal – 13–24 months",
            "Subtotal – 25+ months"),
          c("#f0f0f0", "#f0f0f0", "#f0f0f0")
        )
      )
  })

}

# Run App
shinyApp(ui, server)