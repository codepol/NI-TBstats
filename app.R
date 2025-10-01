# Load packages used by the app. Install missing packages, if needed.
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(gitlink)
library(lubridate)
library(plotly)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)

# Read data from CSV files
TBAni <- read_csv("data/TB_Animal_SummaryLong.csv")
TBAniPIV <- read_csv("data/TB_Animal_SummaryPivot.csv")
TBHerds <- read_csv("data/TB_ReactorHerds_SummaryAll.csv")
TBHerdsPIV <- read_csv("data/TB_ReactorHerds_SummaryPivot.csv")
AnnualHP <- read.csv("data/TB_annualHP_Pivot.csv")
AnnualAP <- read.csv("data/TB_annualAP_Pivot.csv")
TBCultPos <- read.csv("data/TB_percTBCult_Pivot.csv")
RollReactors <- read.csv("data/TB_rollingReactor_Pivot.csv")
RollReactors$Date <- as.Date(RollReactors$Date)

refreshDatetime <- format(file.info("data/TB_percTBCult_Pivot.csv")$mtime, "%Y-%m-%d %H:%M")

latest_date <- max(TBAni$Date)
prev_year_date <- latest_date %m-% years(1)
# Define 12-month windows
latest_start <- latest_date %m-% months(11)
prev_year_start <- prev_year_date %m-% months(11)

# Shiny UI ----
# UI
ui <- fluidPage(
  titlePanel("NI TB Cases Dashboard"),
  br(),
  
  h4(paste0("Date Last Data Refresh: ", refreshDatetime)),
  
  tabsetPanel(
    tabPanel("TB Reactor Animals",
             # Area filter just for this tab
             selectInput("areaInputAnimals", "Select Area(s):", 
                         choices = unique(TBAni$Area), 
                         selected = "Armagh", multiple = TRUE),
             br(),
             h3("TB Reactor Animals by Area Time Series"),
             plotlyOutput("tsPlot"),
             br(),
             h3(paste("TB Reactor Animals 12-Month Cumulative Change:",
                      format(latest_start, "%b %Y"), "to", format(latest_date, "%b %Y"),
                      " vs ",
                      format(prev_year_start, "%b %Y"), "to", format(prev_year_date, "%b %Y"))),
             plotlyOutput("pctChangePlot"),
             br(),
             h3("TB Reactor Animals 12-Month Cumulative Change Map"),
             plotlyOutput("tbAniMap", width = "100%", height = "900px"),
             h3("Count of TB Reactor Animals over the past 3 calendar years"),
             DTOutput("TBAPivot")
    ),
    tabPanel("TB Reactor Herds",
             # Area filter just for this tab
             selectInput("areaInputHerds", "Select Area(s):", 
                         choices = unique(TBHerds$Area), 
                         selected = "Armagh", multiple = TRUE),
             br(),
             h3("TB Reactor Herds by Area"),
             plotlyOutput("tsPlotHerds"),
             br(),
             h3(paste("TB Reactor Herds 12-Month Cumulative Change:",
                      format(latest_start, "%b %Y"), "to", format(latest_date, "%b %Y"),
                      " vs ",
                      format(prev_year_start, "%b %Y"), "to", format(prev_year_date, "%b %Y"))),
             plotlyOutput("pctChangePlotHerds"),
             br(),
             h3("TB Reactor Herds 12-Month Cumulative Change Map"),
             plotlyOutput("tbHerdsMap", width = "100%", height = "900px"),
             h3("Count of TB Reactor Herds over the past 3 calendar years"),
             DTOutput("TBHPivot")
    ),
    tabPanel("Annual Herd Prevalence",
             h3("Annual Herd Prevalence 2005 - present"),
             DTOutput("AnnualHP")
    ),
    tabPanel("Annual Animal Prevalence",
             h3("Annual Animal Prevalence 2005 - present"),
             DTOutput("AnnualAP")
    ),
    tabPanel("% Animals Infected Detected Post-Mortem",
             h3("Annual Percentage of animals confirmed as infected and detected at post-mortem and not by skin test 2005 - present"),
             DTOutput("TBCultPos")
    ),
    tabPanel("Number of Reactor Animals per Reactor Herd",
             h3("No of Reactor Animals per Reactor Herd (Rolling 12-months)"),
             plotlyOutput("tsReactPH"),
             h3("Rolling 12-Month Count of Reactor Animals, Reactor Herds and Reactor Animals per Reactor Herd"),
             DTOutput("TBReactPH")
    )
  )
)

# Helper function for color breaks and palette
get_color_breaks_and_palette <- function(df, exclude_cols = "Statistic") {
  # Exclude specified columns for breaks calculation
  numeric_data <- df %>% select(-any_of(exclude_cols))
  brks <- quantile(numeric_data, probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
  list(breaks = brks, colors = clrs)
}

# Server
server <- function(input, output, session) {
  
  # For animals
  filtered_data <- reactive({
    TBAni %>% filter(Area %in% input$areaInputAnimals)
  })
  
  # TB Reactor Animals Time Series Plot
  output$tsPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Date, y = Value, color = Area)) +
      geom_line(linewidth = 1) +
      geom_point() +
      labs(x = "Date", y = "Value") +
      theme_gray() 

    ggplotly(p) %>% layout(legend = list(orientation = "h", y=-0.5))
  })
  
  # For herds
  filtered_data2 <- reactive({
    TBHerds %>% filter(Area %in% input$areaInputHerds)
  })
  
  # TB Reactor Herds Time Series Plot
  output$tsPlotHerds <- renderPlotly({
    p <- ggplot(filtered_data2(), aes(x = Date, y = Value, color = Area)) +
      geom_line(linewidth = 1) +
      geom_point() +
      labs(x = "Date", y = "Value") +
      theme_gray()
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", y=-0.5))
  })
  
  # === TB Reactor Animals 12-Month Cumulative Change Chart ===
  output$pctChangePlot <- renderPlotly({
    latest_date <- max(TBAni$Date)
    prev_year_date <- latest_date %m-% years(1)
    
    # Define 12-month windows
    latest_start <- latest_date %m-% months(11)
    prev_year_start <- prev_year_date %m-% months(11)
    
    yoy_data <- TBAni %>%
      mutate(period = case_when(
        Date >= latest_start & Date <= latest_date ~ "latest",
        Date >= prev_year_start & Date <= prev_year_date ~ "previous"
      )) %>%
      filter(!is.na(period)) %>%
      group_by(Area, period) %>%
      summarise(total = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = period, values_from = total) %>%
      filter(!is.na(latest), !is.na(previous)) %>%
      mutate(
        abs_change = latest - previous,
        pct_change = 100 * (latest - previous) / previous
      )
    
    if (nrow(yoy_data) == 0) {
      message("No data to display for YoY change.")
      return(NULL)
    }
    
    p <- ggplot(yoy_data, aes(
      x = reorder(Area, pct_change), y = abs_change, fill = abs_change > 0,
      text = paste0(
        "Area: ", Area,
        "<br>Latest 12mo: ", latest,
        "<br>Previous 12mo: ", previous,
        "<br>Change: ", abs_change,
        "<br>% Change: ", round(pct_change, 1), "%"
      )
    )) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "forestgreen")) +
      labs(x = "Area", y = "Change in Counts") +
      #theme_gray() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # === TB Reactor Animals 12-Month Cumulative Change Chart ===
  output$pctChangePlotHerds <- renderPlotly({
    latest_date <- max(TBHerds$Date)
    prev_year_date <- latest_date %m-% years(1)
    
    # Define 12-month windows
    latest_start <- latest_date %m-% months(11)
    prev_year_start <- prev_year_date %m-% months(11)
    
    yoy_data <- TBHerds %>%
      mutate(period = case_when(
        Date >= latest_start & Date <= latest_date ~ "latest",
        Date >= prev_year_start & Date <= prev_year_date ~ "previous"
      )) %>%
      filter(!is.na(period)) %>%
      group_by(Area, period) %>%
      summarise(total = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = period, values_from = total) %>%
      filter(!is.na(latest), !is.na(previous)) %>%
      mutate(
        abs_change = latest - previous,
        pct_change = 100 * (latest - previous) / previous
      )
    
    if (nrow(yoy_data) == 0) {
      message("No data to display for YoY change.")
      return(NULL)
    }
    
    p <- ggplot(yoy_data, aes(
      x = reorder(Area, pct_change), y = abs_change, fill = abs_change > 0,
      text = paste0(
        "Area: ", Area,
        "<br>Latest 12mo: ", latest,
        "<br>Previous 12mo: ", previous,
        "<br>Change: ", abs_change,
        "<br>% Change: ", round(pct_change, 1), "%"
      )
    )) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "forestgreen")) +
      labs(x = "Area", y = "Change in Counts") +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # === YOY TB Animal Data Table === #  
  output$yoyTable <- renderDT({
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
    
    datatable(yoy_data, options = list(pageLength = 15))
  })
  
  # === YOY TB Herds Data Table === # 
  output$yoyTableHerds <- renderDT({
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
    
    datatable(yoy_data, options = list(pageLength = 15))
  })
  
  # === TB Herds Headline Pivot Table === # 
  output$TBHPivot <- renderDT({
    
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
    datatable(
      piv_with_subs,
      options = list(pageLength = 36, dom = 't', ordering=FALSE),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Date",
        target = "row",
        fontWeight = styleEqual(
          c("Subtotal – Previous 12 months",
            "Subtotal – 13–24 months",
            "Subtotal – 25+ months",
            "% CHANGE vs PREV 12MTHS"),
          c("bold", "bold", "bold", "bold")
        ),
        color = styleEqual(
          c("% CHANGE vs PREV 12MTHS"),
          c("blue")
        ),
        backgroundColor = styleEqual(
          c("Subtotal – Previous 12 months",
            "Subtotal – 13–24 months",
            "Subtotal – 25+ months"),
          c("#f0f0f0", "#f0f0f0", "#f0f0f0")
        )
      )
  })
  
  # === TB Animals Headline Pivot Table === # 
  output$TBAPivot <- renderDT({
    
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
    datatable(
      piv_with_subs,
      options = list(pageLength = 36, dom = 't', ordering=FALSE),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Date",
        target = "row",
        fontWeight = styleEqual(
          c("Subtotal – Previous 12 months",
            "Subtotal – 13–24 months",
            "Subtotal – 25+ months",
            "% CHANGE vs PREV 12MTHS"),
          c("bold", "bold", "bold", "bold")
        ),
        color = styleEqual(
          c("% CHANGE vs PREV 12MTHS"),
          c("blue")
        ),
        backgroundColor = styleEqual(
          c("Subtotal – Previous 12 months",
            "Subtotal – 13–24 months",
            "Subtotal – 25+ months"),
          c("#f0f0f0", "#f0f0f0", "#f0f0f0")
        )
      )
  })
  
  # === Annual Herd Prevalence ===
  output$AnnualHP <- renderDT({ 
    piv_data <- AnnualHP %>%
      mutate(across(where(is.numeric), round, digits=2))
    
    color_info <- get_color_breaks_and_palette(piv_data, exclude_cols = "Statistic")
    
    datatable(piv_data, options = list(pageLength = 25, dom='t', ordering=FALSE), rownames=FALSE) %>%
      formatStyle(names(piv_data), backgroundColor = styleInterval(color_info$breaks, color_info$colors))
  })
  
  # === Annual Animal Prevalence ===
  output$AnnualAP <- renderDT({ 
    piv_data <- AnnualAP %>%
      mutate(across(where(is.numeric), round, digits=2))
    
    color_info <- get_color_breaks_and_palette(piv_data, exclude_cols = "Statistic")
    
    datatable(piv_data, options = list(pageLength = 25, dom='t', ordering=FALSE), rownames=FALSE) %>%
      formatStyle(names(piv_data), backgroundColor = styleInterval(color_info$breaks, color_info$colors))
  })
  
  # === Percentage of animals confirmed as infected and detected at post-mortem and not by skin test ===
  output$TBCultPos <- renderDT({ 
    piv_data <- TBCultPos %>%
      mutate(across(where(is.numeric), round, digits=2))
    
    color_info <- get_color_breaks_and_palette(piv_data, exclude_cols = "Statistic")
    
    datatable(piv_data, options = list(pageLength = 25, dom='t', ordering=FALSE), rownames=FALSE) %>%
      formatStyle(names(piv_data), backgroundColor = styleInterval(color_info$breaks, color_info$colors))
  })
  
  # === Overall No of Reactor Animals per Reactor Herd (Rolling 12-months) ===
  output$tsReactPH <- renderPlotly({
    p <- ggplot(RollReactors, aes(x = Date, y = ReactorsPerHerd)) +
      geom_line(linewidth = 1) + geom_point() +
      labs(x = "Date", y = "ReactorsPerHerd") + 
      ylim(0, max(RollReactors$ReactorsPerHerd, na.rm = TRUE)) 

    p
  })
  
  # === Overall No of Reactor Animals per Reactor Herd (Rolling 12-months) Datatable ===
  output$TBReactPH <- renderDT({ 
    piv_data <- RollReactors %>% arrange(desc(Date))
    
    datatable(piv_data, options = list(pageLength = 24, dom='top', ordering=TRUE), rownames=FALSE) %>%
      formatRound(columns="ReactorsPerHerd", digits=4) %>%
      formatCurrency(columns=c("RollingTotal_Animals","RollingTotal_Herds"), currency="", interval=3, digits=0, mark=",") %>%
      formatStyle(names(piv_data))
    
  })
  
  # === TB Reactor Animals Map ===
  output$tbAniMap <- renderPlotly({
    latest_date <- max(TBAni$Date)
    prev_year_date <- latest_date %m-% years(1)
    
    # Define 12-month windows
    latest_start <- latest_date %m-% months(11)
    prev_year_start <- prev_year_date %m-% months(11)
    
    # Compute 12-month totals
    yoy_data <- TBAni %>%
      mutate(period = case_when(
        Date >= latest_start & Date <= latest_date ~ "latest",
        Date >= prev_year_start & Date <= prev_year_date ~ "previous"
      )) %>%
      filter(!is.na(period)) %>%
      group_by(Area, period) %>%
      summarise(total = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = period, values_from = total) %>%
      filter(!is.na(latest), !is.na(previous)) %>%
      mutate(
        abs_change = latest - previous,
        pct_change = 100 * (latest - previous) / previous
      )
    
    # Compute most recent month counts
    latest_month_data <- TBAni %>%
      filter(Date == latest_date) %>%
      group_by(Area) %>%
      summarise(latest_month_count = sum(Value, na.rm = TRUE), .groups = "drop")
    
    # Load coordinates
    dvo_shapes <- st_read("data/dvo_regions_main.geojson")
    
    # Join with TB dataset
    map_data <- dvo_shapes %>%
      left_join(yoy_data, by = "Area")
    
    map_data <- map_data[complete.cases(map_data$Area), ]
    
    map_data %>%
      select(Area, latest, previous, pct_change) %>%
      as.data.frame()
    
    if (nrow(map_data) == 0) {
      message("No data to display on map.")
      return(NULL)
    }
    
    # --- colour palette: prefer viridis (colour-blind friendly) ---
    if (requireNamespace("viridisLite", quietly = TRUE)) {
      colours_for_plot <- viridisLite::viridis(100)
    } else {
      # fallback: a simple diverging ramp (readable even without viridis)
      colours_for_plot <- colorRampPalette(c("#2166ac","#f7f7f7","#b2182b"))(100)
    }
    
    # Use the saved GeoJSON file
    geojson_obj <- jsonlite::fromJSON("data/dvo_regions_main.geojson", simplifyVector = FALSE)
    
    p <- plot_ly(
      type = "choroplethmapbox",
      geojson = geojson_obj,
      locations = map_data$Area,             # ✅ use joined data
      z = map_data$pct_change,               # ✅ same source
      featureidkey = "properties.Area",
      text = ~paste0(
        "<b>", map_data$Area, "</b><br>",
        "12mo Latest: ", map_data$latest, "<br>",
        "12mo Previous: ", map_data$previous, "<br>",
        "Change: ", map_data$abs_change, "<br>",
        "% Change: ", round(map_data$pct_change, 1), "%"
      ),
      hoverinfo = "text",
      colorscale = "Viridis",
      marker = list(line = list(width = 0.5, color = "black"))
      ) %>%
      layout(
        mapbox = list(
          style = "carto-positron",
          zoom = 8,
          center = list(lat = 54.6, lon = -6.7)
        ),
        title = paste("TB Reactor Animals % Change by DVO Region ( 12 months to", format(latest_date, "%B %Y"),")")
      )
    
    p
  })
  
  # === TB Reactor Herds Map ===
  output$tbHerdsMap <- renderPlotly({
    latest_date <- max(TBHerds$Date)
    prev_year_date <- latest_date %m-% years(1)
    
    # Define 12-month windows
    latest_start <- latest_date %m-% months(11)
    prev_year_start <- prev_year_date %m-% months(11)
    
    # Compute 12-month totals
    yoy_data <- TBHerds %>%
      mutate(period = case_when(
        Date >= latest_start & Date <= latest_date ~ "latest",
        Date >= prev_year_start & Date <= prev_year_date ~ "previous"
      )) %>%
      filter(!is.na(period)) %>%
      group_by(Area, period) %>%
      summarise(total = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = period, values_from = total) %>%
      filter(!is.na(latest), !is.na(previous)) %>%
      mutate(
        abs_change = latest - previous,
        pct_change = 100 * (latest - previous) / previous
      )
    
    # Compute most recent month counts
    latest_month_data <- TBHerds %>%
      filter(Date == latest_date) %>%
      group_by(Area) %>%
      summarise(latest_month_count = sum(Value, na.rm = TRUE), .groups = "drop")
    
    # Load coordinates
    dvo_shapes <- st_read("data/dvo_regions_main.geojson")
    
    # Join with TB dataset
    map_data <- dvo_shapes %>%
      left_join(yoy_data, by = "Area")
    
    map_data <- map_data[complete.cases(map_data$Area), ]
    
    map_data %>%
      select(Area, latest, previous, pct_change) %>%
      as.data.frame()
    
    if (nrow(map_data) == 0) {
      message("No data to display on map.")
      return(NULL)
    }
    
    # --- colour palette: prefer viridis (colour-blind friendly) ---
    if (requireNamespace("viridisLite", quietly = TRUE)) {
      colours_for_plot <- viridisLite::viridis(100)
    } else {
      # fallback: a simple diverging ramp (readable even without viridis)
      colours_for_plot <- colorRampPalette(c("#2166ac","#f7f7f7","#b2182b"))(100)
    }
    
    # Use the saved GeoJSON file
    geojson_obj <- jsonlite::fromJSON("data/dvo_regions_main.geojson", simplifyVector = FALSE)
    
    p <- plot_ly(
      type = "choroplethmapbox",
      geojson = geojson_obj,
      locations = map_data$Area,             # ✅ use joined data
      z = map_data$pct_change,               # ✅ same source
      featureidkey = "properties.Area",
      text = ~paste0(
        "<b>", map_data$Area, "</b><br>",
        "12mo Latest: ", map_data$latest, "<br>",
        "12mo Previous: ", map_data$previous, "<br>",
        "Change: ", map_data$abs_change, "<br>",
        "% Change: ", round(map_data$pct_change, 1), "%"
      ),
      hoverinfo = "text",
      colorscale = "Viridis",
      marker = list(line = list(width = 0.5, color = "black"))
    ) %>%
      layout(
        mapbox = list(
          style = "carto-positron",
          zoom = 8,
          center = list(lat = 54.6, lon = -6.7)
        ),
        title = paste("TB Reactor Herds % Change by DVO Region ( 12 months to", format(latest_date, "%B %Y"),")")
      )
    
    p
  })

}

# Run App
shinyApp(ui, server)