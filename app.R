# Load packages used by the app. Install missing packages, if needed.
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(gitlink)
library(lubridate)
library(plotly)

# Read data from a CSV file and perform data preprocessing
long_data <- read_csv("data/TB_Animal_SummaryLong20250820.csv")
  

# Shiny UI ----
# UI
ui <- fluidPage(
  titlePanel("NI TB Cases Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("areaInput", "Select Area(s):", 
                  choices = unique(long_data$Area), 
                  selected = "Armagh", multiple = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("TB Reactor Animals",
                 plotlyOutput("tsPlot")),
        tabPanel("TB Reactor Animals YoY % Change",
                 plotlyOutput("pctChangePlot"),
                 br(),
                 h4("Raw Counts & % Change Table"),
                 DT::dataTableOutput("yoyTable"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    long_data %>% filter(Area %in% input$areaInput)
  })
  
  # Interactive Time Series Plot
  output$tsPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Date, y = Value, color = Area)) +
      geom_line(size = 1) +
      geom_point() +
      labs(title = "TB Reactor Animals Time Series by Area", x = "Date", y = "Value") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # === % Change Chart ===
  output$pctChangePlot <- renderPlotly({
    latest_date <- max(long_data$Date)
    prev_year_date <- latest_date %m-% years(1)
    
    yoy_data <- long_data %>%
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
        title = paste("TB Reactor Animals YoY % Change —", format(latest_date, "%B %Y")),
        x = "Area", y = "% Change"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  output$yoyTable <- DT::renderDataTable({
    latest_date <- max(long_data$Date)
    prev_year_date <- latest_date %m-% years(1)
    
    yoy_data <- long_data %>%
      filter(Date %in% c(latest_date, prev_year_date)) %>%
      mutate(period = case_when(
        Date == latest_date ~ "latest",
        Date == prev_year_date ~ "previous"
      )) %>%
      select(Area, period, Value) %>%
      pivot_wider(names_from = period, values_from = Value) %>%
      filter(!is.na(latest), !is.na(previous)) %>%
      mutate(
        `Previous Year` = previous,
        `Latest` = latest,
        `% Change` = round(100 * (latest - previous) / previous, 1)
      ) %>%
      select(Area, `Previous Year`, `Latest`, `% Change`) %>%
      arrange(desc(`% Change`))
    
    DT::datatable(yoy_data, options = list(pageLength = 15))
  })

}

# Run App
shinyApp(ui, server)