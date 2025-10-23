# app.R
# Overbooking Planner with Week & Day views
# Assumes you have a CSV with per‑appointment predictions, e.g. test_with_predictions.csv
# Required columns (minimum):
#   appt_time (datetime), provider_id, address, age, specialty,
#   prob_no_show (numeric in [0,1]), pred_no_show (0/1)

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)

# ---- helper: safe read ----
read_predictions <- function(path = "test_with_predictions.csv") {
  if (!file.exists(path)) {
    stop(sprintf("File '%s' not found. Place your predictions CSV in the app directory.", path))
  }
  df <- readr::read_csv(path, show_col_types = FALSE)
  # Basic normalization
  df <- df %>% mutate(
    appt_time   = as_datetime(appt_time),
    provider_id = as.factor(provider_id),
    specialty   = as.factor(specialty),
    address     = as.factor(address),
    prob_no_show = pmin(pmax(prob_no_show, 0), 1),
    pred_no_show = as.integer(pred_no_show)
  )
  df
}

ui <- fluidPage(
  titlePanel("Clinic Overbooking Planner — No‑Show Aware"),
  sidebarLayout(
    sidebarPanel(
      dateInput("week_start", "Select week (Mon start):", value = Sys.Date()),
      selectInput("provider_filter", "Provider(s)", choices = NULL, multiple = TRUE),
      sliderInput("risk_cap", "Max risk per hour (expected missed)", min = 0, max = 10, value = 3, step = 1),
      sliderInput("risk_weight", "Emphasis on high‑risk appts (0=even, 1=heavily weight)", min = 0, max = 1, value = 0.5, step = 0.1),
      sliderInput("min_prob", "Minimum prob to count toward expected missed", min = 0, max = 1, value = 0.2, step = 0.05),
      helpText("Recommendation = floor( expected missed (filtered by min_prob) ), capped by 'Max risk per hour'."),
      width = 3
    ),
    mainPanel(
      tabsetPanel(id = "viewtabs",
                  tabPanel("Week View",
                           br(),
                           plotlyOutput("week_bar", height = 360),
                           br(),
                           DTOutput("week_table")
                  ),
                  tabPanel("Day View",
                           br(),
                           uiOutput("day_picker"),
                           br(),
                           plotlyOutput("day_timeline", height = 360),
                           br(),
                           DTOutput("day_table")
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  df <- read_predictions()
  
  # Populate provider choices
  observe({
    updateSelectInput(session, "provider_filter",
                      choices = levels(df$provider_id),
                      selected = levels(df$provider_id)
    )
  })
  
  # ---- Week filter ----
  week_data <- reactive({
    req(input$week_start)
    start <- floor_date(input$week_start, unit = "week", week_start = 1)
    end   <- start + days(7)
    out <- df %>% filter(appt_time >= start, appt_time < end)
    if (!is.null(input$provider_filter) && length(input$provider_filter) > 0) {
      out <- out %>% filter(provider_id %in% input$provider_filter)
    }
    out
  })
  
  # Expected missed per hour (filtered by min_prob and weighted)
  week_hour_summary <- reactive({
    wd <- week_data()
    w <- input$risk_weight
    minp <- input$min_prob
    
    wd %>%
      mutate(
        hour_block = floor_date(appt_time, unit = "hour"),
        contrib = if_else(prob_no_show >= minp, (1 - (1 - prob_no_show)^(1 + 4*w)), 0)
        # contrib increases with risk_weight; w=0 -> approx prob; w=1 -> steeper weighting
      ) %>%
      group_by(hour_block) %>%
      summarize(
        expected_missed = sum(contrib, na.rm = TRUE),
        n_appts = n(),
        .groups = "drop"
      ) %>%
      mutate(recommended_overbook = pmin(floor(expected_missed), input$risk_cap))
  })
  
  output$week_bar <- renderPlotly({
    hs <- week_hour_summary()
    validate(need(nrow(hs) > 0, "No appointments in selected week/providers."))
    
    p <- ggplot(hs, aes(x = hour_block)) +
      geom_col(aes(y = expected_missed)) +
      geom_point(aes(y = recommended_overbook), size = 2) +
      labs(x = "Hour", y = "Expected missed (bars) / Recommended overbook (points)")
    
    ggplotly(p, tooltip = c("x","y"))
  })
  
  output$week_table <- renderDT({
    hs <- week_hour_summary()
    datatable(
      hs %>% arrange(hour_block) %>%
        mutate(
          hour = format(hour_block, "%a %b %d %H:00"),
          .before = 1
        ) %>% select(hour, n_appts, expected_missed, recommended_overbook),
      options = list(pageLength = 15), rownames = FALSE
    )
  })
  
  # ---- Day View ----
  output$day_picker <- renderUI({
    days <- unique(as_date(week_data()$appt_time)) %>% sort()
    if (length(days) == 0) return(helpText("No days available in this week."))
    selectInput("day", "Choose a day:", choices = days, selected = days[1])
  })
  
  day_data <- reactive({
    req(input$day)
    wd <- week_data()
    wd %>% filter(as_date(appt_time) == as_date(input$day)) %>% arrange(appt_time)
  })
  
  day_hour_summary <- reactive({
    dd <- day_data()
    w <- input$risk_weight
    minp <- input$min_prob
    dd %>%
      mutate(hour_block = floor_date(appt_time, "hour"),
             contrib = if_else(prob_no_show >= minp, (1 - (1 - prob_no_show)^(1 + 4*w)), 0)) %>%
      group_by(hour_block) %>%
      summarize(expected_missed = sum(contrib), n_appts = n(), .groups = "drop") %>%
      mutate(recommended_overbook = pmin(floor(expected_missed), input$risk_cap))
  })
  
  output$day_timeline <- renderPlotly({
    dd <- day_data()
    validate(need(nrow(dd) > 0, "No appointments on this day."))
    
    dd <- dd %>% mutate(
      start = floor_date(appt_time, unit = "minute"),
      end   = start + minutes(30), # assume 30‑min slots for visualization; adjust if you know true duration
      hour_block = floor_date(appt_time, "hour")
    )
    
    p <- ggplot(dd, aes(y = provider_id)) +
      geom_segment(aes(x = start, xend = end, yend = provider_id)) +
      geom_point(aes(x = start, size = prob_no_show)) +
      labs(x = "Time", y = "Provider", size = "Prob no‑show")
    
    ggplotly(p, tooltip = c("x","y","size"))
  })
  
  output$day_table <- renderDT({
    dd <- day_data()
    hs <- day_hour_summary()
    
    rec_map <- hs %>% select(hour_block, recommended_overbook)
    
    dd %>%
      mutate(
        hour_block = floor_date(appt_time, "hour"),
        recommended_overbook = rec_map$recommended_overbook[match(hour_block, rec_map$hour_block)]
      ) %>%
      transmute(
        time = format(appt_time, "%H:%M"),
        provider_id, specialty, address, age,
        prob_no_show = round(prob_no_show, 3),
        pred_no_show,
        recommended_overbook
      ) %>%
      datatable(options = list(pageLength = 25), rownames = FALSE)
  })
}

shinyApp(ui, server)
