# week_schedule.R
library(shiny)
library(dplyr)
library(lubridate)
library(plotly)
library(scales)
library(readr)
library(ggplot2)
library(RColorBrewer)

# Load in data
read_predictions <- function(path = "test_with_predictions.csv") {
  if (!file.exists(path)) stop("Can't find 'test_with_predictions.csv'")
  df <- readr::read_csv(path, show_col_types = FALSE) %>%
    mutate(
      appt_datetime = lubridate::as_datetime(appt_time),
      provider      = as.factor(provider_id)
    )
  stopifnot("appt_datetime" %in% names(df), "prob_no_show" %in% names(df))
  df
}

raw <- read_predictions()

# Always start at this week (Mon)
default_week_start <- as.Date("2023-05-08")

# Aggregate per hour
compute_nsc_by_hour <- function(dat) {
  dat %>%
    mutate(
      hour_block = floor_date(appt_datetime, "hour"),
      date       = as.Date(hour_block),
      hour_num   = hour(hour_block)
    ) %>%
    group_by(date, hour_num, hour_block) %>%
    summarise(
      n_appts = n(),
      avg_p   = mean(prob_no_show),
      nsc     = { p <- pmin(pmax(prob_no_show, 1e-6), 1 - 1e-6)
      1 - exp(sum(log1p(-p))) },     # NSC = P(X>=1) = 1 - Π(1-p)
      .groups = "drop"
    ) %>%
    mutate(
      wday_lab = factor(weekdays(date),
                        levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
    )
}

# ui
ui <- fluidPage(
  titlePanel("Overbooking Planner — Week → Day Drill"),
  sidebarLayout(
    sidebarPanel(
      dateInput("week_start", "Week starting (Mon):", value = default_week_start),
      sliderInput("thr", "NSC threshold (red if ≥):", min = 0, max = 1, value = 0.6, step = 0.05),
      uiOutput("day_picker_help"),
      helpText("Red = P(≥1 no-show in the hour) ≥ threshold → Consider overbooking this hour.")
    ),
    mainPanel(
      h4("Week overview (click a point to drill into a day)"),
      plotlyOutput("week_lines", height = 300),
      h4(textOutput("day_title")),
      plotlyOutput("day_bars", height = 340)
    )
  )
)

server <- function(input, output, session){
  
  # Recompute week window
  week_data <- reactive({
    ws <- floor_date(input$week_start, "week", week_start = 1)
    we <- ws + days(6)
    compute_nsc_by_hour(
      raw %>% filter(as.Date(appt_datetime) >= ws, as.Date(appt_datetime) <= we)
    )
  })
  
  # Selected day (init to week's first day)
  selected_day <- reactiveVal(default_week_start)
  
  output$day_picker_help <- renderUI({
    d_choices <- unique(week_data()$date)
    if (length(d_choices) > 0 && !selected_day() %in% d_choices) {
      selected_day(min(d_choices))
    }
    helpText(paste0("Selected day: ",
                    ifelse(length(d_choices) > 0, as.character(selected_day()), "—")))
  })
  
  # Week lineplot (NSC is the metric; tooltip uses same condition as bars)
  output$week_lines <- renderPlotly({
    d <- week_data()
    req(nrow(d) > 0)
    
    thr <- input$thr
    d <- d %>%
      mutate(
        advise = ifelse(nsc >= thr, "Consider overbooking this hour", "Do not overbook"),
        hover_txt = paste0(
          "Day: ", as.character(date),
          "<br>Hour: ", hour_num, ":00",
          "<br>NSC: ", scales::percent(nsc, accuracy = 0.1),
          "<br>Appts: ", n_appts,
          "<br>Avg p(no-show): ", scales::percent(avg_p, accuracy = 0.1),
          "<br><b>", advise, "</b>"
        )
      )
    
    plot_ly(
      data = d,
      x = ~hour_num, y = ~nsc, color = ~wday_lab,
      colors = RColorBrewer::brewer.pal(7, "Set2"),
      type = "scatter", mode = "lines+markers",
      source = "week", customdata = ~date,
      text = ~hover_txt, hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Hour", dtick = 1),
        yaxis = list(title = "NSC (≥1 no-show)", tickformat = ".0%", range = c(0,1)),
        legend = list(
          title = list(text = "Weekday"),
          orientation = "h", x = 0, y = 1.12, xanchor = "left", yanchor = "bottom"
        ),
        margin = list(b = 70, t = 40)
      )
  })
  
  # Click to drill into a day
  observeEvent(plotly::event_data("plotly_click", source = "week"), {
    ev <- plotly::event_data("plotly_click", source = "week")
    if (!is.null(ev$customdata)) selected_day(as.Date(ev$customdata))
  })
  
  output$day_title <- renderText({
    paste("Day detail —", selected_day(), "(bars show NSC by hour)")
  })
  
  # Day bars (color & text both use NSC ≥ threshold → consistent)
  output$day_bars <- renderPlotly({
    d <- week_data() %>% filter(date == selected_day())
    req(nrow(d) > 0)
    
    thr <- input$thr
    d <- d %>%
      arrange(hour_num) %>%
      mutate(
        Risk  = ifelse(nsc >= thr, "High (≥ threshold)", "Low (< threshold)"),
        advise = ifelse(nsc >= thr, "Consider overbooking this hour", "Do not overbook"),
        tip   = paste0(
          "Hour starting ", hour_num, ":00",
          "<br>NSC: ", scales::percent(nsc, accuracy = 0.1),
          "<br>Appointments: ", n_appts,
          "<br>Avg p(no-show): ", scales::percent(avg_p, accuracy = 0.1),
          "<br><b>", advise, "</b>"
        )
      )
    
    p <- ggplot(d, aes(x = factor(hour_num), y = nsc, fill = Risk, text = tip)) +
      geom_col(width = 0.75) +
      geom_hline(yintercept = thr, linetype = "dashed") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      scale_fill_manual(
        values = c("High (≥ threshold)" = "#e53935", "Low (< threshold)" = "#66bb6a"),
        breaks = c("High (≥ threshold)", "Low (< threshold)"),
        labels = c("High (≥ threshold) — consider overbooking", "Low (< threshold)")
      ) +
      labs(x = "Hour", y = "NSC (probability ≥1 no-show)", fill = NULL) +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest")
  })
}

shinyApp(ui, server)
