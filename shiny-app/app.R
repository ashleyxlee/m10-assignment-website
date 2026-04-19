# app.R
# TRI Conversion Analytics - Local GA4 RDS version
# Uses ga4_events_28d.rds instead of simulated data / BigQuery

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(lubridate)
library(forcats)
library(broom)

# ============================================================
# 0. DATA PREP
# ============================================================

ga4_events_28d <- readRDS("ga4_events_28d.rds") |>
  mutate(
    event_date = as.Date(event_date, format = "%Y%m%d"),
    source = ifelse(is.na(source) | source == "", "Unknown", source),
    medium = ifelse(is.na(medium) | medium == "", "Unknown", medium),
    campaign_name = ifelse(is.na(campaign_name) | campaign_name == "", "Unknown", campaign_name),
    device_category = ifelse(is.na(device_category) | device_category == "", "Unknown", device_category),
    country = ifelse(is.na(country) | country == "", "Unknown", country),
    city = ifelse(is.na(city) | city == "", "Unknown", city),
    region = ifelse(is.na(region) | region == "", "Unknown", region),
    user_pseudo_id = ifelse(is.na(user_pseudo_id) | user_pseudo_id == "", NA_character_, user_pseudo_id)
  ) |>
  arrange(event_ts)

# Fallback pseudo-user key because user_pseudo_id appears mostly missing
ga4_events_28d <- ga4_events_28d |>
  mutate(
    user_key = dplyr::case_when(
      !is.na(user_pseudo_id) ~ paste0("uid_", user_pseudo_id),
      TRUE ~ paste("proxy", device_category, country, region, city, sep = "_")
    )
  )

# Create pseudo-sessions using 30-minute inactivity rule
ga4_events_28d <- ga4_events_28d |>
  group_by(user_key) |>
  arrange(event_ts, .by_group = TRUE) |>
  mutate(
    time_diff_mins = as.numeric(difftime(event_ts, lag(event_ts), units = "mins")),
    new_session = ifelse(is.na(time_diff_mins) | time_diff_mins > 30, 1L, 0L),
    session_seq = cumsum(new_session)
  ) |>
  ungroup()

# Session-level table expected by the dashboard
df_sessions <- ga4_events_28d |>
  group_by(user_key, session_seq) |>
  summarise(
    session_date           = as.Date(min(event_ts, na.rm = TRUE)),
    session_start          = min(event_ts, na.rm = TRUE),
    session_end            = max(event_ts, na.rm = TRUE),
    device_category        = first(device_category),
    traffic_source         = first(source),
    medium                 = first(medium),
    campaign_name          = first(campaign_name),
    country                = first(country),
    region                 = first(region),
    city                   = first(city),
    page_views             = sum(event_name == "page_view", na.rm = TRUE),
    training_page_view     = as.integer(any(event_name == "training_page_view")),
    add_to_cart            = as.integer(any(event_name %in% c("add_to_cart", "begin_checkout"))),
    is_booking_conversion  = as.integer(any(event_name == "checkout_started")),
    is_donation_conversion = as.integer(any(event_name == "donate_button_click")),
    is_email_conversion    = as.integer(any(event_name == "newsletter_signup")),
    total_revenue          = sum(purchase_revenue, na.rm = TRUE),
    engagement_sec         = as.numeric(difftime(max(event_ts, na.rm = TRUE), min(event_ts, na.rm = TRUE), units = "secs")),
    .groups = "drop"
  ) |>
  filter(page_views > 0, page_views <= 50) |>
  mutate(
    engagement_sec = ifelse(is.na(engagement_sec) | !is.finite(engagement_sec), 0, engagement_sec),
    is_returning_user = 0L,  # cannot reliably compute from this export
    is_any_conversion = as.integer(
      is_booking_conversion == 1 |
        is_donation_conversion == 1 |
        is_email_conversion == 1
    ),
    page_view_bucket = factor(
      case_when(
        page_views == 1  ~ "1 page",
        page_views <= 3  ~ "2–3 pages",
        page_views <= 6  ~ "4–6 pages",
        page_views <= 15 ~ "7–15 pages",
        TRUE             ~ "16–50 pages"
      ),
      levels = c("1 page", "2–3 pages", "4–6 pages", "7–15 pages", "16–50 pages")
    ),
    device_category = factor(
      device_category,
      levels = c("desktop", "mobile", "tablet")
    ),
    traffic_source = as.character(traffic_source)
  )

date_min <- min(df_sessions$session_date, na.rm = TRUE)
date_max <- max(df_sessions$session_date, na.rm = TRUE)

source_choices <- sort(unique(df_sessions$traffic_source))
device_choices <- c("desktop", "mobile", "tablet")
device_choices <- device_choices[device_choices %in% unique(as.character(df_sessions$device_category))]

sessions_n    <- nrow(df_sessions)
conversions_n <- sum(df_sessions$is_booking_conversion, na.rm = TRUE)
baseline_rate <- 100 * mean(df_sessions$is_booking_conversion, na.rm = TRUE)
avg_pages     <- mean(df_sessions$page_views, na.rm = TRUE)
median_pages  <- median(df_sessions$page_views, na.rm = TRUE)

# ============================================================
# 0.1 THEME / PALETTES
# ============================================================

PAL_DEVICE <- c("desktop" = "#2C2C2C", "mobile" = "#777777", "tablet" = "#B8B8B8")

PAL_CONV <- c(
  "Booking" = "#1A1A1A",
  "Donation" = "#666666",
  "Newsletter" = "#AAAAAA"
)

source_levels <- sort(unique(df_sessions$traffic_source))
grey_vals <- seq(0.15, 0.85, length.out = max(length(source_levels), 1))
PAL_SOURCE <- setNames(gray(grey_vals), source_levels)

PLOT_THEME <- theme_minimal(base_size = 12) +
  theme(
    text               = element_text(family = "sans", color = "#222222"),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 10, color = "#555555"),
    plot.caption       = element_text(size = 8, color = "#888888", hjust = 0),
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(color = "#EEEEEE"),
    panel.grid.major.x = element_blank(),
    axis.text          = element_text(color = "#444444"),
    legend.position    = "bottom",
    legend.title       = element_blank()
  )

# ============================================================
# 1. UI
# ============================================================

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = span(
      img(
        src = "https://www.traumaresourceinstitute.com/wp-content/uploads/2021/07/TRI-Logo.png",
        height = 28,
        style = "margin-right:8px; vertical-align:middle;"
      ),
      "TRI Conversion Analytics"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 240,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview",          tabName = "overview", icon = icon("chart-line")),
      menuItem("Conversion Funnel", tabName = "funnel",   icon = icon("filter")),
      menuItem("Page Depth",        tabName = "depth",    icon = icon("layer-group")),
      menuItem("Device Analysis",   tabName = "device",   icon = icon("mobile-screen")),
      menuItem("Traffic Sources",   tabName = "sources",  icon = icon("arrow-pointer")),
      menuItem("Session Explorer",  tabName = "explorer", icon = icon("table")),
      menuItem("Model Outputs",     tabName = "model",    icon = icon("brain"))
    ),
    hr(),
    div(
      style = "padding: 0 14px;",
      h5(
        "Global Filters",
        style = "color:#AAAAAA; font-size:11px; text-transform:uppercase; letter-spacing:1px;"
      ),
      dateRangeInput(
        "date_range", "Date Range",
        start = date_min,
        end   = date_max,
        min   = date_min,
        max   = date_max
      ),
      checkboxGroupInput(
        "device_filter", "Device",
        choices  = setNames(device_choices, tools::toTitleCase(device_choices)),
        selected = device_choices
      ),
      checkboxGroupInput(
        "source_filter", "Traffic Source",
        choices  = setNames(source_choices, tools::toTitleCase(source_choices)),
        selected = source_choices
      )
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #F7F7F7; }
      .box { border-top: 3px solid #2C2C2C; }
      .small-box { border-radius: 4px; }
      .small-box .icon { opacity: 0.15; }
      .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #2C2C2C; }
      .value-box-label { font-size: 11px; text-transform: uppercase; letter-spacing: 1px; }
      body { font-family: 'Source Sans Pro', sans-serif; }
      .sidebar-menu li a { font-size: 13px; }
      .dataTables_wrapper { font-size: 12px; }
    "))),
    
    tabItems(
      
      tabItem(
        "overview",
        fluidRow(
          valueBoxOutput("vbox_sessions",   width = 2),
          valueBoxOutput("vbox_booking",    width = 2),
          valueBoxOutput("vbox_donation",   width = 2),
          valueBoxOutput("vbox_email",      width = 2),
          valueBoxOutput("vbox_any",        width = 2),
          valueBoxOutput("vbox_mobile_gap", width = 2)
        ),
        fluidRow(
          box(
            title = "Daily Sessions and Conversions",
            width = 8, solidHeader = TRUE, status = "primary",
            plotlyOutput("plot_daily_trend", height = 300)
          ),
          box(
            title = "Conversion Mix by Type",
            width = 4, solidHeader = TRUE, status = "primary",
            plotlyOutput("plot_conv_mix", height = 300)
          )
        ),
        fluidRow(
          box(
            title = "Session Engagement Distribution",
            width = 6, solidHeader = TRUE,
            plotlyOutput("plot_engagement_hist", height = 260)
          ),
          box(
            title = "Source-Level Booking Conversion Rates",
            width = 6, solidHeader = TRUE,
            plotlyOutput("plot_new_vs_return", height = 260)
          )
        )
      ),
      
      tabItem(
        "funnel",
        fluidRow(
          box(
            title = "Booking Conversion Funnel",
            width = 6, solidHeader = TRUE, status = "primary",
            plotlyOutput("plot_booking_funnel", height = 340)
          ),
          box(
            title = "All Conversion Rates by Type",
            width = 6, solidHeader = TRUE, status = "primary",
            plotlyOutput("plot_conv_bars", height = 340)
          )
        ),
        fluidRow(
          box(
            title = "Weekly Conversion Rate Trend",
            width = 12, solidHeader = TRUE,
            plotlyOutput("plot_weekly_conv", height = 280)
          )
        )
      ),
      
      tabItem(
        "depth",
        fluidRow(
          box(
            title = "Conversion Rate by Page View Depth",
            width = 8, solidHeader = TRUE, status = "primary",
            plotlyOutput("plot_depth_bar", height = 340)
          ),
          box(
            title = "Page View Depth Table",
            width = 4, solidHeader = TRUE,
            DTOutput("table_depth")
          )
        ),
        fluidRow(
          box(
            title = "Session Count Distribution (1–15 Pages)",
            width = 6, solidHeader = TRUE,
            plotlyOutput("plot_pv_hist", height = 260)
          ),
          box(
            title = "Training Page View vs. Conversion",
            width = 6, solidHeader = TRUE,
            plotlyOutput("plot_training_pv", height = 260)
          )
        )
      ),
      
      tabItem(
        "device",
        fluidRow(
          box(
            title = "Conversion Rate by Device",
            width = 6, solidHeader = TRUE, status = "primary",
            plotlyOutput("plot_device_conv", height = 320)
          ),
          box(
            title = "Session Volume by Device",
            width = 6, solidHeader = TRUE,
            plotlyOutput("plot_device_vol", height = 320)
          )
        ),
        fluidRow(
          box(
            title = "Page Views Distribution by Device",
            width = 6, solidHeader = TRUE,
            plotlyOutput("plot_device_pv_box", height = 280)
          ),
          box(
            title = "Engagement Time by Device",
            width = 6, solidHeader = TRUE,
            plotlyOutput("plot_device_eng", height = 280)
          )
        )
      ),
      
      tabItem(
        "sources",
        fluidRow(
          box(
            title = "Conversion Rate by Traffic Source",
            width = 7, solidHeader = TRUE, status = "primary",
            plotlyOutput("plot_source_conv", height = 320)
          ),
          box(
            title = "Session Volume by Source",
            width = 5, solidHeader = TRUE,
            plotlyOutput("plot_source_vol", height = 320)
          )
        ),
        fluidRow(
          box(
            title = "Source x Device Heatmap",
            width = 12, solidHeader = TRUE,
            plotlyOutput("plot_source_device_heat", height = 280)
          )
        )
      ),
      
      tabItem(
        "explorer",
        fluidRow(
          box(
            title = "Session-Level Data Explorer",
            width = 12, solidHeader = TRUE,
            p(
              style = "color:#888; font-size:12px; margin-bottom:8px;",
              "Built from local GA4 event data using a 30-minute sessionization rule."
            ),
            DTOutput("table_sessions")
          )
        )
      ),
      
      tabItem(
        "model",
        fluidRow(
          box(
            title = "Logistic Regression: Odds Ratios (95% CI)",
            width = 7, solidHeader = TRUE, status = "primary",
            plotlyOutput("plot_or_forest", height = 360)
          ),
          box(
            title = "Model Summary",
            width = 5, solidHeader = TRUE,
            verbatimTextOutput("model_summary_text")
          )
        ),
        fluidRow(
          box(
            title = "Random Forest Variable Importance",
            width = 6, solidHeader = TRUE,
            plotlyOutput("plot_rf_importance", height = 300)
          ),
          box(
            title = "Predicted Probability vs. Page Views",
            width = 6, solidHeader = TRUE,
            plotlyOutput("plot_pred_prob", height = 300)
          )
        )
      )
    )
  )
)

# ============================================================
# 2. SERVER
# ============================================================

server <- function(input, output, session) {
  
  df <- reactive({
    req(input$date_range)
    
    out <- df_sessions |>
      filter(
        session_date >= as.Date(input$date_range[1]),
        session_date <= as.Date(input$date_range[2])
      )
    
    if (!is.null(input$device_filter) && length(input$device_filter) > 0) {
      out <- out |> filter(as.character(device_category) %in% input$device_filter)
    }
    
    if (!is.null(input$source_filter) && length(input$source_filter) > 0) {
      out <- out |> filter(traffic_source %in% input$source_filter)
    }
    
    out
  })
  
  output$vbox_sessions <- renderValueBox({
    valueBox(comma(nrow(df())), "Total Sessions", icon = icon("users"), color = "black")
  })
  
  output$vbox_booking <- renderValueBox({
    r <- if (nrow(df()) > 0) percent(mean(df()$is_booking_conversion), accuracy = 0.01) else "N/A"
    valueBox(r, "Booking CVR", icon = icon("calendar-check"), color = "black")
  })
  
  output$vbox_donation <- renderValueBox({
    r <- if (nrow(df()) > 0) percent(mean(df()$is_donation_conversion), accuracy = 0.01) else "N/A"
    valueBox(r, "Donation CVR", icon = icon("hand-holding-heart"), color = "black")
  })
  
  output$vbox_email <- renderValueBox({
    r <- if (nrow(df()) > 0) percent(mean(df()$is_email_conversion), accuracy = 0.01) else "N/A"
    valueBox(r, "Newsletter CVR", icon = icon("envelope"), color = "black")
  })
  
  output$vbox_any <- renderValueBox({
    r <- if (nrow(df()) > 0) percent(mean(df()$is_any_conversion), accuracy = 0.01) else "N/A"
    valueBox(r, "Any Conversion", icon = icon("chart-line"), color = "black")
  })
  
  output$vbox_mobile_gap <- renderValueBox({
    d <- df() |>
      group_by(device_category) |>
      summarise(cvr = mean(is_booking_conversion), .groups = "drop")
    
    desk <- d |> filter(device_category == "desktop") |> pull(cvr)
    mob  <- d |> filter(device_category == "mobile")  |> pull(cvr)
    
    gap_str <- if (length(desk) > 0 && length(mob) > 0 && !is.na(mob) && mob > 0) {
      paste0(round(desk / mob, 1), "x")
    } else {
      "N/A"
    }
    
    valueBox(gap_str, "Desktop vs. Mobile CVR", icon = icon("mobile"), color = "black")
  })
  
  output$plot_daily_trend <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |>
      group_by(session_date) |>
      summarise(
        Sessions   = n(),
        Bookings   = sum(is_booking_conversion),
        Donations  = sum(is_donation_conversion),
        Newsletter = sum(is_email_conversion),
        .groups = "drop"
      ) |>
      arrange(session_date)
    
    p <- ggplot(d, aes(x = session_date)) +
      geom_col(aes(y = Sessions, fill = "Sessions"), alpha = 0.25) +
      geom_line(aes(y = Bookings * 10, color = "Bookings"), linewidth = 1) +
      geom_line(aes(y = Donations * 10, color = "Donations"), linewidth = 1) +
      geom_line(aes(y = Newsletter * 10, color = "Newsletter"), linewidth = 1) +
      scale_y_continuous(
        name = "Sessions",
        sec.axis = sec_axis(~ . / 10, name = "Conversions")
      ) +
      scale_color_manual(values = c("Bookings" = "#1A1A1A", "Donations" = "#666666", "Newsletter" = "#AAAAAA")) +
      scale_fill_manual(values = c("Sessions" = "#CCCCCC")) +
      labs(x = NULL, caption = "Right axis = conversion event count. Left axis = session volume.") +
      PLOT_THEME
    
    ggplotly(p, tooltip = c("x", "y")) |>
      layout(legend = list(orientation = "h"))
  })
  
  output$plot_conv_mix <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |>
      summarise(
        Booking    = sum(is_booking_conversion),
        Donation   = sum(is_donation_conversion),
        Newsletter = sum(is_email_conversion)
      ) |>
      pivot_longer(everything(), names_to = "type", values_to = "n")
    
    plot_ly(
      d,
      labels = ~type,
      values = ~n,
      type = "pie",
      hole = 0.55,
      marker = list(
        colors = c("#1A1A1A", "#666666", "#AAAAAA"),
        line = list(color = "#FFFFFF", width = 2)
      ),
      textinfo = "label+percent"
    ) |>
      layout(showlegend = FALSE, margin = list(t = 20, b = 20))
  })
  
  output$plot_engagement_hist <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |> filter(engagement_sec < 600)
    
    p <- ggplot(d, aes(x = engagement_sec)) +
      geom_histogram(bins = 40, fill = "#444444", color = "#FFFFFF", linewidth = 0.2) +
      labs(title = "Engagement Time per Session", x = "Seconds", y = "Sessions") +
      PLOT_THEME
    
    ggplotly(p)
  })
  
  output$plot_new_vs_return <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |>
      group_by(traffic_source) |>
      summarise(
        Booking    = mean(is_booking_conversion),
        Donation   = mean(is_donation_conversion),
        Newsletter = mean(is_email_conversion),
        .groups = "drop"
      ) |>
      pivot_longer(-traffic_source, names_to = "conv_type", values_to = "rate")
    
    p <- ggplot(d, aes(x = conv_type, y = rate, fill = traffic_source)) +
      geom_col(position = "dodge", width = 0.65) +
      scale_fill_manual(values = PAL_SOURCE[unique(d$traffic_source)]) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(title = "Conversion Rate by Source", x = NULL, y = "Conversion Rate") +
      PLOT_THEME
    
    ggplotly(p)
  })
  
  output$plot_booking_funnel <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df()
    
    stages <- c(
      "All Sessions"       = nrow(d),
      "Training Page View" = sum(d$training_page_view, na.rm = TRUE),
      "Add to Cart"        = sum(d$add_to_cart, na.rm = TRUE),
      "Checkout Started"   = sum(d$is_booking_conversion, na.rm = TRUE)
    )
    
    fd <- data.frame(stage = names(stages), n = as.integer(stages)) |>
      mutate(
        stage = factor(stage, levels = rev(names(stages))),
        pct = percent(n / stages[1], accuracy = 0.1)
      )
    
    p <- ggplot(fd, aes(x = stage, y = n)) +
      geom_col(fill = "#2C2C2C", width = 0.6) +
      geom_text(
        aes(label = paste0(comma(n), " (", pct, ")")),
        hjust = -0.1, size = 3.5, color = "#333333"
      ) +
      coord_flip() +
      expand_limits(y = max(fd$n) * 1.3) +
      labs(title = "Booking Conversion Funnel", x = NULL, y = "Sessions") +
      PLOT_THEME +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line()
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$plot_conv_bars <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |>
      summarise(
        Booking    = mean(is_booking_conversion),
        Donation   = mean(is_donation_conversion),
        Newsletter = mean(is_email_conversion)
      ) |>
      pivot_longer(everything(), names_to = "type", values_to = "rate") |>
      mutate(type = factor(type, levels = c("Booking", "Donation", "Newsletter")))
    
    p <- ggplot(d, aes(x = type, y = rate, fill = type)) +
      geom_col(width = 0.5) +
      geom_text(
        aes(label = percent(rate, accuracy = 0.01)),
        vjust = -0.4, size = 4, fontface = "bold"
      ) +
      scale_fill_manual(values = PAL_CONV) +
      scale_y_continuous(
        labels = percent_format(accuracy = 0.1),
        expand = expansion(mult = c(0, 0.15))
      ) +
      labs(title = "Conversion Rate by Type", x = NULL, y = "CVR") +
      PLOT_THEME +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$plot_weekly_conv <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |>
      mutate(week = floor_date(session_date, "week")) |>
      group_by(week) |>
      summarise(
        Booking    = mean(is_booking_conversion),
        Donation   = mean(is_donation_conversion),
        Newsletter = mean(is_email_conversion),
        .groups = "drop"
      ) |>
      pivot_longer(-week, names_to = "type", values_to = "rate")
    
    p <- ggplot(d, aes(x = week, y = rate, color = type, group = type)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      scale_color_manual(values = PAL_CONV) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(title = "Weekly Conversion Rate Trend", x = NULL, y = "CVR") +
      PLOT_THEME
    
    ggplotly(p, tooltip = c("x", "y", "color")) |>
      layout(legend = list(orientation = "h"))
  })
  
  output$plot_depth_bar <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |>
      group_by(page_view_bucket) |>
      summarise(
        sessions = n(),
        conversions = sum(is_booking_conversion),
        conv_rate = 100 * conversions / sessions,
        .groups = "drop"
      ) |>
      mutate(above = page_view_bucket %in% c("4–6 pages", "7–15 pages", "16–50 pages"))
    
    baseline <- 100 * mean(df()$is_booking_conversion)
    
    p <- ggplot(d, aes(x = page_view_bucket, y = conv_rate, fill = above)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = baseline, linetype = "dashed", color = "#888888", linewidth = 0.6) +
      geom_text(aes(label = paste0(round(conv_rate, 2), "%")), vjust = -0.4, size = 3.8, fontface = "bold") +
      annotate(
        "text", x = 0.6, y = baseline + 0.2,
        label = paste0("Baseline: ", round(baseline, 2), "%"),
        size = 3, color = "#888888", hjust = 0
      ) +
      scale_fill_manual(
        values = c("FALSE" = "#AAAAAA", "TRUE" = "#1A1A1A"),
        labels = c("Below threshold (1–3 pages)", "Above threshold (4+ pages)")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = "Booking Conversion Rate by Page View Depth",
        x = "Page View Bucket", y = "Conversion Rate (%)",
        caption = "Dashed line = baseline conversion rate. Sessions above 50 page views excluded."
      ) +
      PLOT_THEME
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$table_depth <- renderDT({
    req(nrow(df()) > 0)
    
    df() |>
      group_by(`Page View Bucket` = page_view_bucket) |>
      summarise(
        Sessions = n(),
        `% of Total` = paste0(round(100 * n() / nrow(df()), 1), "%"),
        Conversions = sum(is_booking_conversion),
        `Conv. Rate` = paste0(round(100 * Conversions / Sessions, 2), "%"),
        .groups = "drop"
      ) |>
      datatable(
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE),
        class = "compact stripe"
      )
  })
  
  output$plot_pv_hist <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |> filter(page_views <= 15) |> count(page_views)
    
    p <- ggplot(d, aes(x = page_views, y = n)) +
      geom_col(fill = "#444444", color = "#FFFFFF", linewidth = 0.3) +
      geom_text(aes(label = n), vjust = -0.3, size = 3.2) +
      labs(
        title = "Session Count by Page View Count (1–15)",
        x = "Page Views in Session", y = "Sessions"
      ) +
      scale_x_continuous(breaks = 1:15) +
      PLOT_THEME
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$plot_training_pv <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |>
      mutate(training = if_else(training_page_view == 1, "Visited Training Page", "No Training Page")) |>
      group_by(training) |>
      summarise(cvr = mean(is_booking_conversion), .groups = "drop")
    
    p <- ggplot(d, aes(x = training, y = cvr, fill = training)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = percent(cvr, accuracy = 0.01)), vjust = -0.4, size = 4, fontface = "bold") +
      scale_fill_manual(values = c("Visited Training Page" = "#1A1A1A", "No Training Page" = "#AAAAAA")) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.15))) +
      labs(title = "Training Page View vs. Booking Conversion", x = NULL, y = "Booking CVR") +
      PLOT_THEME +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$plot_device_conv <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |>
      group_by(device_category) |>
      summarise(
        Booking    = mean(is_booking_conversion),
        Donation   = mean(is_donation_conversion),
        Newsletter = mean(is_email_conversion),
        .groups = "drop"
      ) |>
      pivot_longer(-device_category, names_to = "type", values_to = "rate")
    
    p <- ggplot(d, aes(x = device_category, y = rate, fill = type)) +
      geom_col(position = "dodge", width = 0.65) +
      scale_fill_manual(values = PAL_CONV) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(title = "Conversion Rate by Device and Type", x = "Device", y = "Conversion Rate") +
      PLOT_THEME
    
    ggplotly(p, tooltip = c("x", "y", "fill")) |>
      layout(legend = list(orientation = "h"))
  })
  
  output$plot_device_vol <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |> count(device_category)
    
    p <- ggplot(d, aes(x = device_category, y = n, fill = device_category)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = comma(n)), vjust = -0.4, size = 4) +
      scale_fill_manual(values = PAL_DEVICE[names(PAL_DEVICE) %in% unique(as.character(d$device_category))]) +
      labs(title = "Session Volume by Device", x = NULL, y = "Sessions") +
      PLOT_THEME +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$plot_device_pv_box <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |> filter(page_views <= 20)
    
    p <- ggplot(d, aes(x = device_category, y = page_views, fill = device_category)) +
      geom_boxplot(outlier.shape = 21, outlier.size = 1.5, width = 0.5) +
      scale_fill_manual(values = PAL_DEVICE[names(PAL_DEVICE) %in% unique(as.character(d$device_category))]) +
      labs(title = "Page Views Distribution by Device", x = NULL, y = "Page Views") +
      PLOT_THEME +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$plot_device_eng <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |>
      filter(engagement_sec < 500) |>
      group_by(device_category) |>
      summarise(med = median(engagement_sec), avg = mean(engagement_sec), .groups = "drop")
    
    p <- ggplot(d, aes(x = device_category)) +
      geom_col(aes(y = avg, fill = device_category), width = 0.5, alpha = 0.85) +
      geom_point(aes(y = med), shape = 23, size = 4, fill = "#FFFFFF", color = "#333333") +
      scale_fill_manual(values = PAL_DEVICE[names(PAL_DEVICE) %in% unique(as.character(d$device_category))]) +
      labs(title = "Avg. Engagement Time by Device (diamonds = median)", x = NULL, y = "Seconds") +
      PLOT_THEME +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$plot_source_conv <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |>
      group_by(traffic_source) |>
      summarise(
        Booking    = mean(is_booking_conversion),
        Donation   = mean(is_donation_conversion),
        Newsletter = mean(is_email_conversion),
        .groups = "drop"
      ) |>
      pivot_longer(-traffic_source, names_to = "type", values_to = "rate")
    
    p <- ggplot(d, aes(x = reorder(traffic_source, rate), y = rate, fill = type)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = PAL_CONV) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(title = "Conversion Rate by Traffic Source", x = "Source", y = "CVR") +
      coord_flip() +
      PLOT_THEME
    
    ggplotly(p, tooltip = c("x", "y", "fill")) |>
      layout(legend = list(orientation = "h"))
  })
  
  output$plot_source_vol <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |> count(traffic_source)
    
    p <- ggplot(d, aes(x = reorder(traffic_source, n), y = n, fill = traffic_source)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = comma(n)), hjust = -0.1, size = 3.5) +
      scale_fill_manual(values = PAL_SOURCE[names(PAL_SOURCE) %in% d$traffic_source]) +
      expand_limits(y = max(d$n) * 1.15) +
      labs(title = "Sessions by Traffic Source", x = NULL, y = "Sessions") +
      coord_flip() +
      PLOT_THEME +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$plot_source_device_heat <- renderPlotly({
    req(nrow(df()) > 0)
    
    d <- df() |>
      group_by(traffic_source, device_category) |>
      summarise(cvr = mean(is_booking_conversion), .groups = "drop")
    
    p <- ggplot(d, aes(x = device_category, y = traffic_source, fill = cvr)) +
      geom_tile(color = "#FFFFFF", linewidth = 0.8) +
      geom_text(aes(label = percent(cvr, accuracy = 0.1)), size = 3.5) +
      scale_fill_gradient(low = "#EEEEEE", high = "#1A1A1A", labels = percent_format(accuracy = 0.1)) +
      labs(title = "Booking CVR: Source x Device", x = "Device", y = "Source", fill = "CVR") +
      PLOT_THEME +
      theme(panel.grid = element_blank())
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$table_sessions <- renderDT({
    req(nrow(df()) > 0)
    
    df() |>
      select(
        Date          = session_date,
        Device        = device_category,
        Source        = traffic_source,
        Medium        = medium,
        Campaign      = campaign_name,
        Country       = country,
        `Page Views`  = page_views,
        `Eng. Sec`    = engagement_sec,
        `Training PV` = training_page_view,
        `Add to Cart` = add_to_cart,
        Booking       = is_booking_conversion,
        Donation      = is_donation_conversion,
        Newsletter    = is_email_conversion,
        Revenue       = total_revenue
      ) |>
      datatable(
        rownames = FALSE,
        filter   = "top",
        options  = list(pageLength = 20, scrollX = TRUE),
        class    = "compact stripe hover"
      )
  })
  
  output$plot_or_forest <- renderPlotly({
    req(nrow(df()) > 50)
    
    d_model <- df() |>
      mutate(
        device_mobile = as.integer(device_category == "mobile"),
        device_tablet = as.integer(device_category == "tablet")
      )
    
    tryCatch({
      mod <- glm(
        is_booking_conversion ~ training_page_view + page_views + device_mobile + device_tablet,
        data = d_model,
        family = binomial
      )
      
      td <- broom::tidy(mod, exponentiate = TRUE, conf.int = TRUE) |>
        filter(term != "(Intercept)") |>
        mutate(
          label = c("Training Page View", "Page Views", "Device: Mobile", "Device: Tablet"),
          sig = p.value < 0.05,
          label = factor(label, levels = rev(label))
        )
      
      p <- ggplot(td, aes(x = label, y = estimate, color = sig)) +
        geom_point(size = 4) +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, linewidth = 1) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "#888888") +
        scale_color_manual(
          values = c("FALSE" = "#AAAAAA", "TRUE" = "#1A1A1A"),
          labels = c("Non-significant", "Significant (p < .05)")
        ) +
        scale_y_log10() +
        labs(
          title = "Odds Ratios with 95% Confidence Intervals (log scale)",
          x = NULL, y = "Odds Ratio (log scale)",
          caption = "Reference: Desktop device."
        ) +
        coord_flip() +
        PLOT_THEME
      
      ggplotly(p, tooltip = c("x", "y"))
    }, error = function(e) {
      plotly_empty()
    })
  })
  
  output$model_summary_text <- renderPrint({
    req(nrow(df()) > 50)
    
    d_model <- df() |>
      mutate(
        device_mobile = as.integer(device_category == "mobile"),
        device_tablet = as.integer(device_category == "tablet")
      )
    
    tryCatch({
      mod <- glm(
        is_booking_conversion ~ training_page_view + page_views + device_mobile + device_tablet,
        data = d_model,
        family = binomial
      )
      
      cat("Sessions analyzed: ", nrow(d_model), "\n")
      cat("Conversions:       ", sum(d_model$is_booking_conversion), "\n")
      cat("Baseline CVR:      ", round(mean(d_model$is_booking_conversion) * 100, 2), "%\n\n")
      print(summary(mod))
    }, error = function(e) {
      cat("Insufficient data for modeling with current filters.\n")
    })
  })
  
  output$plot_rf_importance <- renderPlotly({
    req(nrow(df()) > 100, requireNamespace("randomForest", quietly = TRUE))
    
    tryCatch({
      d_rf <- df() |>
        mutate(
          device_mobile = as.integer(device_category == "mobile"),
          device_tablet = as.integer(device_category == "tablet"),
          outcome = factor(is_booking_conversion)
        ) |>
        select(outcome, training_page_view, page_views, device_mobile, device_tablet) |>
        na.omit()
      
      rf <- randomForest::randomForest(outcome ~ ., data = d_rf, ntree = 300, importance = TRUE)
      
      imp <- importance(rf) |>
        as.data.frame() |>
        tibble::rownames_to_column("variable") |>
        arrange(desc(MeanDecreaseAccuracy)) |>
        mutate(variable = factor(variable, levels = rev(variable)))
      
      p <- ggplot(imp, aes(x = variable, y = MeanDecreaseAccuracy)) +
        geom_col(fill = "#2C2C2C", width = 0.6) +
        coord_flip() +
        labs(title = "Random Forest: Mean Decrease Accuracy", x = NULL, y = "Mean Decrease Accuracy") +
        PLOT_THEME
      
      ggplotly(p, tooltip = c("x", "y"))
    }, error = function(e) {
      plotly_empty() |>
        layout(
          title = list(
            text = "Install 'randomForest' package to view this chart",
            font = list(size = 13)
          )
        )
    })
  })
  
  output$plot_pred_prob <- renderPlotly({
    req(nrow(df()) > 50)
    
    tryCatch({
      d_model <- df() |>
        mutate(
          device_mobile = as.integer(device_category == "mobile"),
          device_tablet = as.integer(device_category == "tablet")
        )
      
      mod <- glm(
        is_booking_conversion ~ training_page_view + page_views + device_mobile + device_tablet,
        data = d_model,
        family = binomial
      )
      
      grid <- expand.grid(
        page_views = 1:25,
        training_page_view = c(0, 1),
        device_mobile = c(0, 1),
        device_tablet = 0
      ) |>
        mutate(
          pred = predict(mod, newdata = ., type = "response"),
          label = case_when(
            training_page_view == 1 & device_mobile == 0 ~ "Training PV + Desktop",
            training_page_view == 1 & device_mobile == 1 ~ "Training PV + Mobile",
            training_page_view == 0 & device_mobile == 0 ~ "No Training PV + Desktop",
            TRUE                                         ~ "No Training PV + Mobile"
          )
        )
      
      p <- ggplot(grid, aes(x = page_views, y = pred, color = label, linetype = label)) +
        geom_line(linewidth = 1.1) +
        scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
        scale_color_manual(values = c(
          "Training PV + Desktop" = "#1A1A1A",
          "Training PV + Mobile" = "#555555",
          "No Training PV + Desktop" = "#999999",
          "No Training PV + Mobile" = "#CCCCCC"
        )) +
        labs(
          title = "Predicted Booking Probability by Page Views",
          x = "Page Views", y = "Predicted Probability",
          caption = "Tablet excluded."
        ) +
        PLOT_THEME
      
      ggplotly(p, tooltip = c("x", "y", "color")) |>
        layout(legend = list(orientation = "h", y = -0.25))
    }, error = function(e) {
      plotly_empty()
    })
  })
}

# ============================================================
# 3. RUN APP
# ============================================================

shinyApp(ui, server)
