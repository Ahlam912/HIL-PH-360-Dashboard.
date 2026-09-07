
library(shiny)

library(shinydashboard)
library(httr)
library(jsonlite)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)
library(emayili)

# ==========================================
# 1. CONFIGURATION - KOBO TOOLBOX API
# ==========================================
KOBO_SERVER   <- "https://kf.kobotoolbox.org"
KOBO_TOKEN    <- Sys.getenv("KOBO_TOKEN")
KOBO_ASSET_ID <- "aDp97W9BbuVn9tASh7wdSM"

competency_names <- c(
  "Public Sector Leadership",
  "Ethics & Integrity",
  "Resilience & Pressure",
  "Empathy & Coaching",
  "Vision & Storytelling",
  "Networking & Collaboration",
  "Change Management"
)

roles_list <- c("Self", "Manager", "Peer", "Direct Report", "Other")

fetch_kobo_data <- function(server, asset_id, token) {
  url <- sprintf("%s/api/v2/assets/%s/data.json", server, asset_id)
  res <- GET(url, add_headers(Authorization = paste("Token", token)), timeout(12))
  
  if (status_code(res) == 200) {
    raw_content <- content(res, "text", encoding = "UTF-8")
    parsed <- jsonlite::fromJSON(raw_content, flatten = TRUE)
    return(as.data.frame(parsed$results))
  } else {
    stop("API Error: Status ", status_code(res))
  }
}

map_likert_to_num <- function(val) {
  case_when(
    tolower(as.character(val)) %in% c("strongly disagree", "1") ~ 1,
    tolower(as.character(val)) %in% c("somewhat disagree", "2") ~ 2,
    tolower(as.character(val)) %in% c("neither agree nor disagree", "3") ~ 3,
    tolower(as.character(val)) %in% c("somewhat agree", "4") ~ 4,
    tolower(as.character(val)) %in% c("strongly agree", "5") ~ 5,
    TRUE ~ as.numeric(as.character(val))
  )
}

# ==========================================
# 2. USER INTERFACE (UI)
# ==========================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "HIL-PH 360° Portal"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Executive Summary", tabName = "summary", icon = icon("file-alt")),
      menuItem("Overview & Visuals", tabName = "overview", icon = icon("chart-pie")),
      menuItem("Perception Gap", tabName = "perception_gap", icon = icon("balance-scale")),
      menuItem("Qualitative Comments", tabName = "comments", icon = icon("comments")),
      menuItem("Email Notification", tabName = "email_tab", icon = icon("envelope"))
    ),
    hr(),
    div(
      style = "padding: 12px;",
      actionButton("sync_kobo_btn", "Sync Kobo Data", icon = icon("sync"), class = "btn-success btn-block"),
      br(),
      h4("Dashboard Filters", style = "color: white; font-weight: bold;"),
      uiOutput("leader_select_ui"),
      checkboxGroupInput("role_filter", "Filter Roles:", choices = roles_list, selected = roles_list),
      dateRangeInput("date_filter", "Date Range:", start = Sys.Date() - 180, end = Sys.Date())
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "summary",
        fluidRow(
          box(width = 12, title = "HIL-PH 360-Degree Executive Summary (2026)", status = "primary", solidHeader = TRUE, uiOutput("summary_text_ui"))
        ),
        fluidRow(
          box(width = 6, title = "Top Competency Strengths", status = "success", solidHeader = TRUE, tableOutput("top_strengths_tbl")),
          box(width = 6, title = "Development Opportunities", status = "warning", solidHeader = TRUE, tableOutput("low_competencies_tbl"))
        )
      ),
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("self_score_box", width = 2),
          valueBoxOutput("manager_score_box", width = 3),
          valueBoxOutput("peer_score_box", width = 2),
          valueBoxOutput("direct_report_score_box", width = 3),
          valueBoxOutput("other_score_box", width = 2)
        ),
        fluidRow(
          box(width = 7, title = "360° Radar Competency Overlay", status = "primary", solidHeader = TRUE, plotlyOutput("radar_chart", height = "400px")),
          box(width = 5, title = "Evaluator Count by Role", status = "primary", solidHeader = TRUE, plotlyOutput("role_donut_chart", height = "400px"))
        )
      ),
      tabItem(
        tabName = "perception_gap",
        fluidRow(
          box(width = 12, title = "Competency Gap Analysis across Roles", status = "info", solidHeader = TRUE, plotlyOutput("gap_bar_chart", height = "450px"))
        )
      ),
      tabItem(
        tabName = "comments",
        fluidRow(
          box(width = 12, title = "Open Feedback (Questions 53 & 54)", status = "warning", solidHeader = TRUE, DTOutput("comments_dt"))
        )
      ),
      tabItem(
        tabName = "email_tab",
        fluidRow(
          box(
            width = 8, title = "Send Summary Report via Email", status = "primary", solidHeader = TRUE,
            textInput("email_to", "Recipient Email:", placeholder = "recipient@example.com"),
            textInput("email_subject", "Subject:", value = "HIL-PH 360 Leadership Assessment Summary"),
            textAreaInput("email_body", "Custom Message:", value = "Below is the summary of the 360-degree leadership feedback results.", rows = 4),
            hr(),
            h5("SMTP Configuration (Sender Account)"),
            fluidRow(
              column(6, textInput("smtp_host", "SMTP Host:", value = "smtp.gmail.com")),
              column(6, numericInput("smtp_port", "Port:", value = 465)),
              column(6, textInput("smtp_user", "Sender Email:", placeholder = "your_email@gmail.com")),
              column(6, passwordInput("smtp_pass", "Google App Password (16 characters):"))
            ),
            br(),
            actionButton("send_email_btn", "Send Summary Email", class = "btn-success btn-lg", icon = icon("paper-plane"))
          )
        )
      )
    )
  )
)

# ==========================================
# 3. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {
  
  raw_kobo_store <- reactiveVal()
  
  load_kobo_data <- function() {
    tryCatch({
      df <- fetch_kobo_data(KOBO_SERVER, KOBO_ASSET_ID, KOBO_TOKEN)
      
      target_col <- grep("target|leader|person|survey_for", names(df), value = TRUE, ignore.case = TRUE)[1]
      role_col   <- grep("role|relationship", names(df), value = TRUE, ignore.case = TRUE)[1]
      date_col   <- grep("_submission_time|today|date", names(df), value = TRUE, ignore.case = TRUE)[1]
      
      if(is.na(target_col)) df$Target <- "Leader A" else df$Target <- df[[target_col]]
      if(is.na(role_col))   df$Role   <- "Peer"     else df$Role   <- df[[role_col]]
      if(is.na(date_col))   df$Date   <- Sys.Date() else df$Date   <- as.Date(df[[date_col]])
      
      q_cols <- grep("^q[1-9]|^q[1-4][0-9]|^q5[0-2]", names(df), value = TRUE, ignore.case = TRUE)
      for(col in q_cols) { df[[col]] <- map_likert_to_num(df[[col]]) }
      
      raw_kobo_store(as.data.frame(df))
      showNotification("Data synchronized with KoboToolbox!", type = "message")
    }, error = function(e) {
      showNotification("Loading demo dataset...", type = "message", duration = 3)
      
      set.seed(42)
      mock_rows <- list()
      id <- 1
      for(leader in c("Leader A", "Leader B")) {
        for(r in roles_list) {
          count <- if(r %in% c("Self", "Manager")) 1 else 3
          for(i in 1:count) {
            row_data <- list(ID = id, Target = leader, Role = r, Date = Sys.Date() - sample(1:60, 1))
            for(q in 1:52) { row_data[[paste0("q", q)]] <- runif(1, 3.0, 5.0) }
            row_data[["q53"]] <- "Great vision and leading by example."
            row_data[["q54"]] <- "Focus more on operational delegation."
            mock_rows[[id]] <- row_data
            id <- id + 1
          }
        }
      }
      # FIX: Explicitly bind to data.frame
      raw_kobo_store(as.data.frame(bind_rows(mock_rows)))
    })
  }
  
  observe({ load_kobo_data() })
  observeEvent(input$sync_kobo_btn, { load_kobo_data() })
  
  output$leader_select_ui <- renderUI({
    df <- raw_kobo_store()
    req(df)
    selectInput("leader_filter", "Select Leader:", choices = unique(df$Target), selected = unique(df$Target)[1])
  })
  
  # FIX: Ensure as.data.frame wrapper inside filtered_data
  filtered_data <- reactive({
    df <- raw_kobo_store()
    req(df, input$leader_filter, input$role_filter, input$date_filter)
    
    as.data.frame(df) %>%
      filter(
        Target == input$leader_filter,
        Role %in% input$role_filter,
        Date >= input$date_filter[1] & Date <= input$date_filter[2]
      )
  })
  
  competency_scores <- reactive({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    q_cols <- grep("^q[1-9]|^q[1-4][0-9]|^q5[0-2]", names(df), value = TRUE, ignore.case = TRUE)
    if(length(q_cols) == 0) return(NULL)
    
    df %>%
      mutate(
        C1 = rowMeans(select(., any_of(q_cols[1:7])), na.rm = TRUE),
        C2 = rowMeans(select(., any_of(q_cols[8:15])), na.rm = TRUE),
        C3 = rowMeans(select(., any_of(q_cols[16:22])), na.rm = TRUE),
        C4 = rowMeans(select(., any_of(q_cols[23:30])), na.rm = TRUE),
        C5 = rowMeans(select(., any_of(q_cols[31:37])), na.rm = TRUE),
        C6 = rowMeans(select(., any_of(q_cols[38:45])), na.rm = TRUE),
        C7 = rowMeans(select(., any_of(q_cols[46:52])), na.rm = TRUE)
      )
  })
  
  comp_summary <- reactive({
    df <- competency_scores()
    req(df)
    
    long_df <- df %>%
      select(Role, C1:C7) %>%
      pivot_longer(cols = C1:C7, names_to = "Comp_ID", values_to = "Score") %>%
      group_by(Comp_ID) %>%
      summarise(AvgScore = round(mean(Score, na.rm = TRUE), 2), .groups = 'drop')
    
    comp_map <- setNames(competency_names, paste0("C", 1:7))
    long_df$Competency <- comp_map[long_df$Comp_ID]
    long_df %>% arrange(desc(AvgScore))
  })
  
  output$summary_text_ui <- renderUI({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    total_evals <- nrow(df)
    df_comp <- competency_scores()
    overall_avg <- round(mean(c(df_comp$C1, df_comp$C2, df_comp$C3, df_comp$C4, df_comp$C5, df_comp$C6, df_comp$C7), na.rm = TRUE), 2)
    
    tagList(
      h4(paste("Evaluation Report for:", input$leader_filter)),
      p(paste0("This summary consolidates feedback from ", total_evals, " evaluators across selected roles.")),
      p(tags$b("Overall Leadership Rating: "), span(style = "font-size: 18px; color: #28a745;", paste0(overall_avg, " / 5.0")))
    )
  })
  
  output$top_strengths_tbl <- renderTable({ head(comp_summary(), 3) %>% select(Competency, `Avg Score` = AvgScore) })
  output$low_competencies_tbl <- renderTable({ tail(comp_summary(), 3) %>% select(Competency, `Avg Score` = AvgScore) })
  
  get_role_avg <- function(role_name) {
    df <- competency_scores() %>% filter(Role == role_name)
    if(nrow(df) > 0) round(mean(c(df$C1, df$C2, df$C3, df$C4, df$C5, df$C6, df$C7), na.rm = TRUE), 2) else "N/A"
  }
  
  output$self_score_box <- renderValueBox({ valueBox(get_role_avg("Self"), "Self", icon = icon("user"), color = "purple") })
  output$manager_score_box <- renderValueBox({ valueBox(get_role_avg("Manager"), "Manager", icon = icon("user-tie"), color = "blue") })
  output$peer_score_box <- renderValueBox({ valueBox(get_role_avg("Peer"), "Peers", icon = icon("user-friends"), color = "green") })
  output$direct_report_score_box <- renderValueBox({ valueBox(get_role_avg("Direct Report"), "Direct Reports", icon = icon("users"), color = "teal") })
  output$other_score_box <- renderValueBox({ valueBox(get_role_avg("Other"), "Others", icon = icon("globe"), color = "orange") })
  
  output$radar_chart <- renderPlotly({
    df <- competency_scores()
    req(df)
    summary_df <- df %>% group_by(Role) %>% summarise(across(C1:C7, ~mean(.x, na.rm = TRUE)))
    
    fig <- plot_ly(type = 'scatterpolar', fill = 'toself')
    for(r in unique(summary_df$Role)) {
      role_data <- summary_df %>% filter(Role == r)
      vals <- as.numeric(role_data[1, -1])
      fig <- fig %>% add_trace(r = c(vals, vals[1]), theta = c(competency_names, competency_names[1]), name = r)
    }
    fig %>% layout(polar = list(radialaxis = list(visible = TRUE, range = c(1, 5))))
  })
  
  output$role_donut_chart <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    plot_ly(df %>% count(Role), labels = ~Role, values = ~n, type = 'pie', hole = 0.5)
  })
  
  output$gap_bar_chart <- renderPlotly({
    df <- competency_scores()
    req(df)
    
    long_df <- df %>%
      select(Role, C1:C7) %>%
      pivot_longer(cols = C1:C7, names_to = "Comp_ID", values_to = "Score") %>%
      group_by(Role, Comp_ID) %>%
      summarise(AvgScore = mean(Score, na.rm = TRUE), .groups = 'drop')
    
    comp_map <- setNames(competency_names, paste0("C", 1:7))
    long_df$Competency <- comp_map[long_df$Comp_ID]
    
    plot_ly(long_df, x = ~Competency, y = ~AvgScore, color = ~Role, type = "bar") %>%
      layout(barmode = "group", yaxis = list(range = c(1, 5)))
  })
  
  output$comments_dt <- renderDT({
    df <- filtered_data()
    q53_col <- grep("q53|do_more|more", names(df), value = TRUE, ignore.case = TRUE)[1]
    q54_col <- grep("q54|differently|change", names(df), value = TRUE, ignore.case = TRUE)[1]
    
    res <- df %>% select(Role, Date)
    res$`What to do more (Q53)` <- if(!is.na(q53_col)) df[[q53_col]] else "N/A"
    res$`What to do differently (Q54)` <- if(!is.na(q54_col)) df[[q54_col]] else "N/A"
    
    datatable(res, options = list(pageLength = 5))
  })
  
  observeEvent(input$send_email_btn, {
    req(input$email_to, input$smtp_user, input$smtp_pass)
    
    tryCatch({
      top_comp <- comp_summary()$Competency[1]
      body_text <- paste0(
        input$email_body, "\n\n",
        "--- HIL-PH 360 EVALUATION SUMMARY ---\n",
        "Leader: ", input$leader_filter, "\n",
        "Total Evaluators: ", nrow(filtered_data()), "\n",
        "Top Competency: ", top_comp, "\n\n",
        "Generated via HIL-PH 360° Dashboard."
      )
      
      email <- envelope() %>%
        from(input$smtp_user) %>%
        to(input$email_to) %>%
        subject(input$email_subject) %>%
        text(body_text)
      
      smtp <- server(
        host = input$smtp_host,
        port = input$smtp_port,
        username = input$smtp_user,
        password = input$smtp_pass,
        max_bytes = 10000000
      )
      
      smtp(email, verbose = FALSE)
      showNotification("Summary email sent successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Email failed:", e$message), type = "error", duration = 8)
    })
  })
}

shinyApp(ui = ui, server = server)
