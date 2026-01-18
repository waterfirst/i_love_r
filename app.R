# # 모든 패키지 제거
install.packages(c("DBI", "RSQLite", "bslib", "rlang"))

# 2단계: R 캐시 디렉토리 정리
# Windows: %APPDATA%\R 폴더 삭제
# Mac: ~/.R 폴더 삭제
# Linux: ~/.R 폴더 삭제

# 3단계: R을 완전히 재시작하고 다시 설치
install.packages("shiny", type = "binary")
install.packages("ggplot2", type = "binary")
install.packages("dplyr", type = "binary")

library(tidyverse)

library(shiny)
library(ggplot2)
library(dplyr)
library(DBI)
library(RSQLite)
library(rlang)

# Initialize SQLite database
db_path <- "i_love_r.db"
con <- dbConnect(RSQLite::SQLite(), db_path)

# Create tables if not exist
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS meetups (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT,
    date TEXT,
    zoom_link TEXT,
    recording_link TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS discussions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    text TEXT,
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS news (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    content TEXT,
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )
")

# Sample data for manufacturing data visualization
manufacturing_data <- data.frame(
  Date = seq(as.Date("2023-01-01"), by = "month", length.out = 12),
  Production = rnorm(12, 1000, 100),
  Defects = rnorm(12, 50, 10),
  Efficiency = rnorm(12, 85, 5)
)

# UI - 기본 Shiny 문법 사용
ui <- fluidPage(
  title = "R Community Platform - I Love R",
  
  # CSS 스타일
  tags$head(
    tags$style(HTML("
      .nav-tabs { margin-bottom: 20px; }
      .discussion-item { padding: 10px; border-bottom: 1px solid #eee; }
      .card { border: 1px solid #ddd; padding: 15px; margin: 10px 0; border-radius: 5px; }
      .card-header { font-weight: bold; font-size: 16px; margin-bottom: 10px; }
    "))
  ),
  
  # 헤더
  h1("R Community Platform - I Love R", style = "text-align: center; color: #2c3e50;"),
  
  # 탭 네비게이션
  tabsetPanel(
    id = "main_tabs",
    
    # 탭 1: 데이터 시각화
    tabPanel(
      "Manufacturing Data Viz",
      icon = icon("chart-line"),
      
      div(class = "card",
          div(class = "card-header", "Interactive Data Visualization"),
          fluidRow(
            column(4,
                   selectInput("x_var", "X Variable", 
                               choices = c("Date", "Production", "Defects"), 
                               selected = "Date")
            ),
            column(4,
                   selectInput("y_var", "Y Variable", 
                               choices = c("Production", "Defects", "Efficiency"), 
                               selected = "Production")
            ),
            column(4,
                   checkboxInput("show_trend", "Show Trend Line", TRUE)
            )
          ),
          plotOutput("viz_plot", height = "500px")
      )
    ),
    
    # 탭 2: 미팅 일정
    tabPanel(
      "Meetup Calendar",
      icon = icon("calendar"),
      
      div(class = "card",
          div(class = "card-header", "Upcoming Meetups"),
          uiOutput("meetup_list"),
          hr(),
          h4("Schedule New Meetup"),
          textInput("meetup_title", "Meetup Title", placeholder = "예: R과 AI를 이용한 데이터 분석"),
          dateInput("meetup_date", "Date"),
          textInput("zoom_link", "Zoom Link"),
          textInput("recording_link", "Recording Link (after meetup)"),
          actionButton("schedule_meetup", "Schedule Meetup", class = "btn btn-primary")
      )
    ),
    
    # 탭 3: 데이터 공유
    tabPanel(
      "Data Sharing",
      icon = icon("upload"),
      
      div(class = "card",
          div(class = "card-header", "Share Your Data"),
          fileInput("data_file", "Upload CSV/Excel", accept = c(".csv", ".xlsx")),
          actionButton("analyze_data", "Analyze Data", class = "btn btn-primary"),
          verbatimTextOutput("data_summary")
      )
    ),
    
    # 탭 4: 뉴스
    tabPanel(
      "News & Packages",
      icon = icon("newspaper"),
      
      div(class = "card",
          div(class = "card-header", "Latest R News & Package Introductions"),
          uiOutput("news_list"),
          hr(),
          textAreaInput("news_input", "Share News/Package Info", rows = 3),
          actionButton("post_news", "Post", class = "btn btn-primary")
      )
    ),
    
    # 탭 5: 커뮤니티
    tabPanel(
      "Community Discussion",
      icon = icon("users"),
      
      div(class = "card",
          div(class = "card-header", "AI & R Discussion"),
          uiOutput("discussions"),
          hr(),
          textAreaInput("discussion_input", "Ask about AI in R or share insights", rows = 3),
          actionButton("post_discussion", "Post Discussion", class = "btn btn-primary")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load data from DB on start
  meetups_data <- reactiveVal(dbGetQuery(con, "SELECT * FROM meetups ORDER BY date DESC"))
  discussions_data <- reactiveVal(dbGetQuery(con, "SELECT * FROM discussions ORDER BY timestamp DESC"))
  news_data <- reactiveVal(dbGetQuery(con, "SELECT * FROM news ORDER BY timestamp DESC"))
  
  # Data Visualization
  output$viz_plot <- renderPlot({
    p <- ggplot(manufacturing_data, aes(x = !!sym(input$x_var), y = !!sym(input$y_var))) +
      geom_point(color = "#3498db", size = 3) +
      theme_minimal() +
      labs(
        title = "Manufacturing Data Analysis",
        x = input$x_var,
        y = input$y_var
      )
    
    if (input$show_trend) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "#e74c3c")
    }
    
    p
  })
  
  # Data Sharing
  uploaded_data <- reactive({
    req(input$data_file)
    ext <- tools::file_ext(input$data_file$name)
    if (ext == "csv") {
      read.csv(input$data_file$datapath)
    } else if (ext == "xlsx") {
      readxl::read_excel(input$data_file$datapath)
    }
  })
  
  output$data_summary <- renderPrint({
    req(uploaded_data())
    summary(uploaded_data())
  })
  
  # Discussions management
  output$discussions <- renderUI({
    discs <- discussions_data()
    if (nrow(discs) == 0) {
      p("No discussions yet. Start the conversation!")
    } else {
      lapply(1:nrow(discs), function(i) {
        d <- discs[i, ]
        div(
          class = "discussion-item",
          p(strong(format(as.POSIXct(d$timestamp), "%Y-%m-%d %H:%M")), ": ", d$text)
        )
      })
    }
  })
  
  observeEvent(input$post_discussion, {
    req(input$discussion_input)
    dbExecute(con, "INSERT INTO discussions (text) VALUES (?)", 
              params = list(input$discussion_input))
    discussions_data(dbGetQuery(con, "SELECT * FROM discussions ORDER BY timestamp DESC"))
    showNotification("Discussion posted!", type = "message")
    updateTextAreaInput(session, "discussion_input", value = "")
  })
  
  
  # Meetup management
  output$meetup_list <- renderUI({
    meetups <- meetups_data()
    if (nrow(meetups) == 0) {
      p("No meetups scheduled yet.")
    } else {
      lapply(1:nrow(meetups), function(i) {
        m <- meetups[i, ]
        div(
          class = "discussion-item",
          h5(m$title),
          p("📅 Date: ", strong(m$date)),
          if (!is.na(m$zoom_link) && m$zoom_link != "") 
            tags$a(href = m$zoom_link, "🔗 Join Zoom", target = "_blank", class = "btn btn-sm btn-info"),
          if (!is.na(m$recording_link) && m$recording_link != "") 
            tags$a(href = m$recording_link, "📹 View Recording", target = "_blank", class = "btn btn-sm btn-success")
        )
      })
    }
  })
  
  observeEvent(input$schedule_meetup, {
    req(input$meetup_title, input$meetup_date)
    
    # 입력값 검증
    if (input$meetup_title == "") {
      showNotification("Meetup title is required!", type = "error")
      return()
    }
    
    dbExecute(con, 
              "INSERT INTO meetups (title, date, zoom_link, recording_link) VALUES (?, ?, ?, ?)",
              params = list(
                input$meetup_title, 
                as.character(input$meetup_date), 
                input$zoom_link, 
                input$recording_link
              )
    )
    
    meetups_data(dbGetQuery(con, "SELECT * FROM meetups ORDER BY date DESC"))
    showNotification("Meetup scheduled!", type = "message")
    updateTextInput(session, "meetup_title", value = "")
    updateTextInput(session, "zoom_link", value = "")
    updateTextInput(session, "recording_link", value = "")
  })
  
  # News management
  output$news_list <- renderUI({
    news <- news_data()
    if (nrow(news) == 0) {
      p("No news shared yet.")
    } else {
      lapply(1:nrow(news), function(i) {
        n <- news[i, ]
        div(
          class = "discussion-item",
          p(strong(format(as.POSIXct(n$timestamp), "%Y-%m-%d %H:%M")), ": ", n$content)
        )
      })
    }
  })
  
  observeEvent(input$post_news, {
    req(input$news_input)
    
    if (input$news_input == "") {
      showNotification("Please enter news content!", type = "error")
      return()
    }
    
    dbExecute(con, "INSERT INTO news (content) VALUES (?)", 
              params = list(input$news_input))
    news_data(dbGetQuery(con, "SELECT * FROM news ORDER BY timestamp DESC"))
    showNotification("News posted!", type = "message")
    updateTextAreaInput(session, "news_input", value = "")
  })
  
  # Session 종료 시 정리
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

# 앱 실행
shinyApp(ui, server)
