library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(DBI)
library(RSQLite)

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

# UI
ui <- page_sidebar(
  title = "R Community Platform - I Love R",
  sidebar = sidebar(
    title = "Navigation",
    navset_tab(
      nav_panel("Data Visualization", icon = icon("chart-line")),
      nav_panel("Meetups", icon = icon("calendar")),
      nav_panel("Data Sharing", icon = icon("upload")),
      nav_panel("News & Packages", icon = icon("newspaper")),
      nav_panel("Community", icon = icon("users"))
    )
  ),
  navset_card_tab(
    nav_panel(
      "Manufacturing Data Viz",
      card(
        card_header("Interactive Data Visualization"),
        layout_columns(
          selectInput("x_var", "X Variable", choices = c("Date", "Production", "Defects"), selected = "Date"),
          selectInput("y_var", "Y Variable", choices = c("Production", "Defects", "Efficiency"), selected = "Production"),
          checkboxInput("show_trend", "Show Trend Line", TRUE)
        ),
        plotOutput("viz_plot")
      )
    ),
    nav_panel(
      "Meetup Calendar",
      card(
        card_header("Upcoming Meetups"),
        uiOutput("meetup_list"),
        hr(),
        h4("Schedule New Meetup"),
        textInput("meetup_title", "Meetup Title"),
        dateInput("meetup_date", "Date"),
        textInput("zoom_link", "Zoom Link"),
        textInput("recording_link", "Recording Link (after meetup)"),
        actionButton("schedule_meetup", "Schedule Meetup")
      )
    ),
    nav_panel(
      "Data Sharing",
      card(
        card_header("Share Your Data"),
        fileInput("data_file", "Upload CSV/Excel", accept = c(".csv", ".xlsx")),
        actionButton("analyze_data", "Analyze Data"),
        verbatimTextOutput("data_summary")
      )
    ),
    nav_panel(
      "News & Packages",
      card(
        card_header("Latest R News & Package Introductions"),
        uiOutput("news_list"),
        hr(),
        textAreaInput("news_input", "Share News/Package Info", rows = 3),
        actionButton("post_news", "Post")
      )
    ),
    nav_panel(
      "Community Discussion",
      card(
        card_header("AI & R Discussion"),
        uiOutput("discussions"),
        hr(),
        textAreaInput("discussion_input", "Ask about AI in R or share insights", rows = 3),
        actionButton("post_discussion", "Post Discussion")
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
    p <- ggplot(manufacturing_data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
      geom_point() +
      theme_minimal() +
      labs(title = "Manufacturing Data Analysis")

    if (input$show_trend) {
      p <- p + geom_smooth(method = "lm")
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
          p(strong(format(as.POSIXct(d$timestamp), "%Y-%m-%d %H:%M")), ": ", d$text),
          hr()
        )
      })
    }
  })

  observeEvent(input$post_discussion, {
    req(input$discussion_input)
    dbExecute(con, "INSERT INTO discussions (text) VALUES (?)", params = list(input$discussion_input))
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
          h4(m$title),
          p("Date: ", m$date),
          if (!is.na(m$zoom_link) && m$zoom_link != "") tags$a(href = m$zoom_link, "Join Zoom", target = "_blank"),
          if (!is.na(m$recording_link) && m$recording_link != "") tags$a(href = m$recording_link, "View Recording", target = "_blank"),
          hr()
        )
      })
    }
  })

  observeEvent(input$schedule_meetup, {
    req(input$meetup_title, input$meetup_date)
    dbExecute(con, "INSERT INTO meetups (title, date, zoom_link, recording_link) VALUES (?, ?, ?, ?)",
              params = list(input$meetup_title, as.character(input$meetup_date), input$zoom_link, input$recording_link))
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
          p(strong(format(as.POSIXct(n$timestamp), "%Y-%m-%d %H:%M")), ": ", n$content),
          hr()
        )
      })
    }
  })

  observeEvent(input$post_news, {
    req(input$news_input)
    dbExecute(con, "INSERT INTO news (content) VALUES (?)", params = list(input$news_input))
    news_data(dbGetQuery(con, "SELECT * FROM news ORDER BY timestamp DESC"))
    showNotification("News posted!", type = "message")
    updateTextAreaInput(session, "news_input", value = "")
  })
}

# Close DB connection on app stop
onStop(function() {
  dbDisconnect(con)
})

shinyApp(ui, server)