library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)

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
        p("Meetup scheduling feature coming soon!"),
        textInput("meetup_title", "Meetup Title"),
        dateInput("meetup_date", "Date"),
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
        p("R Shiny updates, new packages for data analysis, AI integration tips."),
        textAreaInput("news_input", "Share News/Package Info", rows = 3),
        actionButton("post_news", "Post")
      )
    ),
    nav_panel(
      "Community Discussion",
      card(
        card_header("AI & R Discussion"),
        textAreaInput("discussion_input", "Ask about AI in R or share insights", rows = 3),
        actionButton("post_discussion", "Post Discussion"),
        uiOutput("discussions")
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Data Visualization
  output$viz_plot <- renderPlot({
    p <- ggplot(manufacturing_data, aes_string(x = input$x_var, y = input$y_var)) +
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

  # Discussions (simple reactive list)
  discussions <- reactiveVal(list())

  observeEvent(input$post_discussion, {
    req(input$discussion_input)
    new_disc <- list(
      text = input$discussion_input,
      timestamp = Sys.time()
    )
    current <- discussions()
    discussions(c(current, list(new_disc)))
    updateTextAreaInput(session, "discussion_input", value = "")
  })

  output$discussions <- renderUI({
    disc_list <- discussions()
    if (length(disc_list) == 0) {
      p("No discussions yet. Start the conversation!")
    } else {
      lapply(disc_list, function(d) {
        div(
          class = "discussion-item",
          p(strong(format(d$timestamp, "%Y-%m-%d %H:%M")), ": ", d$text),
          hr()
        )
      })
    }
  })

  # Meetup placeholder
  observeEvent(input$schedule_meetup, {
    showNotification("Meetup scheduled! (Feature expanding soon)", type = "message")
  })

  # News placeholder
  observeEvent(input$post_news, {
    showNotification("News posted! (Feature expanding soon)", type = "message")
  })
}

shinyApp(ui, server)