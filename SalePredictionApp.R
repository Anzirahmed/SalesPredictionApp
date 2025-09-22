
library(shiny)
library(DBI)
library(RMariaDB)
library(ggplot2)
library(shinydashboard)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Sales Prediction Dashboard", titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Predict Sales", tabName = "predict", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Prediction Tab
      tabItem(
        tabName = "predict",
        fluidRow(
          box(
            title = "Input Advertising Costs",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            sliderInput("tv", "TV Advertising Cost:", min = 0, max = 300, value = 100, step = 1),
            sliderInput("radio", "Radio Advertising Cost:", min = 0, max = 100, value = 20, step = 1),
            sliderInput("newspaper", "Newspaper Advertising Cost:", min = 0, max = 100, value = 30, step = 1),
            actionButton("predict", "Predict Sales", class = "btn-primary")
          ),
          box(
            title = "Prediction Results",
            status = "success",
            solidHeader = TRUE,
            width = 8,
            h3("Predicted Sales:"),
            verbatimTextOutput("predicted_sales")
          )
        ),
        fluidRow(
          box(
            title = "Sales Visualization",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotOutput("sales_plot", height = "400px")
          )
        )
      ),
      # About Tab
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About this Dashboard",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            h4("This dashboard predicts sales based on advertising costs for TV, Radio, and Newspaper."),
            p("Use the sliders in the 'Predict Sales' tab to input advertising costs."),
            p("The dashboard uses a linear regression model to calculate predictions and visualize relationships."),
            p("Built using R Shiny and MySQL.")
          )
        )
      )
    )
  )
)


server <- function(input, output) {
  # Connect to MySQL database
  conn <- dbConnect(RMariaDB::MariaDB(),
                    dbname = "advertising_db",
                    host = "localhost",
                    user = "root",
                    password = "12345678")
  
  on.exit(dbDisconnect(conn), add = TRUE)

  data <- dbReadTable(conn, "advertising")
  
  # linear regression model
  model <- lm(Sales ~ TV + Radio + Newspaper, data = data)
  

  predicted_value <- eventReactive(input$predict, {
    new_data <- data.frame(TV = input$tv, Radio = input$radio, Newspaper = input$newspaper)
    predict(model, newdata = new_data)
  })
  
  # Output
  output$predicted_sales <- renderText({
    if (input$predict > 0) {
      paste(round(predicted_value(), 2))
    } else {
      "Enter advertising costs and click Predict Sales."
    }
  })
  
  # relationships
  output$sales_plot <- renderPlot({
    ggplot(data, aes(x = TV, y = Sales)) +
      geom_point(color = "#0073B7") +
      geom_smooth(method = "lm", color = "#FF5733", size = 1) +
      labs(
        title = "Sales vs TV Advertising Cost",
        x = "TV Advertising Cost",
        y = "Sales"
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui = ui, server = server)
