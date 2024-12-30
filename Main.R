library(shiny)

ui <- fluidPage(
  titlePanel("Discover Employee"),
  
  sidebarLayout(
    sidebarPanel(
      h3(""),
      
      # Button or links to other apps
      actionButton("app1", "Apply for Job here"),
      br(), br(),
      actionButton("app2", "Employer Dashboard"),
      br(), br(),
      actionButton("app3", "Job Trends"),
    ),
    
    mainPanel(
      h4("Instructions"),
      p("Click the buttons on the left to open.")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$app1, {
    # Replace with the link to App 1
    browseURL("http://127.0.0.1:5000")  # Example: Local app
  })
  
  observeEvent(input$app2, {
    # Replace with the link to App 2
    browseURL("http://127.0.0.1:5001")  # Example: Another local app
  })
  
  observeEvent(input$app3, {
    # Replace with the link to App 3
    browseURL("http://127.0.0.1:5002")  # Example: Third app
  })
}

shinyApp(ui, server)
