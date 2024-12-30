# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("RSQLite")) install.packages("RSQLite")
if (!require("pdftools")) install.packages("pdftools")
if (!require("readtext")) install.packages("readtext")

library(shiny)
library(RSQLite)
library(pdftools)
library(readtext)


db <- dbConnect(SQLite(), "applications.db")
dbExecute(db, "
  CREATE TABLE IF NOT EXISTS Applications (
    ID INTEGER PRIMARY KEY,
    Name TEXT,
    Skills TEXT,
    Location TEXT,
    Experience REAL
  )
")


read_resume <- function(file_path) {
  if (grepl("\\.pdf$", file_path, ignore.case = TRUE)) {
    text <- pdf_text(file_path)
  } else if (grepl("\\.docx?$", file_path, ignore.case = TRUE)) {
    text <- readtext(file_path)$text
  } else {
    stop("Unsupported file format. Please upload a valid PDF or DOC/DOCX file.")
  }
  paste(text, collapse = "\n")
}


parse_resume_details <- function(resume_text) {
  
  name <- str_extract(resume_text, "(?<=Name:|\\n)[A-Z][a-zA-Z]+ [A-Z][a-zA-Z]+")
  
 
  skills <- str_extract(resume_text, "(?i)(Skills|Expertise):\\s*([^\n]+(?:\\n[^\n]+)?)")
  skills <- gsub("(Skills|Expertise):", "", skills, ignore.case = TRUE)
  skills <- trimws(skills)
  
 
  location <- str_extract(resume_text, "(?i)Location:\\s*([^\\n]+)")
  location <- gsub("Location:", "", location, ignore.case = TRUE)
  location <- trimws(location)
  
 
  experience <- str_extract(resume_text, "(?i)(\\d+\\.?\\d*)\\s*(years|yrs|Year|Yr)")
  experience <- as.numeric(str_extract(experience, "\\d+\\.?\\d*"))
  
  
  list(
    Name = ifelse(!is.na(name), name, "Not Specified"),
    Skills = ifelse(!is.na(skills), skills, "Not Specified"),
    Location = ifelse(!is.na(location), location, "Not Specified"),
    Experience = ifelse(!is.na(experience), experience, NA)
  )
}


ui <- fluidPage(
  titlePanel("Resume Parser and Storage App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("resume", "Upload Resume (PDF or DOC/DOCX)", 
                accept = c(".pdf", ".doc", ".docx")),
      actionButton("process", "Process Resume")
    ),
    
    mainPanel(
      h4("Extracted Details"),
      tableOutput("details_table"),
      hr(),
      h4("All Applications in Database"),
      tableOutput("applications_table")
    )
  )
)


server <- function(input, output, session) {
  parsed_details <- reactiveVal(NULL)  # Reactive to hold parsed details
  
  observeEvent(input$process, {
    req(input$resume)
    
    
    resume_text <- tryCatch({
      read_resume(input$resume$datapath)
    }, error = function(e) {
      showNotification("Error reading resume. Ensure it is a valid PDF/DOC file.", type = "error")
      return(NULL)
    })
    
    if (is.null(resume_text)) return()
    
    details <- parse_resume_details(resume_text)
    parsed_details(details)
    
    
    dbExecute(db, "INSERT INTO Applications (Name, Skills, Location, Experience) 
                   VALUES (?, ?, ?, ?)",
              params = list(details$Name, details$Skills, details$Location, details$Experience))
    
    showNotification("Resume processed and stored successfully!", type = "message")
  })
  
  
  output$details_table <- renderTable({
    details <- parsed_details()
    if (is.null(details)) return(NULL)
    data.frame(Field = names(details), Value = unlist(details))
  })
  

  output$applications_table <- renderTable({
    dbReadTable(db, "Applications")
  })
}


shinyApp(ui, server)
