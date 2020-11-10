library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(readxl)
library(dplyr)
library(stringr)

rm(list=ls())

# Set path
DirInput  <- "[filepath]"

# input data
mydata <- read_xlsx(paste(DirInput, "Merged LAFLA Data (Full Joins).xlsx", sep="/"))


# Create a single flag for supervisoral distit
mydata <- mydata %>%
  mutate(supervisorial_district = gsub("  ", " ", str_trim(paste(ifelse(is.na(`Supervisorial District 1`), "", "1"),
                                                 ifelse(is.na(`Supervisorial District 2`), "", "2"),
                                                 ifelse(is.na(`Supervisorial District 3`), "", "3"),
                                                 ifelse(is.na(`Supervisorial District 4`), "", "4"),
                                                 ifelse(is.na(`Supervisorial District 5`), "", "5"),
                                        sep=" "))),
         # Only capitalize the first letter in each name 
         city = str_to_title(city))

# Create the list of variable names
columnChoices <- colnames(mydata)
zipChoices <- unique(mydata$GEOID)
cityChoices <- c("All Cities", unique(mydata$city))
supervisorChoices <- c("1", "2", "3", "4")
congressionalChoices <- c("TBD")

###########################################################################
# Data Download UI
###########################################################################

ui <- fluidPage(
  # Set theme
  theme = shinytheme("spacelab"),
  # Some help text
  h2("Data Download"),
  h4("Make selections to generate a table."),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(
      
      # Choose the columns
      selectInput("variableChoice", "Choose one or more variables:", choices = columnChoices, selected = c("city", "GEOID"), multiple = TRUE),
      
      # Choose the variables
      selectInput("cityChoice", "Choose one or more Cites:", choices = cityChoices, multiple = TRUE),
      
      # Choose the variables
      selectInput("zipChoice", "Choose one or more Zip Codes:", choices = zipChoices, multiple = TRUE),
      
      # Choose the variables
      selectInput("supervisorChoice", "Choose one or more Supervisorial Districts:", choices = supervisorChoices, multiple = TRUE),
      
      # Choose the variables
      selectInput("congressionalChoice", "Choose one or more Congressional Districts:", choices = congressionalChoices, multiple = TRUE),
      
      # Have another download button
      downloadButton("downloadData", "Download Selected Data")
    ),
      
    mainPanel(
      
      # Some help text
      h4("Table with choosen variables"), 
      dataTableOutput("table"))
      
  )
)

###########################################################################
# Data Download Server
###########################################################################

server <- function(input, output) {
  
  # Reactive value for dataset
  datasetInput <- reactive({
    
    # If supervisor districts are not choosen
    if(is.null(input$supervisorChoice)){
      
    mydata %>%
           # Select columns
           select(input$variableChoice) %>%
           # Filer cities
           filter(city %in% input$cityChoice |
           # Filter zip
                   GEOID %in% input$zipChoice)
    }
    
    # If supervisor districts are not choosen and all citites are
    # if(input$cityChoice == "All Cities"){
    # 
    #   mydata %>%
    #     # Select columns
    #     select(input$variableChoice)
    # 
    # }
    
    # If supervisor districts are choosen
    else {

      mydata %>%
             # Select columns
             select(input$variableChoice, supervisorial_district) %>%
             filter(str_detect(supervisorial_district, input$supervisorChoice) == TRUE)
    }
    
    
  })
  
  # Table of filtered dataset
  output$table <- renderDataTable({
    datasetInput()
  })
  
  # Downloadable csv of filtered dataset
  output$downloadData <- downloadHandler(
    filename = function() {

      # Create filename
      paste('Filtered Data - ', Sys.Date(), ".csv", sep = "")
      
    },
    content = function(file) {
      
      # Download content
      write.csv(datasetInput(), file, row.names = FALSE)
      
    }
  )
}

###########################################################################
# Run R Shiny App
###########################################################################

shinyApp(ui = ui, server = server)

####################################EOF####################################
