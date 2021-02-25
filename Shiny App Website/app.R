library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(readxl)
library(dplyr)
library(stringr)
library(rsconnect)

rm(list=ls())

# input data
mydata <- read_xlsx("./Data/Merged LAFLA Data.xlsx")

# Create a single flag for supervisoral distit
mydata <- mydata %>%
    rename(Zip_Code = GEOID,
           supervisorial_district = `Supervisorial District`) %>% 
    # Only capitalize the first letter in each name 
    mutate(city = str_to_title(city))

###########################################################################
# Remove not neeeded names
###########################################################################

mydata <- subset(mydata, select = -c(NAME, name, zip, `Area Name`,
                                     `Unlimited Civil (exclude Personal Injury)`,
                                     `Unlimited Civil (Personal Injury)`,
                                     `Family Law`, `Restraining Orders`, Probate,
                                     `Limited Civil (exclude Collection)`,
                                     `Limited Civil (Collection)`,
                                     `Limited Unlawful Detainer`, `Small Claims`,
                                     state, county, year))

###########################################################################
#  Create the list of variable names
###########################################################################

columnChoices <- colnames(mydata)
zipChoices <- c("NA", unique(mydata$Zip_Code))
cityChoices <- c("NA", unique(mydata$city))
cityCounselChoices <- c("NA", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
                        "13", "14", "15")
supervisorChoices <- c("NA", "1", "2", "3", "4", "5")
congressionalChoices <- str_split(unique(mydata$Congressional.District), " ", simplify = TRUE)

# Create a single column with all the different 
congressionalChoices <- c("NA", as.matrix(distinct(data.frame(congressional_district = c(t(congressionalChoices)))) %>% 
                                              filter(congressional_district != "" | !is.na(congressional_district) == T) %>% 
                                              arrange(congressional_district)))

###########################################################################
# Data Download UI
###########################################################################

ui <- fluidPage(
    # Set theme
    theme = shinytheme("spacelab"),
    
    # Add logos
    img(src = 'AG_Logo_Primary_Medium.png', height = '150px', width = '550px'),
    img(src = 'LAFLA.png', height = '150px', width = '350px'),
    
    # Some help text
    h2("Download Filtered Data"),
    h4("Make selections to generate a table"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        sidebarPanel(
            
            # Choose the columns
            selectInput("variableChoice", "Choose one or more variables:", choices = columnChoices, selected = c("city", "Zip_Code", "City_Council_District", "Congressional.District",
                                                                                                                 "supervisorial_district"), multiple = TRUE),
            
            # Choose the variables
            selectInput("cityChoice", "Choose Cites:", choices = cityChoices, selected = c("NA"), multiple = TRUE),
            
            # Choose the variables
            selectInput("zipChoice", "Choose Zip Codes:", choices = zipChoices, selected = c("NA"), multiple = TRUE),
            
            # Choose the variables
            selectInput("cityCounselChoice", "Choose City Council Districts:", selected = c("NA"),choices = cityCounselChoices, multiple = TRUE),
            
            # Choose the variables
            selectInput("supervisorChoice", "Choose Supervisorial Districts:", selected = c("NA"),choices = supervisorChoices, multiple = TRUE),
            
            # Choose the variables
            selectInput("congressionalChoice", "Choose Congressional Districts:", selected = c("NA"), choices = congressionalChoices, multiple = TRUE),
            
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
        
        mydata %>%
            # Select columns
            select(input$variableChoice) %>%
            # Filer cities
            filter(city %in% input$cityChoice |
                       # Filter citites
                       Zip_Code %in% input$zipChoice |
                       # Filter city counsel
                       str_detect(City_Council_District, input$cityCounselChoice) == TRUE |
                       # Filter supervisorial district
                       str_detect(supervisorial_district, input$supervisorChoice) == TRUE |
                       # Filter congressional distrcit
                       str_detect(Congressional.District, input$congressionalChoice) == TRUE)
        
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