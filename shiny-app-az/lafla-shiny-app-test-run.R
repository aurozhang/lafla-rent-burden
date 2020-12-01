##################################################################################
###                                 Set-up                                     ###
##################################################################################


## Import Libraries ##

library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(ggpubr)
library(highcharter)
library(lubridate)
library(plotly)
library(readxl)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(sqldf)
library(zoo)
library(tidyr)
#library(devtools)
#library(choroplethrZip)


mydata <- read_xlsx("Data/Merged LAFLA Data.xlsx")

##################################################################################
###                               User Interface                               ###
##################################################################################


## Define the User Interface ##

ui <- dashboardPage(
  
  dashboardHeader(title = "LAFLA Shiny App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Rent Burden Datasets",
               tabName = "datasets-tab",
               icon = icon("table")),
      menuItem(text = "Rent Burden Heat Map", 
               tabName = "heatmap-tab",
               icon = icon("map"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "datasets-tab"),
      tabItem(tabName = "heatmap-tab",
              fluidPage(
                # Set theme
                theme = shinytheme("spacelab"),
                # Some help text
                h2("LA County Heat Maps"),
                h4("Make selections to generate a heat map."),
                # Sidebar layout with a input and output definitions
                sidebarLayout(
                  # Inputs
                  sidebarPanel(
                    # Data to View
                    radioButtons(inputId = "HM_val", 
                                 label = "Select Data to View", 
                                 choices = c("Total Data", "Demographics", "Housing", "Other"), 
                                 selected = "Total Data"),
                    
                    
                    uiOutput("map_a_val"),
                    uiOutput("map_b_val"),                    
                    
                  ),
                  
                  # Plotly Chart Area
                  
                  mainPanel(
                    
                    fluidRow(
                      splitLayout(cellWidths = c("50%", "50%"), 
                                  h2("Map of Variable A"),
                                  h2("Map of Variable B")),
                      splitLayout(cellWidths = c("50%", "50%"), 
                                  plotOutput(
                                    outputId = "HM_map_a",
                                    height = 600,
                                    width = 600),
                                  plotOutput(
                                    outputId = "HM_map_b",
                                    height = 600,
                                    width = 600)
                                  ),
                      splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton('download_var_a_map', 'Download Var A Map'),
                                  downloadButton('download_var_b_map', 'Download Var B Map')
                                  ),
                      h4(strong("Notes:")),
                      h4("1. The legend can represent dollars, count, or percentages."),
                      h4("2. The zip codes are manually selected, and might not represent the governement definition of Greater Los Angeles.\n\n"),
                      
                      splitLayout(cellWidths = c("50%", "50%"), 
                                  h2("Variable A Data"),
                                  h2("Variable B Data")),
                      
                      splitLayout(cellWidths = c("50%", "50%"),
                                  DT::dataTableOutput(outputId = "DT_table_a"),
                                  DT::dataTableOutput(outputId = "DT_table_b")),
                      
                      splitLayout(cellWidths = c("50%", "50%"),
                                  plotlyOutput(outputId = "PL_chart_a"),
                                  plotlyOutput(outputId = "PL_chart_b"))
                      
                      
                    )
                    
                  )
                  
                )
              )
              
              
              
              
              
              
              
              )
      
    ),
    
    
    tags$head(tags$style(HTML('
        /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #e11631;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #e11631;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #e11631;
                              }        
                              
                              ')))
  )
  
)


##################################################################################
###                                 Server                                     ###
##################################################################################


## Define the Server Functionality ##

server <- function(input, output, session){
  
  # Set path
  Dir       <- ""
  DirInput  <- "shiny-app-az/input/"
  DirOutput <- "shiny-app-az/output/"
  
  ###########################################################################
  # Import data
  ###########################################################################
  
  # Import federal housing data
  filename <- paste(DirInput, "Merged LAFLA Data (Full Joins).xlsx", sep="/")
  masterData <- read_xlsx(filename) %>%
    # Make GEOID a character
    mutate(GEOID = as.character(GEOID)) 
  
  ###########################################################################
  # Filter data
  ###########################################################################
  
  # Call dataset from choroplethrZip
  data(zip.regions)
  
  # Filter to LA Zip codes from choroplethrZip
  LARegions <- (zip.regions %>% 
                  filter(county.fips.numeric == 6037) %>%
                  distinct(region))[[1]]
  
  # The data.frame that you provide to zip_choropleth must have one column named 
  # "region" and one column named "value". Your entries for "region" must exactly 
  # match how regions are named in the map which choroplethr uses. 

  Select_Var_A <- eventReactive(input$HM_map_a_val, {
    # Input interested variable
    var = c(input$HM_map_a_val)
    
    masterDataMap <- masterData %>%
      # Filter to LA FIPS code
      filter(GEOID %in% LARegions) %>%
      # Only keep interested variables
      select(GEOID, var)
    
    # Rename variables to work with package
    names(masterDataMap) <- c("region", "value")
    
    ###########################################################################
    # Create map
    ###########################################################################
    
    # Use the choroplethrZip package
    map <- zip_choropleth(masterDataMap,
                          # Filter to LA codes
                          zip_zoom = LARegions,
                          legend   = "Count") + 
      # Change the title theme
      theme(plot.title = element_text(size = 12, face = "bold", hjust=0.5, margin = margin(b = 20, r=0, l=0, t=20)),
            legend.title = element_text( size=10, face="bold")) +
      # Add a title
      ggtitle(paste0(var,"\n", "Greater Los Angeles"))
    
    map
  })

  
  output$HM_map_a <- renderPlot({
    print(Select_Var_A()) 
  })
  
  Select_Var_B <- eventReactive(input$HM_map_b_val, {
    # Input interested variable
    var = c(input$HM_map_b_val)
    
    masterDataMap <- masterData %>%
      # Filter to LA FIPS code
      filter(GEOID %in% LARegions) %>%
      # Only keep interested variables
      select(GEOID, var)
    
    # Rename variables to work with package
    names(masterDataMap) <- c("region", "value")
    
    ###########################################################################
    # Create map
    ###########################################################################
    
    # Use the choroplethrZip package
    map <- zip_choropleth(masterDataMap,
                          # Filter to LA codes
                          zip_zoom = LARegions,
                          legend   = "Count") + 
      # Change the title theme
      theme(plot.title = element_text(size = 12, face = "bold", hjust=0.5, margin = margin(b = 20, r=0, l=0, t=20)),
            legend.title = element_text( size=10, face="bold")) +
      # Add a title
      ggtitle(paste0(var,"\n", "Greater Los Angeles"))
    
    map
  })
  
  
  output$HM_map_b <- renderPlot({
    print(Select_Var_B()) 
  })
  
  output$download_var_a_map <- downloadHandler(
    filename = "Variable A Map.png",
    content = function(file) {
      png(file)
      print(Select_Var_A())
      dev.off()
    })    
  
  output$download_var_b_map <- downloadHandler(
    filename = "Variable B Map.png",
    content = function(file) {
      png(file)
      print(Select_Var_B())
      dev.off()
    })    
  
  select_variable_grouping <- eventReactive(input$HM_val, {

    if(input$HM_val == "Total Data"){
      cols = c(1:544)
    } 
    else if (input$HM_val == "Demographics"){
      cols = c(1:511)
    }
    else if (input$HM_val == "Housing"){
      cols = c(512:517)
    }
    else if (input$HM_val == "Other"){
      cols = c(519:540)
    }
    data <- mydata[, cols]
    data
  })
  
  output$map_a_val <- renderUI(
    # Map Variable A
    shinyWidgets::pickerInput(inputId = "HM_map_a_val",
                              label = "Map Variable A",
                              choices = names(select_variable_grouping()),
                              selected = "Total Mid-March Employees",
                              multiple = FALSE,
                              options = list(`actions-box` = TRUE)
    )
  )
  
  output$map_b_val <- renderUI(
    # Map Variable B
    shinyWidgets::pickerInput(inputId = "HM_map_b_val",
                              label = "Map Variable B",
                              choices = names(select_variable_grouping()),
                              selected = "B25064_001",
                              multiple = FALSE,
                              options = list(`actions-box` = TRUE)
    )
  )
  
  Var_A <- eventReactive(input$HM_map_a_val, {
    # Input interested variable
    var = c(input$HM_map_a_val)
    
    masterDataMap <- masterData %>%
      # Filter to LA FIPS code
      filter(GEOID %in% LARegions) %>%
      # Only keep interested variables
      select(GEOID, var)
    
    # Rename variables to work with package
    names(masterDataMap) <- c("ZIP Code", input$HM_map_a_val)
    masterDataMap
  })
  
  Var_B <- eventReactive(input$HM_map_b_val, {
    # Input interested variable
    var = c(input$HM_map_b_val)
    
    masterDataMap <- masterData %>%
      # Filter to LA FIPS code
      filter(GEOID %in% LARegions) %>%
      # Only keep interested variables
      select(GEOID, var)
    
    # Rename variables to work with package
    names(masterDataMap) <- c("ZIP Code", input$HM_map_b_val)
    masterDataMap
  })
  
  output$DT_table_a <- DT::renderDataTable(
    Var_A(),
    rownames = FALSE,
  )
  
  output$DT_table_b <- DT::renderDataTable(
    Var_B(),
    rownames = FALSE,
  )
  
}


##################################################################################
###                           Knit UI & Server                                 ###
##################################################################################


shinyApp(ui = ui, server = server)