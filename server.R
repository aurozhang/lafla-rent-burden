library(shiny)
library(janitor)

path <- "/Users/aurorazhang/Projects/Academic Projects/LAFLA Rent Burden Project/Data/Cleaned/"

income <- read.csv(paste0(path, "income.csv"))
income <- row_to_names(income, row_number = 1)
colnames(income) <- c("id", "Name", "Median Household Income (Estimate)", "Median Household Income (MOE)")
rent <- read.csv(paste0(path, "rent.csv"))
rent <- row_to_names(rent, row_number = 1)
colnames(rent) <- c("id", "Name", "Median Gross Rent (Estimate)", "Median Gross Rent (MOE)")
tenure <- read.csv(paste0(path, "tenure.csv"))
tenure <- row_to_names(tenure, row_number = 1)
colnames(tenure) <- c("id", "Name", "Total (Estimate)", "Total (MOE)", "Owner Occupied (Estimate)", "Owner Occupied (MOE)",
                      "Renter Occupied (Estimate)", "Renter Occupied (MOE)")


server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Median Income" = income,
           "Median Rent" = rent,
           "Tenure" = tenure)
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}
