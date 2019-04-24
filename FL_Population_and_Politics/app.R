
# Florida's Population & Politics Project
# By: Debora Gonzalez


# Data Setup:


library(tidyverse)
library(readxl)
library(gt)
library(tigris)
library(janitor)
library(leaflet)
library(ggthemes)
library(shiny)


party_affiliation_years <- read_rds("party_affiliation_years")

# Spreadsheet containing voter registration of FL voters by party as
# of Feb. 2019 from 1972 to 2019.



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("The Purple Sunshine State: Florida's Population & Politics"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("year", 
                    label = h3("Select Years"), 
                    min = min(unique(data2$year)), 
                    max = max(unique(data2$year)),
                    value = c(1972, 2019),
                    sep = "")
      ),
      
   
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("barPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$barPlot <- renderPlot({
     gop_subset <- party_affiliation_years %>% filter(year >= input$year[1] & year <= input$year[2])
     
     ggplot(gop_subset, aes(x = year, y = republican_party_of_florida)) + 
     geom_bar(stat="identity", fill = "red", colour = "black") +
       labs(y = NULL,
            x = "Year Range",
            title = "Number of Registered Republicans over Time")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

