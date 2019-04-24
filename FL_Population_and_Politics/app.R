
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
                    min = min(unique(party_affiliation_years$year)), 
                    max = max(unique(party_affiliation_years$year)),
                    value = c(1972, 2019),
                    sep = ""),
      
      selectInput("party",
                  label = "Select Party:",
                  choices = c("florida_republican_party", "democratic_republic_of_florida", "other_or_no_party_affiliation"),
                  selected = "florida_republican_party")
      ),
   
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(
           tabPanel(h3("About this Project"),
                    htmlOutput("text")),
           tabPanel(h3("Political Allegiance over Time"),
                    plotOutput("percents"), br(), plotOutput("reps"), br(), plotOutput("dems")), 
           tabPanel(h3("Florida's Political Allegiance by County"),
                    plotOutput("map_fl"))
      )
   )))


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$percents <- renderPlot({
    party_percents <- party_affiliation_years %>% 
      mutate(percent_dem = (florida_democratic_party/total)*100) %>% 
      mutate(percent_rep = (republican_party_of_florida/total)*100) %>% 
      mutate(percent_other = (other_or_no_party_affiliation/total)*100) %>% 
      filter(year >= input$year[1] & year <= input$year[2])
    
    ggplot(party_percents) +
      geom_line(aes(x =year, y = percent_rep),  color = "red3", size = 1, show.legend = FALSE) + 
      geom_line(aes(x = year, y = percent_dem), color = "blue4", size = 1, show.legend = FALSE) +
      geom_line(aes(x = year, y = percent_other), color = "green", size = 1, show.legend = FALSE) +
      labs(y = "Percentage of Registered Voters",
           x = "Year Range",
           title = "Changes in Political Allegiance Over Time") + 
      theme_economist()
      
  })
  
   output$reps <- renderPlot({
     gop_subset <- party_affiliation_years %>% filter(year >= input$year[1] & year <= input$year[2])
     
     ggplot(gop_subset, aes(x = year, y = republican_party_of_florida)) + 
     geom_bar(stat="identity", fill = "red", colour = "black") +
       labs(y = NULL,
            x = "Year Range",
            title = "Number of Registered Republicans over Time") + 
       theme_economist()
   })
   
     
     output$dems <- renderPlot({
       gop_subset <- party_affiliation_years %>% filter(year >= input$year[1] & year <= input$year[2])
       
       ggplot(gop_subset, aes(x = year, y = florida_democratic_party)) + 
         geom_bar(stat="identity", fill = "blue", colour = "black") +
         labs(y = NULL,
              x = "Year Range",
              title = "Number of Registered Democrats over Time") + 
         theme_economist()
         
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

