
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
library(sf)
library(fivethirtyeight)
library(plotly)


party_affiliation_years <- read_rds("party_affiliation_years")

# Spreadsheet containing voter registration of FL voters by party as
# of Feb. 2019 from 1972 to 2019.

data_by_county <- read_rds("data_by_county")

no_geometry_county <- read_rds("no_geometry_county")

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
                    sep = "")
      ),
   
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(
           tabPanel(h3("About this Project"),
                    htmlOutput("text")),
           tabPanel(h3("Political Allegiance over Time"),
                    plotOutput("percents"), br(), plotOutput("reps"), br(), plotOutput("dems")), 
           tabPanel(h3("Florida by County: Political Allegiance & Demographics"),
                    plotlyOutput("map_fl")), br(), plotOutput("county_table"))
   )))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$text <- renderText({
    "<h4><b>See the next tab for some cool plots</h4></b> <br/><br/> 
    Using data from: <br/>

    <br/>*The United States Census Bureau 2000 and 2010 reports
    <br/>*The American Community Survey 5-Year (2013-2017) reports
    <br/>*Florida Department of State - Division of Elections Voter Registration
        <br/>-Registration reports by County (2019)
        <br/>-Registration reports by County and by Party (1972-2019)"})
   
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
     
     output$map_fl <- renderPlotly({ 
       ggplotly(ggplot(data = data_by_county, aes(text = paste(NAMELSAD, "<br>", "Major Party:", party_control, "<br>", "Foreign Born Population:", percent,"%", "<br>", "Median Family Income: $",dollar))) +
                  geom_sf(aes(fill = party_control)) +
                  theme_map() + theme_economist() + scale_fill_fivethirtyeight() +
                  labs(title = "County Partisanship by Majority of Registered Voters", fill = NULL) +
                  theme(
                    panel.grid.major = element_line(colour = 'transparent'), 
                    line = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    plot.background = element_rect(fill = "transparent")), tooltip = c("text"))
        })
     
     
     output$county_table <- renderPlot({
       
       no_geometry_county %>% 
       select(NAMELSAD, florida_democratic_party, republican_party_of_florida, party_control, percent, dollar) %>%
         mutate(percent = percent/100) %>% 
         gt() %>% 
         tab_header(title = "The Purple State in Numbers",
                    subtitle = "Politics & Selected Demographics in Florida by County") %>% 
         tab_spanner("Registered Voters", columns = vars(florida_democratic_party, republican_party_of_florida)) %>% 
         cols_label(NAMELSAD = "County",
                    florida_democratic_party = "Democrat",
                    republican_party_of_florida = "Republican",
                    party_control = "Dominant Party",
                    percent = "Percent of Foreign Born",
                    dollar = "Median Family Income") %>% 
         tab_footnote(footnote = "Florida's median annual income is $61,442 This is the distribution by County.",
                      locations = cells_column_labels(
                        columns = vars(dollar))) %>% 
         fmt_currency(columns = vars(dollar)) %>% 
         fmt_percent(columns = vars(percent), decimals = 1) %>% 
         fmt_number(columns = vars(florida_democratic_party, republican_party_of_florida),  decimals = 0)
       
       })
}

# Run the application 
shinyApp(ui = ui, server = server)

