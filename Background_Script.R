# Florida's Population & Politics Project
# By: Debora Gonzalez


# These libraries are necessary for the functions and themes used in this app.

library(tidyverse)

# General functions to organize data.

library(readxl)

# Necessary to read in the data.

library(gt)

# To be used in designing and formatting a table.

library(tigris)

# Contains the map files used for the map portion of the app

library(ggthemes)

# Contains the theme I want to use to design my plots/graphs.

library(sf)

# Allows me to work with the Tigris map files.

library(fivethirtyeight)

# Contains the party color schemes I want to use to color my graphics.

library(plotly)

# Provides helpful mapping tools with interactive tooltip.

library(janitor)

# Will help to clean out data before it can be used/joined.


data1 <- read_excel("Data/Voter_Registration_By_County_and_Party_Feb_2019.xlsx") %>% 
  clean_names() %>% 
  head(-2) %>% 
  mutate(republican_party_of_florida = as.numeric(republican_party_of_florida)) %>% 
  mutate(florida_democratic_party = as.numeric(florida_democratic_party)) %>% 
  mutate(no_party_affiliation = as.numeric(no_party_affiliation)) %>% 
  mutate(totals = as.numeric(totals)) %>% 
  mutate(county = str_trim(county))

# Data1 is a spreadsheet containing voter registration of FL voters by county
# and party as of Feb. 2019. 


data2 <- read_excel("Data/Voter_Registration_By_Party_Affiliation_Feb_2019.xlsx") %>% 
  clean_names() %>% 
  mutate(republican_party_of_florida = parse_number(republican_party_of_florida)) %>% 
  mutate(florida_democratic_party = parse_number(florida_democratic_party)) %>% 
  mutate(other_or_no_party_affiliation = parse_number(other_or_no_party_affiliation)) %>% 
  mutate(total = parse_number(total)) %>% 
  filter(year != 1991) %>% 
  write_rds(path = "party_affiliation_years")

# Data2 is a spreadsheet containing voter registration of FL voters by party as
# of Feb. 2019 from 1972 to 2019.


foreign_pop_county <- read_csv("Data/PERCENT_OF_PEOPLE_WHO_ARE_FOREIGN_BORN_State_By_County 2013-2017/ACS_17_5YR_GCT0501.ST05_with_ann.csv", skip = 1)
data3 <- foreign_pop_county[, ! str_detect(names(foreign_pop_county), pattern = "Margin of Error")] %>% 
  clean_names() %>% 
  tail(-1) %>% 
  select(geographic_area_1, percent)

# Data3 shows the percent of foreign born individuals by county.


income_county <- read_csv("Data/MEDIAN_FAMILY_INCOME/ACS_17_5YR_GCT1902.ST05_with_ann.csv", skip = 1)
data4 <- income_county[, ! str_detect(names(income_county), pattern = "Margin of Error")] %>% 
  clean_names() %>% 
  tail(-1) %>% 
  select(geographic_area_1, dollar)

# Data4 shows the median family income by county. 





# Political Allegiance over Time - Bar plots using Data2:

# Republican

data2 %>% 
  ggplot(aes(x = year, y = republican_party_of_florida)) + 
  geom_bar(stat="identity", fill = "red", colour = "black") +
  labs(y = NULL,
       x = "Year Range",
       title = "Number of Registered Republicans over Time")

# Democrat

data2 %>% 
  ggplot(aes(x = year, y = florida_democratic_party)) + 
  geom_bar(stat="identity", fill = "blue", colour = "black") +
  labs(y = NULL,
       x = "Year Range",
       title = "Number of Registered Democrats over Time")


# Percent change over time - Line graph using Data2:

data2_percent <- data2 %>% 
  mutate(percent_dem = (florida_democratic_party/total)*100) %>% 
  mutate(percent_rep = (republican_party_of_florida/total)*100) %>% 
  mutate(percent_other = (other_or_no_party_affiliation/total)*100) 

      # The part above is to be added to the output (server) portion right
      # before plotting in the shiny app. It prepares the data for plotting the
      # percentage of voters in the given party over time.

data2_percent %>% 
  ggplot() + geom_line(aes(x =year, y = percent_rep), color = "red3") + 
  geom_line(aes(x = year, y = percent_dem), color = "blue4") +
  geom_line(aes(x = year, y = percent_other), color = "green") 

  # This is a rough draft of the percentage of registered voters by party line
  # graph I will show in my app. Very simple procedure. In the app, I will call
  # the dataframe inside the ggplot.
  


# Map files
# Must convert Data1, Data3, and Data4 to sf files, but will do so by doing a
# left join with the raw_shape files filtered for Florida. The datasets will be
# joined by county.

raw_shapes_counties <- counties(class = "sf")

# Using the counties call gives me a dataset with all the counties in the U.S. I
# ensure I get the sf file in order to use the map.

shapes_FL <- raw_shapes_counties %>% 
  filter(STATEFP == "12")

# In the dataframe I looked for Miami, identified Miami Dade county. Then looked
# for the state variable, and set it equal to the number for FL.


data1_map <- left_join(shapes_FL, data1, by = c("NAME" = "county")) %>% 
  mutate(party_control = if_else(republican_party_of_florida > florida_democratic_party, "Republican", "Democrat")) 

# This will create a new dataset using the shape files and the number of
# registered voters by county.I set name and county equal to each other. To do
# this, I ensured earlier that the character values in each varibale were clean
# (no additional spaces) and denominated as character values. The variable
# party_control will help me to later draw up a map and color it by party
# majority in each county.
  
data3_4 <- left_join(data3, data4, by = c("geographic_area_1" = "geographic_area_1")) 

  # Percent is the percent of people born outside of the U.S. and dollar is the
  # median income per family. These two datasets were from the Census Bureau,
  # and shared the same values, which facilitates the joining. This will put the
  # the geographic_area_1 variable (county name), on one single column followed
  # by both percent and dollar. Now, we can join it to our first joint dataset
  # and have all three datasets ready to map in one.

data1_3_4_map <- left_join(data1_map, data3_4, by = c("NAMELSAD" = "geographic_area_1")) %>% 
  write_rds("data_by_county")

# For this one again, we make sure that NAMELSAD (the county name variable that
# includes -- County like the data from the census) is clean and a character
# value. Once we put it together we can use write_rds to create the file we need
# in the app. I will then place a copy of that rds file into my app folder. 

  
# Map rough draft

ggplotly(ggplot(data = data1_3_4_map, aes(text = paste(NAMELSAD, "<br>", "Major Party:", party_control, "<br>", "Foreign Born Population:", percent,"%", "<br>", "Median Family Income: $",dollar))) +
  geom_sf(aes(fill = party_control)) +
  theme_map() + theme_economist() + scale_fill_fivethirtyeight() +
  labs(title = "County Partisanship by Majority of Registered Voters", fill = NULL) +
  theme(
    panel.grid.major = element_line(colour = 'transparent'), 
    line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "transparent")), tooltip = c("text"))

# This map is the initial rought draft for the maps I will use in my app. After
# calling the data in ggplot, I can use text inside aes() to lay out what I want
# on the tooltip hover. The text to be displayed (as the variable's label) is
# written in HTML syntax following the variable name itself. Theme_map and
# Theme_economist gives my app a thick and professional aesthetic. Since I set
# fill = to party_control, the scale_fill_fivethirtyeight() is perfect because
# it will color according to party with professionally used colors for party
# representation. This theme setting get rid of the plot grid, the map and axis
# ticks, and gets rid of background colors.



# The map in numbers:

without_geometry <- as_tibble(data1_3_4_map) %>% 
  mutate(geometry = NULL) %>% 
  write_rds("no_geometry_county")

# This part simply prepares the by county dataframe without the geometry
# variable in order to create a gt table of it. It sets the earlier 3-joint
# dataframe to a tibble using as_tibble, and setting geometry = NULL. Then, I
# use rds_write to create the r file and put a copy of it in my app file.


# Rough Draft of fully formatted gt table.

without_geometry %>% 
  select(NAMELSAD, florida_democratic_party, republican_party_of_florida, party_control, percent, dollar) %>%
  mutate(percent = percent/100) %>% 
  
    # This ensures that when I add fmt_percent to format the number as
    # percentage, the proper percent value is displayed. The numbers were
    # already in their percent value, but were not formatted.
  
  gt() %>% 
  tab_header(title = "The Purple State in Numbers",
             subtitle = "Politics & Selected Demographics in Florida by County") %>% 
  tab_spanner("Registered Voters", columns = vars(florida_democratic_party, republican_party_of_florida)) %>% 
  
    # This allows me to create a column group and assign it a name.
  
  cols_label(NAMELSAD = "County",
             florida_democratic_party = "Democrat",
             republican_party_of_florida = "Republican",
             party_control = "Dominant Party",
             percent = "Percent of Foreign Born",
             dollar = "Median Family Income") %>% 
        
            # Renaming variables for aesthetic and clarity purposes.
  
  tab_footnote(footnote = "Florida's median annual income is $61,442 This is the distribution by County.",
               locations = cells_column_labels(
               columns = vars(dollar))) %>% 
  
          # Adds a footnote tag to the dollar variable in order to add useful
          # information to help the viewer assess the data.
  
  fmt_currency(columns = vars(dollar)) %>% 
  fmt_percent(columns = vars(percent), decimals = 1) %>% 
  fmt_number(columns = vars(florida_democratic_party, republican_party_of_florida),  decimals = 0)
    
    # Formats the data columns by currency, percent and clean number values
    # respectively.


  
# Florida's median annual income is $61,442 This is the distribution by County. 


 


