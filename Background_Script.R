library(tidyverse)
library(readxl)
library(gt)
library(tigris)
library(janitor)
library(leaflet)
library(ggthemes)
library(sf)
library(fivethirtyeight)
library(plotly)


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
      # before plotting in the shiny app.

data2_percent %>% 
  ggplot() + geom_line(aes(x =year, y = percent_rep), color = "red3") + 
  geom_line(aes(x = year, y = percent_dem), color = "blue4") +
  geom_line(aes(x = year, y = percent_other), color = "green") 
  


# Map files
# Must convert Data1, Data3, and Data4 to sf files, but will do so by doing a
# left join with the raw_shape files filtered for Florida. The datasets will be
# joined by county.

raw_shapes_counties <- counties(class = "sf")

shapes_FL <- raw_shapes_counties %>% 
  filter(STATEFP == "12")


data1_map <- left_join(shapes_FL, data1, by = c("NAME" = "county")) %>% 
  mutate(party_control = if_else(republican_party_of_florida > florida_democratic_party, "Republican", "Democrat")) 
  
data3_4 <- left_join(data3, data4, by = c("geographic_area_1" = "geographic_area_1")) 

  # Percent is the percent of people born outside of the U.S. and dollar is the
  # median income per family.

data1_3_4_map <- left_join(data1_map, data3_4, by = c("NAMELSAD" = "geographic_area_1")) %>% 
  write_rds("data_by_county")

  

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


# This chart in numbers:

  

without_geometry <- as_tibble(data1_3_4_map) %>% 
  mutate(geometry = NULL) %>% 
  write_rds("no_geometry_county")

without_geometry %>% 
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

# look into shiny themes, and 

# fill ="Majority Party Affiliation"
  
# Florida's median annual income is $61,442 This is the distribution by County. 


 


