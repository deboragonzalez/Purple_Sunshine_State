library(tidyverse)
library(readxl)
library(gt)
library(tigris)
library(janitor)
library(leaflet)
library(ggthemes)

data1 <- read_excel("Voter_Registration_By_County_and_Party_Feb_2019.xlsx") %>% 
  clean_names() %>% 
  head(-2) %>% 
  mutate(republican_party_of_florida = as.numeric(republican_party_of_florida)) %>% 
  mutate(florida_democratic_party = as.numeric(florida_democratic_party)) %>% 
  mutate(no_party_affiliation = as.numeric(no_party_affiliation)) %>% 
  mutate(total = as.numeric(totals)) %>% 
  write_rds()

# Data1 is a spreadsheet containing voter registration of FL voters by county
# and party as of Feb. 2019.


data2 <- read_excel("Voter_Registration_By_Party_Affiliation_Feb_2019.xlsx") %>% 
  clean_names() %>% 
  mutate(republican_party_of_florida = parse_number(republican_party_of_florida)) %>% 
  mutate(florida_democratic_party = parse_number(florida_democratic_party)) %>% 
  mutate(other_or_no_party_affiliation = parse_number(other_or_no_party_affiliation)) %>% 
  mutate(total = parse_number(total)) %>% 
  filter(year != 1991) %>% 
  write_rds(path = "party_affiliation_years")

# Data2 is a spreadsheet containing voter registration of FL voters by party as
# of Feb. 2019 from 1972 to 2019.


foreign_pop_county <- read_csv("Bureau_Downloads/PERCENT_OF_PEOPLE_WHO_ARE_FOREIGN_BORN_State_By_County 2013-2017/ACS_17_5YR_GCT0501.ST05_with_ann.csv", skip = 1) 
data3 <- foreign_pop_county[, ! str_detect(names(foreign_pop_county), pattern = "Margin of Error")] %>% 
  clean_names()

# This data shows the percent of foreign born individuals by county.


income_county <- read_csv("Bureau_Downloads/MEDIAN_FAMILY_INCOME/ACS_17_5YR_GCT1902.ST05_with_ann.csv", skip = 1)
data4 <- income_county[, ! str_detect(names(income_county), pattern = "Margin of Error")] %>% 
  clean_names()
# Median family income by county. 









# Political Allegiance over Time:

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


data2_percent <- data2 %>% 
  mutate(percent_dem = (florida_democratic_party/total)*100) %>% 
  mutate(percent_rep = (republican_party_of_florida/total)*100) %>% 
  mutate(percent_other = (other_or_no_party_affiliation/total)*100) 

data2_percent %>% 
  ggplot() + geom_line(aes(x =year, y = percent_rep), color = "red3") + 
  geom_line(aes(x = year, y = percent_dem), color = "blue4") 
  



