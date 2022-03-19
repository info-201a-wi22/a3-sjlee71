library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(reshape2)


incarceration_trends <- read.csv(file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

total_jail_admin <- incarceration_trends %>% select(year, state, county_name, total_jail_adm)

county_jail_admin <- total_jail_admin %>% group_by(year) %>% arrange(year) %>% 
  unite("year, state", year:state, sep = "_", remove = FALSE)

county_jail_admin[is.na(county_jail_admin)] <- 0

state_jail_admin <- county_jail_admin %>% group_by(year, state) %>% summarise(jail_admissions = sum(total_jail_adm)) 

###

total_jail_dish <- incarceration_trends %>% select(year, state, county_name, total_jail_dis)

county_jail_dish <- total_jail_dish %>% group_by(year) %>% arrange(year) %>%
  unite("year, state", year:state, sep = "_", remove = FALSE)

county_jail_dish[is.na(county_jail_dish)] <- 0


state_jail_dish <- county_jail_dish %>% group_by(year, state) %>% summarise(jail_discharges = sum(total_jail_dis)) 

###

state_adm_dis <- left_join(state_jail_admin, state_jail_dish)

#variable comparison chart
top_state_adm_dis <- state_adm_dis %>% arrange(desc(jail_admissions, jail_discharges)) %>%
  group_by(year) %>% slice(1:10)

year_filter <- function(chosenyear){
  
  filter_date <- top_state_adm_dis %>% filter(grepl(chosenyear, year)) %>% 
    ungroup() %>% select(-year) 
  
  return(filter_date)
  
}

chosenyear <- "1978"
variable_chart <- year_filter(chosenyear)

#top_adm_dis <- state_adm_dis %>% group_by(year) %>% summarise(across(starts_with('jail'), sum)) %>%
#slice(39:49)


filter_adm_dis <- melt(variable_chart, id = "state")

chart_Var <- ggplot(filter_adm_dis, aes(state, value, fill = variable)) + 
  geom_bar(position="dodge", stat="identity") + ggtitle("Jail Admission vs Discharge Counts in:", chosenyear) +
  labs(y = "Jail Population Count", x = "Top 10 States (US)", fill = "Admission/Discharge")

#  race_jail_pop <- incarceration_trends %>% select(year, state, aapi_jail_pop, black_jail_pop, latinx_jail_pop,
#                                                 native_jail_pop, white_jail_pop, other_race_jail_pop)
#Prison

race_prison_pop <- incarceration_trends %>% select(year, state, aapi_prison_pop, black_prison_pop, latinx_prison_pop,
  native_prison_pop, white_prison_pop, other_race_prison_pop) 

race_prison_pop[is.na(race_prison_pop)] <- 0
#Trends over time chart
total_race_prison_pop <- race_prison_pop %>% group_by(year) %>% summarise(across(ends_with('pop'), sum)) %>% slice(37:47)

long_race_prison_pop <- melt(total_race_prison_pop, id = "year")

ggplot(long_race_prison_pop, aes(year, value, color = variable)) + 
  geom_line() +
  labs(title = "Prison Population Counts (Race)" ,y = "Prison Population Count", x = "Years")

#map
states <- map_data("state")

map_chart <- variable_chart
map_chart <- data.frame(state.name[match(map_chart,state.abb)])

#test <- state.name[match(variable_chart,state.abb)])
colnames(test)[1] <- ("state_")



new_variable_chart <- rbind(test, variable_chart)
