library(tidyverse)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(ggthemes)
library(maps)
library(data.table)
col_covid <- list(
  dateRep = col_date("%d/%m/%Y"),
  day = col_double(),
  month = col_double(),
  year = col_double(),
  cases = col_double(),
  deaths = col_double(),
  countriesAndTerritories = col_character(),
  geoId = col_character(),
  countryterritoryCode = col_character(),
  popData2019 = col_double(),
  continentExp = col_character(),
  `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000` = col_double()
)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", col_types = col_covid)


covid_data_monthly_tbl<-covid_data_tbl%>%  
  select(dateRep, cases, countriesAndTerritories, continentExp) %>%
  mutate(month = month(dateRep)) %>%
  filter(countriesAndTerritories %in% c("Germany", "United_Kingdom" , "France" , "Spain", "United_States_of_America")) %>%
  arrange(countriesAndTerritories,year(dateRep),month,date(dateRep))%>%
  group_by(countriesAndTerritories) %>%
  mutate(cum_cases=cumsum(cases)) %>%
  ungroup()

max_values <- covid_data_monthly_tbl %>% 
  filter(countriesAndTerritories == "United_States_of_America")%>%
  slice_max(cum_cases)


covid_data_monthly_tbl %>%    ggplot(aes(x = date(dateRep), y = cum_cases, color = countriesAndTerritories)) +
  geom_line(size = 1)+
  expand_limits(y = 3e6) +
  theme_economist() +
  theme(
    legend.position = "bottom" ,
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")) +
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "As of 11/02/2020 Europe had more cases than USA ",
    x = "Year 2020",
    y = "Cumulative Cases",
    color = "Country"
  )  + geom_label_repel(aes(x = dateRep, y = cum_cases, label = cum_cases),
                        data = max_values,
                        show.legend = F,
                        size  = 5,
                        fill  = "#1f78b4",
                        color = "white",
                        fontface = "italic")



###############

world <- map_data("world")
set_data<- covid_data_tbl  %>% 
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  ))
setnames(set_data,"countriesAndTerritories","region")
covid_deathrate_tbl<-set_data %>%  
  select(deaths, region, popData2019) %>%
  group_by(region) %>%
  mutate(death_rate=(sum(deaths)/popData2019)*100) %>%
  ungroup() 
covid_data_deathrate_tbl<-aggregate(x= covid_deathrate_tbl$death_rate,
          by= list(covid_deathrate_tbl$region),
          FUN=max)
setnames(covid_data_deathrate_tbl,"Group.1","region")
setnames(covid_data_deathrate_tbl,"x","death_rate")

plot_data<-merge(x = world, y = covid_data_deathrate_tbl, 
                 by    = "region", 
                 all.x = TRUE, 
                 all.y = FALSE) %>% mutate(death_rate = scales::dollar(death_rate, 
                                                                       big.mark     = ".", 
                                                                       decimal.mark = ",", 
                                                                       prefix       = "", 
                                                                       suffix       = " %"))

ggplot(plot_data, aes(fill = death_rate)) +
  geom_map(aes(map_id = region), map = world)+
  expand_limits(x = plot_data$long, y = plot_data$lat)+
  labs(
    title = "Confirmed COVID-19 deaths relative to size of the population",
    subtitle = "More than 1.2 Million confirmed COVID-19 deaths worldwide ",
    x = "Longitude",
    y = "Latitude"
  )
