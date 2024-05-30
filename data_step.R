
library(dplyr)
library(readr)
library(lubridate)
# functions ----

get_mom_stats <- function(df) {
  
  df %>%
    arrange(ref_date) %>%
    mutate(mom_val = lag(value),
           mom_pct = ((value / lag(value, n = 1)) - 1) * 100,
           mom_chg = (value - lag(value, n = 1)))
}

get_yoy_stats <- function(df) {
  
  df %>%
    arrange(ref_date) %>%
    mutate(yoy_val = lag(value, n = 12),
           yoy_pct = ((value / lag(value, n = 12)) - 1) * 100,
           yoy_chg = (value - lag(value, n = 12)))
}

## static vector names ----
province_names <- data.frame(
  geo = c("Canada", "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick",
          "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta",  "British Columbia"),
  vector = c("v1446859483", "v1446859543", "v1446859574", "v1446859605", "v1446859636",
             "v1446859667", "v1446859789", "v1446859881", "v1446859942", "v1446859973", "v1446860064"))

sector_names <-   data.frame(
  sector = c("All Retail Trade",
             "Motor vehicle and parts dealers",
             "Furniture, home furnishings stores, electronics and appliances retailers",
             "Building material and garden equipment and supplies dealers",
             "Food and beverage retailers",
             "Health and personal care retailers",
             "Gasoline stations and fuel vendors",
             "Clothing and clothing accessories retailers",
             "Sporting goods, hobby, musical instrument, book retailers and news dealers",
             "General merchandise retailers",
             "Miscellaneous retailers"),
  vector = c("v1446860063", "v1446860065", "v1446860078", "v1446860071", "v1446860072", "v1446860084",
             "v1446860085", "v1446860087", "v1446860091", "v1446860083", "v1446860092"))

## chart theme/functions ----
# source("scripts/chart_theme.R")
# source("scripts/functions.R")




## get cansim data ----
provinces <- cansim::get_cansim_vector_for_latest_periods(vectors = c("v1446859483", "v1446859543", "v1446859574",
                                                                      "v1446859605", "v1446859636", "v1446859667",
                                                                      "v1446859789", "v1446859881", "v1446859942",
                                                                      "v1446859973", "v1446860064"), periods = 61) %>%
  mutate(REF_DATE = ymd(REF_DATE, truncated = 2)) %>%
  janitor::clean_names() %>%
  left_join(province_names, by = c("vector")) %>%
  group_by(geo) %>%
  get_mom_stats() %>%
  ungroup() %>%
  select(ref_date, geo, value, mom_pct)
provinces %>% write_csv("data/provinces.csv")
# provinces = read_csv("data/provinces.csv")
sectors <- cansim::get_cansim_vector_for_latest_periods(vectors = c("v1446860063", "v1446860065",
                                                                    "v1446860078", "v1446860071",
                                                                    "v1446860072", "v1446860084",
                                                                    "v1446860085", "v1446860087",
                                                                    "v1446860091", "v1446860083",
                                                                    "v1446860092"),
                                                        periods = 61) %>%
  mutate(REF_DATE = ymd(REF_DATE, truncated = 2)) %>%
  janitor::clean_names() %>%
  left_join(sector_names, by = c("vector")) %>%
  group_by(vector) %>%
  get_yoy_stats() %>%
  ungroup() %>%
  select(ref_date, sector, value, yoy_pct)
sectors %>% write_csv("data/sectors.csv")
# sectors = read_csv("data/sectors.csv")

latestDate <- provinces %>% summarise(date = max(ref_date)) %>%
  mutate(date = paste(month(date, label = TRUE, abbr = TRUE), year(date), sep = " ")) %>%
  pull()