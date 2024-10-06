###########################################
# clean_mastercard_data.R
# This file creates datasets necessary for
# analysing the global and US mastercard data
###########################################
## Preliminaries
rm(list=ls())

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE);
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}
packages <- c("tidyverse", "zoo", "directlabels", "cowplot", "pracma", "lubridate", "readxl", "stringr")
lapply(packages, pkgTest);

setwd('~/Documents/donut-effect-pnas')

###########################################
#######CREATE MAIN DATASET########
###########################################
df = read_excel('./data/external_data/mastercard/updated-donut-results-10072023.xlsx') %>% 
  mutate(old_city_name = cleansed_city_name, distance_val = as.double(str_extract(distance_column, "^[^\\s]+")),
         distance_column = str_remove(distance_column, " \\w+$")) %>%
  rename(iso = iso3) %>% 
  write_csv('./data/donut-effect-main.csv')

df = read_csv('./data/external_data/mastercard/usa-donut-results.csv') %>% 
  mutate(old_city_name = cleansed_city_name, distance_val = as.double(str_extract(distance_column, "^[^\\s]+")),
         distance_column = str_remove(distance_column, " \\w+$")) %>%
  rename(iso = iso3) %>% 
  write_csv('./data/donut-effect-usa.csv')


###########################################
## Clean coordinates for US cities
###########################################
msa_chars = read_csv('./data/msa_all_chars.csv') %>% filter(!is.na(MsaName)) %>%
  separate(MetroShort, c("old_city_name", "state"), ', ', remove = FALSE) %>% 
  arrange(desc(`2019 Population`)) %>% slice_head(n = 100)

nhgis = read_csv('./data/external_data/nhgis0009_ds244_20195_cbsa.csv') %>%
    rename('cbsa_name' = 'CBSA', 'income_per_capita' = 'ALX5E001', 'population' = 'ALUBE001') %>%
    select('cbsa_name', 'income_per_capita', 'population') %>%
    mutate(
      MsaName = iconv(cbsa_name, to = "ASCII//TRANSLIT", sub = ""),
      MsaName = str_replace_all(cbsa_name, "[^[:alnum:], -]", "")
    ) %>%
    separate(MsaName, c("Metro", "end"), ', ', extra = "merge", fill = "right", remove = FALSE) %>%
    mutate(Metro = sub("-.*", "", Metro),
           MetroState = sub(" .*", "", end),
           MetroState = sub("-.*", "", MetroState),
           MetroShort = paste(Metro, MetroState, sep = ', '))

msa_chars2 = msa_chars %>% inner_join(nhgis, by = 'MetroShort') %>% 
  mutate(us_income_per_capita = 34103, #hard-coded from NHGIS 2015-2019 ACS 5-year estimate, 2019 inflation-adjusted dollars
         income_share = income_per_capita / us_income_per_capita,
         gdp_capita = income_share*65051.88) #hard-coded from city_chars.xlsx sheet with IMF GDP estimates

msa_chars2 %>% write_csv('./data/us_city_chars.csv')

###########################################
# Create global_city_chars.csv which has the following columnsd
# city_name, iso, country_name, gdp_capita_country, gdp_capita_city, population
###########################################
#cities = read_csv('./data/external_data/mastercard/donut-effect-main.csv') %>% distinct(old_city_name, iso, imf_classification) %>% write_csv('./data/mastercard_city_names.csv')
cities = read_csv('./data/mastercard_city_names.csv') %>% mutate(city = ifelse(is.na(new_city_name), old_city_name, new_city_name))

gdp = read_excel('./data/city_chars.xlsx', sheet='IMF_GDP_capita_PPP') %>% select(1, `2019`)
names(gdp) = c('country', 'gdp_ppp_2019')

cross = read_excel('./data/city_chars.xlsx', sheet='country_iso_cross', skip=1) %>% select(3, 9)
names(cross) = c('iso', 'country')

#read in worldcities.csv population data
city_pop = read_csv('./data/worldcities.csv') %>% select(!city) %>%
    rename(iso = iso3, city = city_ascii, population2=population) %>% select(city, iso, population2) %>%
    group_by(city, iso) %>%  # Group the data by city and iso
    slice(which.max(population2)) # Keep only the row with the maximum population

cities = cities %>% left_join(cross, by='iso') %>% left_join(gdp, by = 'country') %>% left_join(city_pop, by=c('city', 'iso'))

cities %>% write_csv('./data/global_city_chars.csv')


###########################################
# Create global-donut-spend-categories.csv
###########################################
df1 = read_excel('./data/external_data/mastercard/global-donut-spend-categories-1.xlsx') %>% filter(distance_column == '30 mile distance')
df2 = read_excel('./data/external_data/mastercard/global-donut-spend-categories-2.xlsx') %>% filter(distance_column == '30 mile distance')
df3 = read_excel('./data/external_data/mastercard/global-donut-spend-categories-3.xlsx') %>% filter(distance_column == '30 mile distance')
df = rbind(df1, df2, df3)
df %>% write_csv('./data/global-donut-spend-categories.csv')

