###########################################
# persistence.R
# This script reads in the cleaned USPS and Zillow 
# files, and creates figures for the paper
# note: creates density groups for each metro individually
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

## These lines load the required packages
packages <- c("tidyverse", "zoo", "directlabels", "cowplot", "pracma", 
              "lubridate", "readxl", "stringr", "forecast", "broom")
lapply(packages, pkgTest);

cbd_radius = 3218.69 #2 mile cbd radius in meters

#define colors
black <- "#2E2D29"; cardinal <- "#B1040E"; teal <- "#66b2b2"; green <- "#228B22"; marmalade <- "#d16002"
orange <- "#FFAE42"; magenta <- "#8B008B"; purple <- "#800080"; blue <- "#0000FF"; red <- "#FF0000"
options(repr.plot.width=10, repr.plot.height=8)

#date rage for figures
start_date='2018-01-01'
end_date = '2023-12-01'
end_date_long = '2024-07-01'

## start and end date for for cumulations
start_period = '2017-06-01' #start period of cumulation ending in 2020-03-01 exclusive
end_period = '2022-12-01' #end period for cumulation starting in 2020-03-01 inclusive

#threshold percentile for donut
thresh = 0.5

#set to your working directory
setwd('~/Documents/donut-effect-pnas/')

#######################
###Mastercard global###
#######################

df = read_csv('./data/donut-effect-main.csv') %>% mutate(date = as.Date(date)) %>%
  select(old_city_name, iso, date, donut_effect_cumulative, distance_val) %>%
  filter(as.Date(date) >= as.Date("2023-01-01"))
city_chars=read_csv('./data/global_city_chars.csv') 
df2 = df %>% select(old_city_name, iso, date, distance_val, donut_effect_cumulative) %>% 
  left_join(city_chars, by=c('old_city_name', 'iso')) %>% filter(!is.na(country), distance_val %in% c(40)) %>%
  select(iso, date, distance_val, donut_effect_cumulative, city, gdp_ppp_2019, population2)

# Calculate average donut_effect_cumulative
avg2023 <- df2 %>%
  group_by(city, distance_val) %>%
  summarise(avg_2023 = mean(donut_effect_cumulative, na.rm = TRUE))

# Run regression and extract results
regression_results <- df2 %>%
  group_by(city, distance_val) %>%
  do(tidy(lm(donut_effect_cumulative ~ date, data = .))) %>%
  filter(term == "date") %>%
  select(city, distance_val, slope = estimate, t_stat = statistic, p_value = p.value)

# Combine results
final_table <- left_join(avg2023, regression_results, by = "city") %>% 
  mutate(not_positive = ifelse((slope <= 0) | (p_value > 0.1), 1, 0))
final_table$less_five = ifelse(final_table$avg_2023 < quantile(final_table$avg_2023, thresh), 1, 0)
final_table = final_table %>% mutate(
  persisters = ifelse(not_positive & less_five, 1, 0)) %>%
  arrange(-1*persisters)

print(table(final_table$less_five))
# 56/113 cities are persisters
print(quantile(final_table$avg_2023, thresh))
#-12.89 is 50th percentile

final_table %>% write_csv('./data/output/mastercard_persistence_table.csv')

######################
#########USPS#########
######################

#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% dplyr::select(!estab_count) %>%
  filter(!is.na(dist_to_cbd)) %>% mutate(county_fips = as.double(state)*1000+as.double(county))

usps <- read_csv('./data/USPS_zips.csv') #read in zip code level USPS flow data

#get county (zip) level gdp capita
crosswalk = read_csv('./data/external_data/cbsa2fipsxw.csv') %>%
  rename('county_fips' = 'fipscountycode', 'state_fips' = 'fipsstatecode', 'county_name' = 'countycountyequivalent', 'state_name' = 'statename', 'cbsa_code' = 'cbsacode') %>%
  mutate(county_fips = state_fips*1000+county_fips) %>%
  select('county_fips', 'state_fips', 'cbsa_code')
nhgis = read_csv('./data/external_data/nhgis0009_ds244_20195_cbsa.csv') %>%
  rename('cbsa_code' = 'CBSAA', 'cbsa_name' = 'CBSA', 'income_per_capita' = 'ALX5E001', 'population' = 'ALUBE001') %>%
  select('cbsa_code', 'cbsa_name', 'income_per_capita', 'population')

chars2 = chars %>% left_join(crosswalk, by = 'county_fips') %>% left_join(nhgis, by='cbsa_code') %>%
  group_by(MetroShort) %>% summarise(income_per_capita = mean(income_per_capita, na.rm=TRUE),
                                                population = sum(`2019 Population`, na.rm=TRUE))

## A. population flows
#construct dataset
temp <- chars %>% filter(!is.na(`2019 Population`)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MetroShort) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= cbd_radius, ntile(density2019[dist_to_cbd>=cbd_radius], 10)),
         category = ifelse(quantile_rank == 'cbd', 'center', 'outside')) %>%
  inner_join(usps, by = 'zip') %>% mutate(date = as.Date(date)) %>% group_by(MetroShort, category, date) %>%
  summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
  filter(date >= as.Date('2018-01-01'), category %in% c('center', 'outside'))

counts = temp %>% group_by(MetroShort, category) %>% summarise(n = n_distinct(date)) %>%
  group_by(MetroShort) %>% summarise(n = sum(n)) %>% filter(n>=132)

temp_filtered = temp %>% semi_join(counts, by = "MetroShort") %>%
  group_by(MetroShort, category) %>%
  mutate(net_pop_avg = zoo::rollmean(net_pop, 12, align='right', na.pad=TRUE),
         net_pop = cumsum(net_pop - net_pop_avg[date == as.Date('2020-02-15')]),
         net_pop = net_pop - net_pop[date==as.Date('2020-02-15')]) %>%
  pivot_wider(id_cols = c(MetroShort, date), names_from = category, values_from = net_pop) %>%
  mutate(dif = `center` - outside) 

temp_filtered_inc = temp_filtered %>% inner_join(chars2, by = 'MetroShort') %>%
  filter(!is.na(dif), !is.na(income_per_capita), !is.na(population)) %>%
  filter(date >= as.Date('2023-01-01'))

# Calculate average donut_effect_cumulative
avg2023_usps <- temp_filtered_inc %>%
  group_by(MetroShort) %>%
  summarise(avg_2023 = mean(dif, na.rm = TRUE))

# Run regression and extract results
regression_results_usps <- temp_filtered_inc %>%
  group_by(MetroShort) %>%
  do(tidy(lm(dif ~ date, data = .))) %>%
  filter(term == "date") %>%
  select(MetroShort, slope = estimate, t_stat = statistic, p_value = p.value)

# Combine results
final_table2 <- left_join(avg2023_usps, regression_results_usps, by = "MetroShort") %>% 
  mutate(not_positive = ifelse((slope <= 0) | (p_value > 0.1), 1, 0))
final_table2$less_five = ifelse(final_table2$avg_2023 < quantile(final_table2$avg_2023, thresh), 1, 0)
final_table2 = final_table2 %>% mutate(persisters = ifelse(not_positive & less_five, 1, 0)) %>%
  arrange(-1*persisters)
  

print(table(final_table2$persisters))
# 100/229 cities are persisters 
print(quantile(final_table2$avg_2023, thresh))
#-0.38 is 50th percentile

final_table2 %>% write_csv('./data/output/usps_persistence_table.csv')

######################
####Mastercard USA####
######################

# US mastercard rankings
df = read_csv('./data/donut-effect-usa.csv') %>% mutate(date = as.Date(date)) %>%
  select(old_city_name, iso, date, donut_effect_cumulative, distance_val) %>%
  filter(as.Date(date) >= as.Date("2023-01-01"))
city_chars=read_csv('./data/us_city_chars.csv') 

df2 = df %>% select(old_city_name, iso, date, distance_val, donut_effect_cumulative) %>% 
  left_join(city_chars, by=c('old_city_name')) %>% filter(distance_val %in% c(40)) %>%
  select(iso, date, distance_val, donut_effect_cumulative, MetroShort, income_per_capita, `2019 Population`)

# Calculate average donut_effect_cumulative
avg2023 <- df2 %>%
  group_by(MetroShort, distance_val) %>%
  summarise(avg_2023 = mean(donut_effect_cumulative, na.rm = TRUE))

# Run regression and extract results
regression_results <- df2 %>%
  group_by(MetroShort, distance_val) %>%
  do(tidy(lm(donut_effect_cumulative ~ date, data = .))) %>%
  filter(term == "date") %>%
  select(MetroShort, distance_val, slope = estimate, t_stat = statistic, p_value = p.value)

# Combine results
final_table3 <- left_join(avg2023, regression_results, by = "MetroShort") %>% 
  mutate(not_positive = ifelse((slope <= 0) | (p_value > 0.1), 1, 0))
final_table3$less_five = ifelse(final_table3$avg_2023 < quantile(final_table3$avg_2023, thresh), 1, 0)
final_table3 = final_table3 %>% mutate(persisters = ifelse(not_positive & less_five, 1, 0))

print(table(final_table3$persisters))
# 43/98 cities are persisters
print(quantile(final_table3$avg_2023, thresh))
#-1.63 is 50th percentile

final_table3 %>% write_csv('./data/output/mastercard_usa_persistence_table.csv')

######################
########Zillow########
######################
#read zip code level home value index for single family homes from Zillow
df <- read_csv('https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1691449278', 
                col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip), names_to = 'date', values_to = 'zhvi') %>%
  filter(date >= as.Date(start_date), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(zhvi = na.approx(zhvi, na.rm=FALSE),
                           triple = ifelse(zhvi/lag(zhvi, 12) > 3, 1, 0),
                           count_na = sum(is.na(zhvi)),
                           num_vals=n()) %>%
  group_by(zip) %>% filter(all(triple==0 | is.na(triple)), 
                           count_na/num_vals<.05) ### filter out rows with 3x growth in rents (happens when there are several consecutive missing observations that zillow imputes)

#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv')

#construct dataset
temp <- chars %>% filter(!is.na(`2019 Population`), !is.na(dist_to_cbd)) %>% 
  mutate(quantile_rank = 'cbd') %>% group_by(MetroShort) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= cbd_radius, ntile(density2019[dist_to_cbd>=cbd_radius], 10)),
         category = ifelse(quantile_rank =='cbd', 'center', 'outside')) %>%
  inner_join(df, by = c('zip')) %>% 
  mutate(date = as.Date(date),
         zhvi_pop = zhvi*`2019 Population`) %>%
  group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE)) %>% 
  group_by(MetroShort, category, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(zhvi = zhvi_pop/population) %>%
  group_by(MetroShort, category) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2020-02-29')]*100) %>%
  pivot_wider(id_cols = c(MetroShort, date), names_from = category, values_from = zhvi) %>%
  mutate(dif = `center` - outside) %>% 
  filter(date >= as.Date('2023-01-01'))

# Calculate average donut_effect_cumulative
avg2023_zillow <- temp %>%
  group_by(MetroShort) %>%
  summarise(avg_2023 = mean(dif, na.rm = TRUE))

# Run regression and extract results
regression_results_zillow <- temp_filtered_inc %>%
  group_by(MetroShort) %>%
  do(tidy(lm(dif ~ date, data = .))) %>%
  filter(term == "date") %>%
  select(MetroShort, slope = estimate, t_stat = statistic, p_value = p.value)

# Combine results
final_table4 <- left_join(avg2023_zillow, regression_results_zillow, by = "MetroShort") %>% 
  mutate(not_positive = ifelse((slope <= 0) | (p_value > 0.1), 1, 0)) %>%
  filter(!is.na(avg_2023), !is.na(slope))
final_table4$less_five = ifelse(final_table4$avg_2023 < quantile(final_table4$avg_2023, thresh), 1, 0)
final_table4 = final_table4 %>% mutate(persisters = ifelse(not_positive & less_five, 1, 0))

print(table(final_table4$persisters))
# 75/228 cities are persisters

final_table4 %>% write_csv('./data/output/zillow_persistence_table.csv')

###### Combine into one
final_table_all = final_table2 %>% inner_join(final_table3, by='MetroShort', suffix=c('_USPS', '_Mcard')) %>%
  inner_join(final_table4, by='MetroShort') %>% 
  mutate(persisters_all = (persisters_USPS + persisters_Mcard + persisters)/3) %>%
  arrange(-1*persisters_all)

print(table(final_table_all$persisters_USPS))
print(table(final_table_all$persisters_Mcard))
print(table(final_table_all$persisters))
print(table(final_table_all$persisters_all))
