###########################################
# donut_tables.R
# This script creates Tables S1-3
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

## Create function to get robust standard errors
rse <- function(reg) { 
  return(as.vector(summary(reg, robust = T)$coefficients[,"Std. Error"]))
} 

## Create function to get clustered standard errors at the iso level
cle_iso <- function(model) {
  cl_se = vcovCL(model, cluster = ~ iso) #get clustered covariance matrix
  return(sqrt(diag(cl_se))) #get diagonal and take square root to get std. errors
}

## Create function to get clustered standard errors
cle <- function(model) {
  cl_se = vcovCL(model, cluster = ~ MetroShort) #get clustered covariance matrix
  return(sqrt(diag(cl_se))) #get diagonal and take square root to get std. errors
}

## These lines load the required packages
packages <- c("tidyverse",  "pracma", "lubridate", "stargazer", "sjstats",
              "readxl", "stringr", "forecast", "lmtest", "sandwich")
lapply(packages, pkgTest);

cities <- c('San Francisco, CA', 'New York, NY', 'Chicago, IL', 'Boston, MA',
            'Los Angeles, CA', 'Washington, DC', 'Atlanta, GA', 'Miami, FL',
            'Philadelphia, PA', 'Dallas, TX', 'Houston, TX', 'Phoenix, AZ')

#set to your working directory
setwd('~/Documents/donut-effect-pnas/')

####################################
# Table S1a. Mastercard USA
####################################
df = read_csv('./data/external_data/mastercard/donut-effect-usa.csv') %>% 
  mutate(date = as.Date(date)) %>% filter(distance_column == '30 mile')
city_chars=read_csv('./data/us_city_chars.csv')

df = df %>% select(old_city_name, iso, date, distance_column, donut_effect_ring, donut_effect_cumulative) %>%
  inner_join(city_chars, by='old_city_name') %>%
  rename(city = old_city_name)

wfh = read_excel('./data/external_data/remote_work_in_job_ads_signup_data.xlsx', sheet = 'us_city_by_month') %>%
  mutate(MetroShort = paste(city, state, sep = ", ")) %>%
  filter(year_month >= 2023, measurement == '1 Month Average') %>%
  group_by(MetroShort) %>% summarise(wfh_share = mean(percent, na.rm=TRUE)/100)

df2 = df %>% left_join(wfh, by='MetroShort')

# July 2022 to June 2023 donut index, average
df_post = df2 %>% filter(date >= as.Date("2022-10-01"), date < as.Date('2023-10-01')) %>%
  group_by(MetroShort) %>% summarise(wfh_share = mean(wfh_share, na.rm=TRUE),
                                     wfh_emp = mean(wfh_emp, na.rm=TRUE),
                                     gdp_capita = mean(gdp_capita, na.rm=TRUE)/1000,
                                     density2019 = mean(density2019, na.rm=TRUE)/1000,
                                     population = mean(population, na.rm=TRUE),
                                     donut_post = mean(donut_effect_cumulative, na.rm=TRUE))

# July 2022 to June 2023 donut index, average
df_pre = df2 %>% filter(date >= as.Date("2018-10-01"), date < as.Date('2019-10-01')) %>%
  group_by(MetroShort) %>% summarise(donut_pre = mean(donut_effect_cumulative, na.rm=TRUE))

df_post = df_post %>% inner_join(df_pre, by = 'MetroShort')

m0 <- lm(donut_post ~ donut_pre + log(density2019), data = df_post, weights=df_post$population)
#m1 <- lm(donut_post ~ donut_pre + wfh_emp, data = df_post, weights=df_post$population)
m1 <- lm(donut_post ~ donut_pre + log(gdp_capita), data = df_post, weights=df_post$population)
m2 <- lm(donut_post ~ donut_pre + log(wfh_share), data = df_post, weights=df_post$population)
m3 <- lm(donut_post ~ donut_pre + log(density2019) + log(gdp_capita) + log(wfh_share), data = df_post, weights=df_post$population)

####################################
# Table S1b. Mastercard global
####################################
df = read_csv('./data/external_data/mastercard/donut-effect-main.csv') %>% 
  mutate(date = as.Date(date)) %>% filter(distance_column == '30 mile')
city_chars=read_csv('./data/global_city_chars.csv') %>% 
  #group_by(iso) %>% slice_max(order_by = population2, n = 1) %>% 
  select(iso, old_city_name, country, gdp_ppp_2019, population2)

df = df %>% select(old_city_name, iso, date, distance_column, donut_effect_ring, donut_effect_cumulative) %>%
  inner_join(city_chars, by=c('old_city_name', 'iso')) %>%
  rename(city = old_city_name)

oxf = read_csv("~/Documents/zillow/thesis/data/global-donut-mastercard/oxf_lockdown.csv") %>%
  rename(country=country_name) %>% group_by(country) %>% 
  summarise(cumulative_lockdown = sum(lockdown_index, na.rm=TRUE))

wfh = read_excel('./data/external_data/GSWA-Figures-Data-2023.xlsx', 
                 sheet='Figure 1', range="A1:C36") %>% rename(wfh_days = `Number of days working from home this week`) %>%
  select(iso, wfh_days)

df2 = df %>% left_join(wfh, by='iso') %>% left_join(oxf, by = "country")

# July 2022 to June 2023 donut index, average
df_post = df2 %>% filter(date >= as.Date("2022-10-01"), date < as.Date('2023-10-01')) %>%
  group_by(city, iso) %>% summarise(gdp_capita=mean(gdp_ppp_2019)/1000, 
                                    donut_post=mean(donut_effect_cumulative, na.rm=TRUE),
                                    cumulative_lockdown = mean(cumulative_lockdown, na.rm = TRUE)/1000,
                                    wfh_share = mean(wfh_days, na.rm=TRUE)/5,
                                    population2 = mean(population2, na.rm=TRUE))

df_pre = df2 %>% filter(date >= as.Date("2018-10-01"), date < as.Date('2019-10-01')) %>%
  group_by(city, iso) %>% summarise(donut_pre = mean(donut_effect_cumulative, na.rm=TRUE))

df_post = df_post %>% inner_join(df_pre, by = c('city', 'iso'))

m4 <- lm(donut_post ~ log(cumulative_lockdown), data = df_post, weights=df_post$population2)
m5 <- lm(donut_post ~ log(gdp_capita), data = df_post, weights=df_post$population2)
m6 <- lm(donut_post ~ log(wfh_share), data = df_post, weights=df_post$population2)
m7 <- lm(donut_post ~ log(cumulative_lockdown) + log(gdp_capita) + log(wfh_share), data = df_post, weights=df_post$population2)

stargazer(list(m0, m1, m2, m3, m4, m5, m6, m7), 
          se = list(rse(m0), rse(m1), rse(m2), rse(m3), cle_iso(m4), cle_iso(m5), cle_iso(m6), cle_iso(m7)),
          omit = c("iso"),
          omit.stat=c("adj.rsq", "ser","f"), 
          type="html", out="./figures-tables/tab0.doc")

################################################
# Table S2: Zillow regression
################################################
## Read in rents file and run ms
df_rent <- read_csv('./data/zori_panel_zips_top12.csv') %>% filter(MetroShort %in% cities) %>%
  rename(rent_pct_change = post_pct_change) %>% filter(!is.na(rent_pct_change))

m0 <- lm(rent_pct_change ~ pre_pct_change + log(density2019) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)
m1 <- lm(rent_pct_change ~ pre_pct_change + log(dist_to_cbd) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)
m2 <- lm(rent_pct_change ~ pre_pct_change + log(wfh_emp) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)
m3 <- lm(rent_pct_change ~ pre_pct_change + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + factor(MetroShort), data = df_rent, weights = df_rent$`2019 Population`)

## Read in home values file and run ms
df6 <- read_csv('./data/zhvi_panel_zips_top12.csv')

m4 <- lm(post_pct_change ~ pre_pct_change + log(density2019) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m5 <- lm(post_pct_change ~ pre_pct_change + log(dist_to_cbd) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m6 <- lm(post_pct_change ~ pre_pct_change + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m7 <- lm(post_pct_change ~ pre_pct_change + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)

###Create table
#https://dmyee.files.wordpress.com/2016/03/table_workshop.pdf
stargazer(m0, m1, m2, m3, m4, m5, m6, m7, 
          se = list(cle(m0), cle(m1), cle(m2), cle(m3), cle(m4), cle(m5), cle(m6), cle(m7)),
          omit = c("MetroShort", 'pre_pct_change'),
          omit.stat=c("adj.rsq", "ser","f"), type="html", out="./figures-tables/tab1.doc")

####################################
# Table S3. USPS regression
####################################
## Read in usps file and run models
df6 <- read_csv('./data/usps_panel_zips_top12.csv')

m0 <- lm(post_pop ~ pre_pop + log(density2019) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m1 <- lm(post_pop ~ pre_pop + log(dist_to_cbd) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m2 <- lm(post_pop ~ pre_pop + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)
m3 <- lm(post_pop ~ pre_pop + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$`2019 Population`)

## Business regressions
m4 <- lm(post_bus ~ pre_bus + log(density2019) + factor(MetroShort), data = df6, weights = df6$estab_count)
m5 <- lm(post_bus ~ pre_bus + log(dist_to_cbd) + factor(MetroShort), data = df6, weights = df6$estab_count)
m6 <- lm(post_bus ~ pre_bus + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$estab_count)
m7 <- lm(post_bus ~ pre_bus + log(density2019) + log(dist_to_cbd) + log(wfh_emp) + factor(MetroShort), data = df6, weights = df6$estab_count)

## Create table
stargazer(m0, m1, m2, m3, m4, m5, m6, m7, 
          se = list(cle(m0), cle(m1), cle(m2), cle(m3), cle(m4), cle(m5), cle(m6), cle(m7)),
          omit = c("MetroShort", 'pre_pop', 'pre_bus'),
          omit.stat=c("adj.rsq", "ser","f"), 
          type="html", out="./figures-tables/tab2.doc")


