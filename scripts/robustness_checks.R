###########################################
# robustness_checks.R
# check whether mastercard data matches other major datasets
# 1. USPS ~ Mastercard
# 2. USPS ~ Census
# 3. USPS ~ Inrix
# 4. USPS ~ Zillow
# 5. Data Axle ~ USPS
# 6. Data Axle ~ Census
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
packages <- c("tidyverse", "zoo", "directlabels", "cowplot", "pracma", "binsreg",
              "lubridate", "readxl", "stringr", "sandwich", "lmtest", "geosphere")
lapply(packages, pkgTest);

cities <- c('San Francisco, CA', 'New York, NY', 'Chicago, IL', 'Boston, MA',
            'Los Angeles, CA', 'Washington, DC', 'Atlanta, GA', 'Miami, FL',
            'Philadelphia, PA', 'Dallas, TX', 'Houston, TX', 'Phoenix, AZ')

cbd_radius = 3218.69 #2 mile cbd radius in meters

#define colors
black <- "#2E2D29"; cardinal <- "#B1040E"; teal <- "#66b2b2"; green <- "#228B22"; marmalade <- "#d16002"
orange <- "#FFAE42"; magenta <- "#8B008B"; purple <- "#800080"; blue <- "#0000FF"; red <- "#FF0000"
options(repr.plot.width=10, repr.plot.height=8)

#end date for figures
start_date = '2018-01-01'
end_date = '2023-09-01'
end_date_long = '2025-06-01'

## start and end date for for cumulations
start_period = '2017-06-01' #start period of cumulation ending in 2020-03-01 exclusive
end_period = '2022-12-01' #end period for cumulation starting in 2020-03-01 inclusive

#set to your working directory
setwd('~/Documents/donut-effect-pnas') #path to directory

###############################################
# 1. Zillow ~ USPS
###############################################
df = read_csv('./data/zhvi_usps.csv')

#prepare binscatter
model = lm(post_pct_change ~ post_pop, weights=`2019 Population`, data = df)
summary_model = coeftest(model, vcov=vcovCL, cluster=~zip)
slope <- summary_model["post_pop", "Estimate"]
intercept <- summary_model["(Intercept)", "Estimate"]
t_value <- summary_model["post_pop", "t value"]
r_squared <- summary(model)$r.squared
equation_text <- sprintf("y = %.4fx + %.2f\nR² = %.2f, slope t-stat = %.2f", slope, intercept, r_squared, t_value)

binscatter = binsreg(y=df$post_pct_change, x=df$post_pop, nbins=30, 
                     polyreg=1, bycolors = teal, weights = df$`2019 Population`, vce='HC1', cluster=df$zip)
data.dots <- binscatter$data.plot$`Group Full Sample`$data.dots
data.line <- binscatter$data.plot$`Group Full Sample`$data.poly

ggplot() + labs(x="Net inflow as a percent of population", y="Percent change in home value index", size=12) +
  geom_point(data=data.dots, aes(x=x, y=fit), size=3, colour=teal) +
  geom_line(data=data.line, aes(x=x, y=fit), size = 1.5, colour=teal) +
  theme_bw() + annotate("text", x = -5, y = 8, label = equation_text, hjust = 0, vjust = 0, size = 5) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.border = element_blank())  +
  xlim(-20, 20) + ylim(5, 40)

ggsave('./figures-tables/robustness/usps_zillow.png', plot = last_plot(), width = 10, height = 8)


###########################################
## 2. Census ~ USPS net flows
###########################################
# 1. Read in Census data
#https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html#par_textimage_70769902
#https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/co-est2019-alldata.pdf
# data definition above show that domestic migration in 2019 covers the period 7/1/2018 to 6/30/2019 
census <- read_csv('https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv') %>%
  mutate(county = as.double(paste(STATE, COUNTY, sep = '')),
         pop_change18 = NPOPCHG_2017 + NPOPCHG_2018, 
         pop_change19 = NPOPCHG_2017 + NPOPCHG_2018 + NPOPCHG_2019,
         pop17 = POPESTIMATE2017,
         pop18 = POPESTIMATE2018,
         pop19 = POPESTIMATE2019,
         pop_est_change19=pop19-pop17,
         mig18 = DOMESTICMIG2018,
         mig19 = DOMESTICMIG2018 + DOMESTICMIG2019,
         pop_pchange18 = pop_change18/pop17,
         pop_pchange19 = pop_change19/pop17,
         pop_est_pchange19=pop_est_change19/pop17,
         mig_pchange18 = mig18/pop17,
         mig_pchange19 = mig19/pop17
  ) %>%
  select(county, pop_change18, pop_change19, pop17, pop18, mig18, mig19, pop_pchange18, pop_pchange19, 
         pop_est_pchange19, mig_pchange18, mig_pchange19)

#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv', col_types = cols('zip' = col_integer(), 'state' = col_integer(), 'county' = col_integer())) %>% 
  select(zip, state, county) %>% mutate(county = state*1000 + as.integer(county))

# 2. Read in zip code level USPS flow data
usps <- read_csv('./data/USPS_zips.csv') %>%
  filter(date >= as.Date('2017-07-01'), date < as.Date('2019-07-01')) %>%
  group_by(zip) %>% summarise(net_pop = sum(net_pop))

#Group data to county level and compare to population data
usps_county <- usps %>% inner_join(chars, by = 'zip') %>%
  group_by(county) %>% summarise(net_pop = sum(net_pop))

#3. Read in IRS county-county inflow and outflow data
# https://www.irs.gov/statistics/soi-tax-stats-migration-data-2018-2019
# https://www.irs.gov/pub/irs-soi/1819inpublicmigdoc.pdf
# 2018-19 file represents the mailing addresses from tax collection on 20180415 and 20190415
# tax season is usually from Jan 1 - April 15 so we really have an interval

outflow1819 <- read_csv('https://www.irs.gov/pub/irs-soi/countyoutflow1819.csv') %>%
  mutate(county1=as.integer(y1_statefips)*1000+as.integer(y1_countyfips),
         county2=as.integer(y2_statefips)*1000+as.integer(y2_countyfips),
         flow = n1+n2) %>%
  filter(grepl('Total Migration-US', y2_countyname))
#  filter(!grepl('Total|Non-migrants|Foreign|Northeast|Midwest|South|West', y2_countyname))
inflow1819 <- read_csv('https://www.irs.gov/pub/irs-soi/countyinflow1819.csv') %>%
  mutate(county1=as.integer(y1_statefips)*1000+as.integer(y1_countyfips),
         county2=as.integer(y2_statefips)*1000+as.integer(y2_countyfips),
         flow = n1+n2) %>%
  filter(grepl('Total Migration-US', y1_countyname))
outflow1718 <- read_csv('https://www.irs.gov/pub/irs-soi/countyoutflow1718.csv') %>%
  mutate(county1=as.integer(y1_statefips)*1000+as.integer(y1_countyfips),
         county2=as.integer(y2_statefips)*1000+as.integer(y2_countyfips),
         flow = n1+n2) %>%
  filter(grepl('Total Migration-US', y2_countyname))
inflow1718 <- read_csv('https://www.irs.gov/pub/irs-soi/countyinflow1718.csv') %>%
  mutate(county1=as.integer(y1_statefips)*1000+as.integer(y1_countyfips),
         county2=as.integer(y2_statefips)*1000+as.integer(y2_countyfips),
         flow = n1+n2) %>%
  filter(grepl('Total Migration-US', y1_countyname))

# 4. Merge and analyze
#Weight by baseline population, DHS percent change, throw out small counties. What we want:
#1. county, net-flow cumulated over (Jul 2017 to Jun 2019) for both USPS and Census
#2. then merge and take cross-county regression

usps_census = usps_county %>% inner_join(census, by = 'county') %>%
  mutate(net_pop_pchange = net_pop/pop17)

net_pop_high = quantile(usps_census$net_pop, .99)
net_pop_low = quantile(usps_census$net_pop, .01)
mig19_high = quantile(usps_census$mig19, .99)
mig19_low = quantile(usps_census$mig19, .01)

usps_census = usps_census %>% filter(
  net_pop < net_pop_high, net_pop > net_pop_low,
  mig19 < mig19_high, mig19 > mig19_low)

#prepare binscatter
model = lm(mig19 ~ net_pop, weights=pop17, data = usps_census)
summary_model = coeftest(model, vcov=vcovCL, cluster=~county)
slope <- summary_model["net_pop", "Estimate"]
intercept <- summary_model["(Intercept)", "Estimate"]
t_value <- summary_model["net_pop", "t value"]
r_squared <- summary(model)$r.squared
equation_text <- sprintf("y = %.4fx + %.2f\nR² = %.2f, slope t-stat = %.2f", slope, intercept, r_squared, t_value)

binscatter = binsreg(y=usps_census$mig19, x=usps_census$net_pop, nbins=30, 
                     polyreg=1, bycolors = teal, weights = usps_census$pop17, vce='HC1', cluster=usps_census$county)
data.dots <- binscatter$data.plot$`Group Full Sample`$data.dots
data.line <- binscatter$data.plot$`Group Full Sample`$data.poly

ggplot() + labs(x="Net change-of-address inflows (USPS 2017-19)", y="Net domestic in-migration\n(Census 2017-19 using IRS data)", size=12) +
  geom_point(data=data.dots, aes(x=x, y=fit), size=3, colour=teal) +
  geom_line(data=data.line, aes(x=x, y=fit), size = 1.5, colour=teal) +
  theme_bw() + annotate("text", x = -2.5e4, y = -5, label = equation_text, hjust = 0, vjust = 0, size = 5) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.border = element_blank())

ggsave('./figures-tables/robustness/usps_census.png', plot = last_plot(), width = 10, height = 8)


###############################################
# 3. Census ~ Data Axle
###############################################
# Calculate moves from 20170701-20190701
gsb_flows <- read_csv('./data/external_data/USPS_gsb/usps_gsb_zip_flows_2017_19.csv') %>%
  mutate(ZIP_pre = as.double(ZIP_pre), ZIP_post = as.double(ZIP_post))

chars <- read_csv('./data/zip_all_chars_cbd.csv', col_types = cols('zip'=col_integer(), 'county' = col_integer(), 'state'=col_integer()))%>% 
  mutate(county = state*1000+as.integer(county)) %>% select(zip, county)

gsb_flows <- gsb_flows %>% inner_join(chars, by=c('ZIP_pre'='zip')) %>% 
  inner_join(chars, by=c('ZIP_post'='zip'), suffix=c('_pre', '_post')) %>%
  group_by(county_pre, county_post) %>%
  summarise(count = sum(count, na.rm = TRUE),
            children = sum(children, na.rm= TRUE),
            spouses = sum(spouses, na.rm=TRUE)) %>%
  filter(county_pre != county_post) %>%
  mutate(total = count + children+spouses)

gsb_flows %>% write_csv('./data/gsb_county_flows_2017_19_clean.csv')
#gsb_flows = read_csv('./data/gsb_county_flows_2017_19_clean.csv')

gsb_inflow <- gsb_flows %>% group_by(county_post) %>% summarise(total = sum(total, na.rm = TRUE))
gsb_outflow <- gsb_flows %>% group_by(county_pre) %>% summarise(total = sum(total, na.rm = TRUE))

gsb_net_flow = gsb_inflow %>% inner_join(gsb_outflow, by = c('county_post'='county_pre'), suffix=c('_in', '_out')) %>%
  mutate(total = total_in-total_out) %>%
  rename(county = county_post) %>%
  select(county, total)

##########Census data (IRS)###########
census <- read_csv('https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv') %>%
  mutate(county = as.double(paste(STATE, COUNTY, sep = '')),
         pop17 = POPESTIMATE2017,
         mig19 = DOMESTICMIG2018 + DOMESTICMIG2019,
         mig_pchange19 = mig19/pop17
  ) %>% select(county, pop17, mig19, mig_pchange19)

census_gsb <- census %>% inner_join(gsb_net_flow, by = 'county')

total_high = quantile(census_gsb$total, .99)
total_low = quantile(census_gsb$total, .01)
mig19_high = quantile(census_gsb$mig19, .99)
mig19_low = quantile(census_gsb$mig19, .01)

census_gsb = census_gsb %>% filter(
  total < total_high, total > total_low,
  mig19 < mig19_high, mig19 > mig19_low)

#prepare binscatter
model = lm(mig19 ~ total, weights=pop17, data = census_gsb)
summary_model = coeftest(model, vcov=vcovCL, cluster=~county)
slope <- summary_model["total", "Estimate"]
intercept <- summary_model["(Intercept)", "Estimate"]
t_value <- summary_model["total", "t value"]
r_squared <- summary(model)$r.squared
equation_text <- sprintf("y = %.4fx + %.2f\nR² = %.2f, slope t-stat = %.2f", slope, intercept, r_squared, t_value)

binscatter = binsreg(y=census_gsb$mig19, x=census_gsb$total, nbins=30, 
                     polyreg=1, bycolors = teal, weights = census_gsb$pop17, vce='HC1', cluster=census_gsb$county)
data.dots <- binscatter$data.plot$`Group Full Sample`$data.dots
data.line <- binscatter$data.plot$`Group Full Sample`$data.poly

binscatter$bins_plot + labs(x="Net change-of-address inflows\nData Axle 2017-19", y="Net domestic in-migration\nCensus 2017-19 using IRS data", size=12) +
  geom_point(data=data.dots, aes(x=x, y=fit), size=3, colour=teal) +
  geom_line(data=data.line, aes(x=x, y=fit), size = 1.5, colour=teal) +
  theme_bw() + annotate("text", x = 1e3, y = 0, label = equation_text, hjust = 0, vjust = 0, size = 5) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.border = element_blank()) +
  xlim(-5e3, 5.5e3) + ylim(-5e3, 7e3)

ggsave('./figures-tables/robustness/census_data_axle.png', plot = last_plot(), width = 10, height = 8)

###########################################
# 4. USPS ~ Data Axle
###########################################
usps <- read_csv('./data/USPS_zips.csv') %>%
  filter(date >= as.Date('2017-07-01'), date < as.Date('2019-07-01')) %>%
  group_by(zip) %>% summarise(net_pop = sum(net_pop))

#Group data to county level and compare to population data
usps_county <- usps %>% inner_join(chars, by = 'zip') %>%
  group_by(county) %>% summarise(net_pop = sum(net_pop))

usps_census_gsb = usps_county %>% inner_join(census, by = 'county') %>%
  mutate(net_pop_pchange = net_pop/pop17) %>% 
  inner_join(gsb_net_flow, by = 'county') %>%
  mutate(total_pchange = total/pop17)

# First model: start with raw migration flows
total_high = quantile(usps_census_gsb$total, .99)
total_low = quantile(usps_census_gsb$total, .01)
net_pop_high = quantile(usps_census_gsb$net_pop, .99)
net_pop_low = quantile(usps_census_gsb$net_pop, .01)

usps_census_gsb = usps_census_gsb %>% filter(
  total < total_high, total > total_low,
  net_pop < net_pop_high, net_pop > net_pop_low)

#prepare binscatter
model = lm(net_pop ~ total, weights=pop17, data = usps_census_gsb)
summary_model = coeftest(model, vcov=vcovCL, cluster=~county)
slope <- summary_model["total", "Estimate"]
intercept <- summary_model["(Intercept)", "Estimate"]
t_value <- summary_model["total", "t value"]
r_squared <- summary(model)$r.squared
equation_text <- sprintf("y = %.4fx + %.2f\nR² = %.2f, slope t-stat = %.2f", slope, intercept, r_squared, t_value)

binscatter = binsreg(y=usps_census_gsb$net_pop, x=usps_census_gsb$total, nbins=30, 
                     polyreg=1, bycolors = teal, weights = usps_census_gsb$pop17, vce='HC1', cluster=usps_census_gsb$county)
data.dots <- binscatter$data.plot$`Group Full Sample`$data.dots
data.line <- binscatter$data.plot$`Group Full Sample`$data.poly

binscatter$bins_plot + labs(x="Net change-of-address inflows\n(Data Axle 2017-19)", y="Net change-of-address inflows\nUSPS 2017-19", size=12) +
  geom_point(data=data.dots, aes(x=x, y=fit), size=3, colour=teal) +
  geom_line(data=data.line, aes(x=x, y=fit), size = 1.5, colour=teal) +
  theme_bw() + annotate("text", x = 0, y = -7e3, label = equation_text, hjust = 0, vjust = 0, size = 5) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.border = element_blank()) +
  xlim(-4e3, 7e3) + ylim(-1e4, 3e3)

ggsave('./figures-tables/robustness/gsb_usps.png', plot = last_plot(), width = 10, height = 8)


###########################################
## Additional data checks, not reported in paper
###########################################

###########################################
## Scatter of net population flows vs GDP per capita | USA ONLY
###########################################
#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% dplyr::select(!estab_count) %>%
  filter(!is.na(dist_to_cbd)) %>% 
  mutate(county_fips = as.double(state)*1000+as.double(county))

#read in zip code level USPS flow data
usps <- read_csv('./data/USPS_zips.csv')

#get county (zip) level gdp capita
crosswalk = read_csv('./data/external_data/cbsa2fipsxw.csv') %>%
  rename('county_fips' = 'fipscountycode', 'state_fips' = 'fipsstatecode', 'county_name' = 'countycountyequivalent', 'state_name' = 'statename', 'cbsa_code' = 'cbsacode') %>%
  mutate(county_fips = state_fips*1000+county_fips) %>%
  select('county_fips', 'state_fips', 'cbsa_code')
nhgis = read_csv('./data/external_data/nhgis0009_ds244_20195_cbsa.csv') %>%
  rename('cbsa_code' = 'CBSAA', 'cbsa_name' = 'CBSA', 'income_per_capita' = 'ALX5E001', 'population' = 'ALUBE001') %>%
  select('cbsa_code', 'cbsa_name', 'income_per_capita', 'population')

chars2 = chars %>% left_join(crosswalk, by = 'county_fips') %>% left_join(nhgis, by='cbsa_code') %>%
  group_by(MetroShort, cbsa_code) %>% summarise(income_per_capita = mean(income_per_capita, na.rm=TRUE),
                                             population = sum(`2019 Population`, na.rm=TRUE))

## A. population flows
#construct dataset
temp <- chars %>% filter(!is.na(`2019 Population`)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MetroShort) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= cbd_radius, ntile(density2019[dist_to_cbd>=cbd_radius], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center', 'outside')) %>%
  inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  group_by(MetroShort, category, date) %>%
  summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
  filter(date >= as.Date('2018-01-01'), category %in% c('city center', 'outside'))

counts = temp %>% group_by(MetroShort, category) %>% summarise(n = n_distinct(date)) %>%
  group_by(MetroShort) %>% summarise(n = sum(n)) %>% filter(n>=132)

temp_filtered = temp %>% semi_join(counts, by = "MetroShort") %>%
  group_by(MetroShort, category) %>%
  mutate(net_pop_avg = zoo::rollmean(net_pop, 12, align='right', na.pad=TRUE),
         net_pop = cumsum(net_pop - net_pop_avg[date == as.Date('2020-02-15')]),
         net_pop = net_pop - net_pop[date==as.Date('2020-02-15')]) %>%
  pivot_wider(id_cols = c(MetroShort, date), names_from = category, values_from = net_pop) %>%
  mutate(dif = outside - `city center`) %>%
  filter(date == as.Date('2023-02-15'))

temp_filtered_inc = temp_filtered %>% inner_join(chars2, by = 'MetroShort') %>%
  filter(!is.na(dif), !is.na(income_per_capita), !is.na(population))

#get stats
model = lm(dif ~ income_per_capita, weights=population, data = temp_filtered_inc)
summary_model = coeftest(model, vcov=vcovCL, cluster=~MetroShort)
slope <- summary_model["income_per_capita", "Estimate"]
intercept <- summary_model["(Intercept)", "Estimate"]
t_value <- summary_model["income_per_capita", "t value"]
r_squared <- summary(model)$r.squared
equation_text <- sprintf("y = %.4fx + %.2f\nR² = %.2f, slope t-stat = %.2f", slope, intercept, r_squared, t_value)

binscatter = binsreg(y=temp_filtered_inc$dif, x=temp_filtered_inc$income_per_capita, nbins=30, 
        polyreg=1, bycolors = teal, weights = temp_filtered_inc$population, vce='HC1', cluster=temp_filtered_inc$MetroShort)
data.dots <- binscatter$data.plot$`Group Full Sample`$data.dots
data.line <- binscatter$data.plot$`Group Full Sample`$data.poly

ggplot() + labs(x="City's estimated income per capita", y="Cumulative pop flow\n Mar 2022-Mar 2023", size=12) +
  geom_point(data=data.dots, aes(x=x, y=fit), size=3, colour=teal) +
  geom_line(data=data.line, aes(x=x, y=fit), size = 1.5, colour=teal) +
  theme_bw() + annotate("text", x = 35000, y = 0, label = equation_text, hjust = 0, vjust = 0, size = 5) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.border = element_blank())
  
ggsave("./figures-tables/robustness/usps_income.png", width=10, height=8)

###########################################
## Scatter of net population flows vs Mastercard donut | USA ONLY
###########################################
df = read_csv('./data/external_data/mastercard/donut-effect-usa.csv') %>%  mutate(date = as.Date(date))
city_chars=read_csv('./data/us_city_chars.csv')
df2 = df %>% select(old_city_name, date, distance_column, donut_effect_ring, donut_effect_cumulative) %>% 
  left_join(city_chars, by=c('old_city_name')) %>% 
  filter(distance_column == '30 mile')

# Jan 2022 till March 2023 is 14 months
df_post = df2 %>% filter(date >= as.Date("2022-10-01"), date < as.Date('2023-10-01')) %>%
  group_by(MetroShort) %>% summarise(gdp_capita=mean(gdp_capita), donut_post=-1*sum(donut_effect_cumulative, na.rm=TRUE))

df_combined = df_post %>% left_join(temp_filtered_inc, by='MetroShort') %>%
  group_by(MetroShort) %>% summarise(income_per_capita = mean(income_per_capita, na.rm=TRUE),
                                  donut_post = mean(donut_post, na.rm=TRUE),
                                  dif = mean(dif, na.rm=TRUE),
                                  population = mean(population, na.rm=TRUE)) %>%
  filter(!is.na(dif), !is.na(donut_post), !is.na(population))

df_combined %>% write_csv('./data/output/robustness_USPS_flows_mcard-USA.csv')
#df_combined = read_csv('./data/output/robustness_USPS_flows_mcard-USA.csv')

#prepare binscatter
model = lm(dif ~ donut_post, weights=population, data = df_combined)
summary_model = coeftest(model, vcov=vcovCL, cluster=~MetroShort)
slope <- summary_model["donut_post", "Estimate"]
intercept <- summary_model["(Intercept)", "Estimate"]
t_value <- summary_model["donut_post", "t value"]
r_squared <- summary(model)$r.squared
equation_text <- sprintf("y = %.4fx + %.2f\nR² = %.2f, slope t-stat = %.2f", slope, intercept, r_squared, t_value)

binscatter = binsreg(y=df_combined$dif, x=df_combined$donut_post, nbins=30, 
                     polyreg=1, bycolors = teal, weights = df_combined$population, vce='HC1', cluster=df_combined$MetroShort)
data.dots <- binscatter$data.plot$`Group Full Sample`$data.dots
data.line <- binscatter$data.plot$`Group Full Sample`$data.poly

ggplot() + labs(x="Spending growth, exterior-city center\n2023 average", y="Cumulative pop flow, exterior-city center\n2023 average", size=12) +
  geom_point(data=data.dots, aes(x=x, y=fit), size=3, colour=teal) +
  geom_line(data=data.line, aes(x=x, y=fit), size = 1.5, colour=teal) +
  theme_bw() + annotate("text", x = -200, y = 7, label = equation_text, hjust = 0, vjust = 0, size = 5) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.border = element_blank())

ggsave("./figures-tables/robustness/usps_mastercard.png", width=10, height=8)

###############################################
# Inrix ~ USPS
###############################################
#Inrix vs USPS
inrix = read_excel('./data/external_data/inrix.xlsx') %>%
  mutate(time = TOD + bucket_start_minute/60) %>% filter(trip_day_of_week %in% c(1:5), time>=7, time<=9) %>%
  group_by(MetroShort, year) %>% summarise(trip_count = sum(trip_count, na.rm=TRUE)) %>%
  pivot_wider(names_from=year, values_from=trip_count) %>%
  mutate(trip_growth = (`2023`-`2019`)/`2019`*100)

chars <- read_csv('./data/zip_all_chars_cbd.csv', col_types = cols('zip' = col_integer())) %>%
  filter(!is.na(dist_to_cbd))
usps <- read_csv('./data/USPS_zips.csv')
temp <- chars %>% filter(!is.na(`2019 Population`)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MetroShort) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= cbd_radius, ntile(density2019[dist_to_cbd>=cbd_radius], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center', 'outside')) %>%
  inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  group_by(MetroShort, category, date) %>%
  summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
  filter(date >= as.Date('2018-01-01'), category %in% c('city center', 'outside'))

counts = temp %>% group_by(MetroShort, category) %>% summarise(n = n_distinct(date)) %>%
  group_by(MetroShort) %>% summarise(n = sum(n)) %>% filter(n>=132)

temp_filtered = temp %>% semi_join(counts, by = "MetroShort") %>%
  group_by(MetroShort, category) %>%
  mutate(net_pop_avg = zoo::rollmean(net_pop, 12, align='right', na.pad=TRUE),
         net_pop = net_pop - net_pop_avg[date == as.Date('2019-12-15')],
         net_pop = cumsum(net_pop)) %>% #comment this line out for monthly flows
  pivot_wider(id_cols = c(MetroShort, date), names_from = category, values_from = net_pop) %>%
  mutate(dif = outside - `city center`, date = as.Date(date)) %>%
  filter(date >= as.Date('2023-01-01')) %>% group_by(MetroShort) %>%
  summarise(dif = mean(dif, na.rm=TRUE)) 

msa_chars = read_csv('./data/msa_all_chars.csv') %>% select(MetroShort, `2019 Population`)

inrix_usps = inrix %>% inner_join(temp_filtered, by='MetroShort') %>% inner_join(msa_chars, by='MetroShort') %>%
  filter(!is.na(dif), !is.na(trip_growth), !is.na(`2019 Population`))

#prepare binscatter
model = lm(dif ~ trip_growth, weights=`2019 Population`, data = inrix_usps)
summary_model = coeftest(model, vcov=vcovCL, cluster=~MetroShort)
slope <- summary_model["trip_growth", "Estimate"]
intercept <- summary_model["(Intercept)", "Estimate"]
t_value <- summary_model["trip_growth", "t value"]
r_squared <- summary(model)$r.squared
equation_text <- sprintf("y = %.4fx + %.2f\nR² = %.2f, slope t-stat = %.2f", slope, intercept, r_squared, t_value)

binscatter = binsreg(y=inrix_usps$dif, x=inrix_usps$trip_growth, nbins=30, 
                     polyreg=1, bycolors = teal, weights = inrix_usps$`2019 Population`, vce='HC1', cluster=inrix_usps$MetroShort)
data.dots <- binscatter$data.plot$`Group Full Sample`$data.dots
data.line <- binscatter$data.plot$`Group Full Sample`$data.poly

ggplot() + labs(x="Change in commutes to city center\n2023H1-2019H1, Inrix", y="Cumulative pop flow, exterior-city center\n2023 average", size=12) +
  geom_point(data=data.dots, aes(x=x, y=fit), size=3, colour=teal) +
  geom_line(data=data.line, aes(x=x, y=fit), size = 1.5, colour=teal) +
  theme_bw() + annotate("text", x = 50, y = 2.5, label = equation_text, hjust = 0, vjust = 0, size = 5)  +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.border = element_blank()) +
  xlim(-50, 110) + ylim(-2.5, 11)

ggsave("./figures-tables/robustness/usps_inrix.png", width=10, height=8)

###########################################
# HISTORICAL COMPARISONS
### 1. Plot Zillow's ZHVI from 1996 for four regions and compare change from
### 2. What share of population in city center vs Metro for top 12, plot over time
###########################################

###########################################
## Long run Donut Effect in Zillow data
###########################################
#read zip code level home value index for single family homes from Zillow
df2 <- read_csv('https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv?t=1631634893', 
                col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  mutate(MetroShort = sub("-.*", "", MsaShort),
         MetroShort = paste(MetroShort, State, sep = ', ')) %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip, MetroShort), names_to = 'date', values_to = 'zhvi')

#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv')

#construct dataset
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = 'cbd', 
         quantile_rank = replace(quantile_rank, dist_to_cbd >= cbd_radius, ntile(density2019[dist_to_cbd>=cbd_radius], 10)),
         category = ifelse(quantile_rank == 'cbd', 'cbd',
                           case_when(quantile_rank == 10 ~ 'high',
                                     quantile_rank %in% c(6:9) ~ 'mid',
                                     TRUE ~ 'low'))) %>%
  inner_join(df2, by = 'zip') %>% 
  mutate(date = as.Date(date),
         zhvi_pop = zhvi*`2019 Population`) %>%
  group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE, rule = 2)) %>% 
  group_by(category, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(zhvi = zhvi_pop/population) %>%
  group_by(category) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2000-01-31')]*100) %>%
  mutate(
    val = zhvi, name = category,
    type = case_when(str_detect(name, 'high') ~ 'high density',
                     str_detect(name, 'mid') ~ 'suburb',
                     str_detect(name, 'cbd') ~ 'city center',
                     TRUE ~ 'exurb'), emph = "b"
  )

temp %>% ggplot(
  aes(x=date, y = val, color = type, linetype = as.factor(emph), 
      alpha = as.factor(emph), size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y", date_breaks="1 year") +
  geom_line() +
  xlim(as.Date('2000-01-01'), as.Date('2026-01-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = 'last.points') +   
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none') +
  scale_size_manual(values = c(1.8, 1), guide="none") +
  scale_alpha_manual(values = c(1, 0.8), guide="none") +
  scale_linetype(guide='none') +
  labs(x = "", 
       y = "Home Value Index (Feb 2020=100)",
       size = 10) +
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=95, size = 6, color = 'black') +
  theme_minimal() + 
  theme(
    text = element_text(size=20),
    plot.title = element_text(hjust = 0.5, family = "serif"),
    plot.caption = element_text(size = 16),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20, hjust = 1, vjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

#save plot
ggsave('./figures-tables/robustness/zhvi-2000.png', plot = last_plot(), width = 10, height = 8)


#######################################################
# Long run city center population shares based on NHGIS
#######################################################
cities <- c('San Francisco, CA', 'New York, NY', 'Chicago, IL', 'Boston, MA',
            'Los Angeles, CA', 'Washington, DC', 'Atlanta, GA', 'Miami, FL',
            'Philadelphia, PA', 'Dallas, TX', 'Houston, TX', 'Phoenix, AZ')
cbd_radius = 3218.69 #2 mile cbd radius in meters

df = read_csv('./data/external_data/nhgis/nhgis0010_ts_geog2010_zcta.csv') %>%
  rename('pop' = 'CL8AA', 'zip' = 'ZCTAA', 'year' = 'DATAYEAR') %>% select(zip, year, pop) %>%
  mutate(zip = as.integer(zip))

chars <- read_csv('./data/zip_all_chars_cbd.csv', col_types = cols('zip' = col_integer())) %>% 
  select(zip, `2010 Population`, `2019 Population`, MetroShort, dist_to_cbd)

df = df %>% inner_join(chars, by = 'zip') %>% 
  mutate(cbd = ifelse(dist_to_cbd <= cbd_radius, 1, 0)) %>%
  group_by(MetroShort, year, cbd) %>% summarise(pop = sum(pop, na.rm=TRUE))

df2 = df %>% group_by(MetroShort, year) %>% reframe(cbd_share = pop[cbd==1]/sum(pop), cbd_pop = pop[cbd==1], pop = sum(pop))

all_shares = df2 %>% group_by(year) %>% 
  summarise(avg_cbd_share = mean(cbd_share, na.rm=TRUE), 
            cbd_pop = sum(cbd_pop, na.rm=TRUE), 
            pop = sum(pop, na.rm=TRUE)) %>% mutate(cbd_share = cbd_pop/pop)

big_shares = df2 %>% filter(MetroShort %in% cities) %>% group_by(year) %>% 
  summarise(avg_cbd_share = mean(cbd_share, na.rm=TRUE), 
            cbd_pop = sum(cbd_pop, na.rm=TRUE), 
            pop = sum(pop, na.rm=TRUE)) %>% mutate(cbd_share = cbd_pop/pop)

###### From USPS ######
chars <- read_csv('./data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% dplyr::select(!estab_count)

#read in zip code level USPS flow data
usps <- read_csv('./data/USPS_zips.csv')

temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`)) %>%
  mutate(cbd = ifelse(dist_to_cbd <= cbd_radius, 1, 0)) %>%
  inner_join(usps, by = 'zip') %>% mutate(date = as.Date(date)) %>%
  group_by(zip) %>% mutate(net_pop = na.approx(net_pop, na.rm=FALSE)) %>%
  group_by(cbd, date) %>%
  summarise(net_pop = sum(net_pop, na.rm = TRUE)) %>% 
  group_by(cbd) %>%
  mutate(net_pop = cumsum(net_pop),
         net_pop = net_pop - net_pop[date == as.Date('2019-12-15')]) %>%
  group_by(date) %>% summarise(cbd_flow = net_pop[cbd==1], net_pop = sum(net_pop))

(2151443-323334)/(97763258-2464939) #1.9%, which is roughly the share in 2000, which reverses 0.3pp of growth till 2020 (2.2%)

###########################################
# Robustness: Check different City Center coordinates
###########################################

###########################################
## Figure 2a with City Hall coords
###########################################
#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% dplyr::select(!estab_count)

#read in zip code level USPS flow data
usps <- read_csv('./data/USPS_zips.csv')

## A. population flows
#construct dataset
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_city_hall >= cbd_radius, ntile(density2019[dist_to_city_hall>=cbd_radius], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'outer suburb'))) %>%
  inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date(start_date), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(net_pop = na.approx(net_pop, na.rm=FALSE)) %>%
  group_by(category, date) %>%
  summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100,
            `2019 Population` = sum(`2019 Population`, na.rm=TRUE)) %>% 
  group_by(category) %>%
  mutate(net_pop_avg = zoo::rollmean(net_pop, 24, align='right', na.pad=TRUE),
         net_pop = net_pop - net_pop_avg[date == as.Date('2019-12-15')],
         net_pop = cumsum(net_pop)) %>% #comment this line out for monthly flows
  mutate(emph="b", val = net_pop, name = category)

#make plot
temp %>% ggplot(
  aes(x=date, y = val, color = name, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date(start_date), as.Date(end_date_long)) + 
  #ylim(-1, .25) + #(-2, .3)
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = name), method = list(cex = 1.2, "last.points","bumpup")) +   
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Cumulative devs from pre-pandemic flow (% points)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-11-01'), y=-.5, size = 6, color = 'black') +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#save plot
ggsave('./figures-tables/robustness/fig2a-city-hall.png', plot = last_plot(), width = 10, height = 8)


###########################################
## Figure 2a with Google Maps coords
###########################################
#read in zip code cha,racteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv', col_types = cols('zip' = col_integer()))

city_chars=read_csv('./data/us_city_chars.csv') %>% select(MetroShort, cbd_lat, cbd_lon) %>% 
  rename(mcard_cbd_lat = cbd_lat, mcard_cbd_lon = cbd_lon)

chars = chars %>% left_join(city_chars, by = 'MetroShort') %>% rowwise() %>%
  mutate(dist_to_mcard_cbd = as.numeric(distm(c(lon, lat), c(mcard_cbd_lon, mcard_cbd_lat), fun=distHaversine)))

#read in zip code level USPS flow data
usps <- read_csv('./data/USPS_zips.csv')

## A. population flows
#construct dataset
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`dist_to_mcard_cbd`)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_mcard_cbd >= cbd_radius, ntile(density2019[dist_to_mcard_cbd>=cbd_radius], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'outer suburb'))) %>%
  inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date(start_date), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(net_pop = na.approx(net_pop, na.rm=FALSE)) %>%
  group_by(category, date) %>%
  summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100,
            `2019 Population` = sum(`2019 Population`, na.rm=TRUE)) %>% 
  group_by(category) %>%
  mutate(net_pop_avg = zoo::rollmean(net_pop, 24, align='right', na.pad=TRUE),
         net_pop = net_pop - net_pop_avg[date == as.Date('2019-12-15')],
         net_pop = cumsum(net_pop)) %>% #comment this line out for monthly flows
  mutate(emph="b", val = net_pop, name = category)

#make plot
temp %>% ggplot(
  aes(x=date, y = val, color = name, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date(start_date), as.Date(end_date_long)) + 
  #ylim(-1, .25) + #(-2, .3)
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = name), method = list(cex = 1.2, "last.points","bumpup")) +   
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Cumulative devs from pre-pandemic flow (% points)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-11-01'), y=-.5, size = 6, color = 'black') +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#save plot
ggsave('./figures-tables/robustness/fig2a-google.png', plot = last_plot(), width = 10, height = 8)

###########################################
## Redo figure 4b. the Donut effect in purchase market, city hall coords
###########################################
#read zip code level home value index for single family homes from Zillow
df2 <- read_csv('https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1691449278', 
                col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  mutate(MetroShort = sub("-.*", "", MsaShort),
         MetroShort = paste(MetroShort, State, sep = ', ')) %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip, MetroShort), names_to = 'date', values_to = 'zhvi') %>%
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
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`), !is.na(dist_to_city_hall)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_city_hall >= cbd_radius, ntile(density2019[dist_to_city_hall>=cbd_radius], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'outer suburb'))) %>%
  inner_join(df2, by = 'zip') %>% 
  mutate(date = as.Date(date),
         zhvi_pop = zhvi*`2019 Population`) %>%
  group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE)) %>% 
  group_by(category, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(zhvi = zhvi_pop/population) %>%
  group_by(category) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2020-02-29')]*100) %>%
  mutate(
    val = zhvi, type = category, emph = "b"
  )

temp %>% ggplot(
  aes(x=date, y = val, color = type, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date(start_date), as.Date(end_date_long)) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(cex = 1.2,"last.points","bumpup")) +  
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Home Value Index (Feb 2020=100)",
       size = 10
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=95, size = 6, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )

#save plot
ggsave('./figures-tables/robustness/fig4b-city-hall.png', plot = last_plot(), width = 10, height = 8)


###########################################
## Redo Figure 4b. Donut effect in purchase market, google maps coords (from Mastercard file)
###########################################
#read zip code level home value index for single family homes from Zillow
df2 <- read_csv('https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1691449278', 
                col_types = cols(RegionName = col_double())) %>% 
  rename(zip = 'RegionName', MsaShort = 'Metro') %>%
  mutate(MetroShort = sub("-.*", "", MsaShort),
         MetroShort = paste(MetroShort, State, sep = ', ')) %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, CountyName, MsaShort)) %>%
  pivot_longer(!c(zip, MetroShort), names_to = 'date', values_to = 'zhvi') %>%
  filter(date >= as.Date(start_date), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(zhvi = na.approx(zhvi, na.rm=FALSE),
                           triple = ifelse(zhvi/lag(zhvi, 12) > 3, 1, 0),
                           count_na = sum(is.na(zhvi)),
                           num_vals=n()) %>%
  group_by(zip) %>% filter(all(triple==0 | is.na(triple)), 
                           count_na/num_vals<.05) ### filter out rows with 3x growth in rents (happens when there are several consecutive missing observations that zillow imputes)

#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv', col_types = cols('zip' = col_integer()))

city_chars=read_csv('./data/us_city_chars.csv') %>% select(MetroShort, cbd_lat, cbd_lon) %>% 
  rename(mcard_cbd_lat = cbd_lat, mcard_cbd_lon = cbd_lon)

chars = chars %>% left_join(city_chars, by = 'MetroShort') %>% rowwise() %>%
  mutate(dist_to_mcard_cbd = as.numeric(distm(c(lon, lat), c(mcard_cbd_lon, mcard_cbd_lat), fun=distHaversine)))

#construct dataset
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`), !is.na(dist_to_mcard_cbd)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_city_hall >= cbd_radius, ntile(density2019[dist_to_city_hall>=cbd_radius], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'outer suburb'))) %>%
  inner_join(df2, by = 'zip') %>% 
  mutate(date = as.Date(date),
         zhvi_pop = zhvi*`2019 Population`) %>%
  group_by(zip) %>% mutate(zhvi_pop = na.approx(zhvi_pop, na.rm=FALSE)) %>% 
  group_by(category, date) %>%
  summarise(zhvi_pop = sum(zhvi_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(zhvi = zhvi_pop/population) %>%
  group_by(category) %>% mutate(zhvi = zhvi/zhvi[date == as.Date('2020-02-29')]*100) %>%
  mutate(
    val = zhvi, type = category, emph = "b"
  )

temp %>% ggplot(
  aes(x=date, y = val, color = type, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date(start_date), as.Date(end_date_long)) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(cex = 1.2,"last.points","bumpup")) +  
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Home Value Index (Feb 2020=100)",
       size = 10
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=95, size = 6, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )

#save plot
ggsave('./figures-tables/robustness/fig4b-google.png', plot = last_plot(), width = 10, height = 8)


#######################################################
# Test WFH potential vs observed WFH
#######################################################

metro_chars = read_csv('./data/msa_all_chars.csv')

wfh = read_excel('./data/external_data/remote_work_in_job_ads_signup_data.xlsx', sheet = 'us_city_by_month') %>%
  mutate(MetroShort = paste(city, state, sep = ", ")) %>%
  filter(year_month >= 2023, measurement == '1 Month Average') %>%
  group_by(MetroShort) %>% summarise(wfh_share = mean(percent, na.rm=TRUE)/100)

metro_chars = metro_chars %>% inner_join(wfh, by = 'MetroShort')

#prepare binscatter
model = lm(wfh_share ~ wfh_emp, weights=`2019 Population`, data = metro_chars)
summary_model = coeftest(model, vcov=vcovCL, cluster=~MetroShort)
slope <- summary_model["wfh_emp", "Estimate"]
intercept <- summary_model["(Intercept)", "Estimate"]
t_value <- summary_model["wfh_emp", "t value"]
r_squared <- summary(model)$r.squared
equation_text <- sprintf("y = %.4fx + %.2f\nR² = %.2f, slope t-stat = %.2f", slope, intercept, r_squared, t_value)

# create a scatterplot with a trendline
ggplot(metro_chars, aes(wfh_share, wfh_emp)) + 
  geom_point(size=1) + geom_smooth(method="lm", mapping = aes(weight=`2019 Population`), show.legend=TRUE) + 
  annotate("text", x = .2, y = .3, label = equation_text, hjust = 0, vjust = 0, size = 5) +
  labs(x  = "WFH exposure\nDingel and Neiman (2020)", 
       y = "WFH share, GSWA",
       size = 10
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  ) 

ggsave('./figures-tables/robustness/wfh_dingel_gwsa.png', plot = last_plot(), width = 10, height = 8)



######################################################################################
# Calculate Donut broken by WFH bucket
######################################################################################

###########################################
# US mastercard
###########################################
wfh = read_excel('./data/external_data/remote_work_in_job_ads_signup_data.xlsx', sheet = 'us_city_by_month') %>%
  mutate(MetroShort = paste(city, state, sep = ", ")) %>%
  filter(year_month >= 2023, measurement == '1 Month Average') %>%
  group_by(MetroShort) %>% summarise(wfh_share = mean(percent, na.rm=TRUE))

### Read in mastercard data and merge
df = read_csv('./data/donut-effect-usa.csv') %>% mutate(date = as.Date(date))
city_chars=read_csv('./data/us_city_chars.csv')
df2 = df %>% select(old_city_name, iso, date, distance_column, donut_effect_cumulative) %>% 
  inner_join(city_chars, by = c('old_city_name')) %>% filter(distance_column == '40 mile') %>%
  select(iso, date, distance_column, donut_effect_cumulative, MetroShort, wfh_emp, `2019 Population`) %>%
  rename(population = `2019 Population`, wfh_share = wfh_emp)

wfh_buckets = df2 %>% select(MetroShort, wfh_share, population) %>% unique() %>% ungroup() %>% filter(!is.na(wfh_share), !is.na(population)) %>%
  mutate(wfh_ntile = ntile(wfh_share, 3), wfh_bucket = case_when(wfh_ntile %in% c(1, 2) ~ 'low', TRUE ~ 'high'),
         pop_rank = dense_rank(desc(population)), pop_bucket = case_when(pop_rank %in% c(1:12) ~ 'Top 12', pop_rank %in% c(13:50) ~ '13-50', TRUE ~ '51+'))

df2 = df2 %>% left_join(wfh_buckets, by = c('MetroShort'))

# Calculate the average donut_effect_cumulative for each date and wfh_bucket
average_data <- df2 %>% group_by(date, wfh_bucket) %>%
  summarise(avg_donut_effect = weighted.mean(donut_effect_cumulative, population.x, na.rm = TRUE))

# Filter data for regression analysis from Jan 1, 2023 onwards
regression_data <- average_data %>% filter(date >= as.Date("2023-01-01"))

# Define colors for each bucket, ensure the buckets are in the right order
buckets <- unique(regression_data$wfh_bucket)
colors <- c(cardinal, teal, green)

average_data %>% write_csv('./data/output/robustness_mastercard_wfh_buckets_USA.csv')
#average_data = read_csv('./data/output/robustness_mastercard_wfh_buckets_USA.csv')

average_data %>% mutate(emph='b') %>% ggplot(
  aes(x=date, y = avg_donut_effect, color=as.factor(wfh_bucket), linetype = as.factor(emph), 
      alpha = as.factor(emph), size = as.factor(emph)), guide='none') +
  geom_line() +
  scale_x_date(date_labels="%Y", date_breaks  ="1 year") +
  xlim(as.Date(start_date), as.Date('2024-06-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = wfh_bucket), method = list(cex = 1.2,"last.points","bumpup")) +
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Relative growth, city center - outside, % points",
       size = 10
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=95, size = 6, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )

ggsave('figures-tables/robustness/persistence_mastercard_usa.png', width = 9, height = 10)

###########################################
# USPS method 2 - metro-level avg
###########################################
wfh = read_excel('./data/external_data/remote_work_in_job_ads_signup_data.xlsx', sheet = 'us_city_by_month') %>%
  mutate(MetroShort = paste(city, state, sep = ", ")) %>%
  filter(year_month >= 2023, measurement == '1 Month Average') %>%
  group_by(MetroShort) %>% summarise(wfh_share = mean(percent, na.rm=TRUE))
metro_pop = read_csv('./data/msa_all_chars.csv') %>% select(MetroShort, `2019 Population`) %>% rename(population = `2019 Population`)
wfh = wfh %>% inner_join(metro_pop, by = 'MetroShort')

#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% dplyr::select(!estab_count) %>%
  filter(!is.na(dist_to_cbd)) %>% mutate(county_fips = as.double(state)*1000+as.double(county))

usps <- read_csv('./data/USPS_zips.csv') #read in zip code level USPS flow data

## A. population flows
#construct dataset
temp <- chars %>% filter(!is.na(`2019 Population`)) %>%
  mutate(quantile_rank = 'cbd') %>% group_by(MetroShort) %>%
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

df2 = temp_filtered %>% left_join(wfh, by = 'MetroShort') %>% 
  filter(!is.na(dif),  !is.na(population))

wfh_buckets = df2 %>% select(MetroShort, wfh_share, population) %>% unique() %>% ungroup() %>% filter(!is.na(wfh_share)) %>%
  mutate(wfh_rank = dense_rank(desc(wfh_share)), wfh_bucket = case_when(wfh_rank %in% c(1:12) ~ 'Top 12', wfh_rank %in% c(13:50) ~ '13-50', TRUE ~ '51+'),
         wfh_ntile = ntile(wfh_share, 3), wfh_bucket = case_when(wfh_ntile %in% c(1, 2) ~ 'low', TRUE ~ 'high'),
         pop_rank = dense_rank(desc(population)), pop_bucket = case_when(pop_rank %in% c(1:12) ~ 'Top 12', pop_rank %in% c(13:50) ~ '13-50', TRUE ~ '51+')) %>%
  arrange(pop_rank)

df2 = df2 %>% left_join(wfh_buckets, by = c('MetroShort'))

# Calculate the average donut_effect_cumulative for each date and wfh_bucket
average_data <- df2 %>% group_by(date, wfh_bucket) %>%
  summarise(avg_donut_effect = weighted.mean(dif, population.x, na.rm = TRUE))

colors <- c(cardinal, teal, green) 

average_data %>% mutate(emph='b') %>% ggplot(
  aes(x=date, y = avg_donut_effect, color=as.factor(wfh_bucket), linetype = as.factor(emph), 
      alpha = as.factor(emph), size = as.factor(emph)), guide='none') +
  geom_line() +
  scale_x_date(date_labels="%Y", date_breaks  ="1 year") +
  xlim(as.Date(start_date), as.Date('2024-06-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = wfh_bucket), method = list(cex = 1.2,"last.points","bumpup")) +
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Relative growth, city center - outside, % points",
       size = 10
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=95, size = 6, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )

ggsave('./figures-tables/robustness/fig5b_wfh_bucket_usps.png', width = 10, height = 8)


###########################################
# Zillow
###########################################
wfh = read_excel('./data/external_data/remote_work_in_job_ads_signup_data.xlsx', sheet = 'us_city_by_month') %>%
  mutate(MetroShort = paste(city, state, sep = ", ")) %>%
  filter(year_month >= 2023, measurement == '1 Month Average') %>%
  group_by(MetroShort) %>% summarise(wfh_share = mean(percent, na.rm=TRUE))

wfh = read_csv('./data/msa_all_chars.csv') %>% select(MetroShort, wfh_emp, `2019 Population`) %>%
  rename(wfh_share = wfh_emp, population = `2019 Population`)

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
  mutate(dif = `center` - outside)

wfh_buckets = temp %>% left_join(wfh, by = 'MetroShort') %>% select(MetroShort, wfh_share, population) %>%
  unique() %>% ungroup() %>% filter(!is.na(wfh_share)) %>% 
  mutate(wfh_ntile = ntile(wfh_share, 3), wfh_bucket = case_when(wfh_ntile %in% c(1, 2) ~ 'low', TRUE ~ 'high'),
         pop_rank = dense_rank(desc(population)), pop_bucket = case_when(pop_rank %in% c(1:12) ~ 'Top 12', pop_rank %in% c(13:50) ~ '13-50', TRUE ~ '51+'))

df2 = temp %>% left_join(wfh_buckets, by = 'MetroShort') %>% 
  filter(!is.na(wfh_bucket))

# Calculate the average donut_effect_cumulative for each date and wfh_bucket
average_data <- df2 %>% group_by(date, wfh_bucket) %>%
  summarise(avg_donut_effect = weighted.mean(dif, population, na.rm = TRUE))

# Filter data for regression analysis from Jan 1, 2023 onwards
regression_data <- average_data %>% filter(date >= as.Date("2023-01-01"))

# Define colors for each bucket, ensure the buckets are in the right order
buckets <- unique(regression_data$wfh_bucket)
colors <- c(cardinal, teal, green) 

average_data %>% mutate(emph='b') %>% ggplot(
  aes(x=date, y = avg_donut_effect, color=as.factor(wfh_bucket), linetype = as.factor(emph), 
      alpha = as.factor(emph), size = as.factor(emph)), guide='none') +
  geom_line() +
  scale_x_date(date_labels="%Y", date_breaks  ="1 year") +
  xlim(as.Date(start_date), as.Date('2024-06-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = wfh_bucket), method = list(cex = 1.2,"last.points","bumpup")) +
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Relative growth, city center - outside, % points",
       size = 10
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=95, size = 6, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )

ggsave('./figures-tables/robustness/wfh_bucket_zillow.png', width = 9, height = 10)

