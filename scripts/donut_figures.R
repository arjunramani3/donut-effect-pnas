###########################################
# donut_figures.R
# This script creates all main and appendix figures
# except for supplementary figure S5 (see ./scripts/net_outflow_distribution.R)
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
packages <- c("tidyverse", "zoo", "directlabels", "cowplot", "pracma", "sandwich",
              "lubridate", "readxl", "stringr", "forecast", 'binsreg', "lmtest")

lapply(packages, pkgTest);

cities <- c('San Francisco, CA', 'New York, NY', 'Chicago, IL', 'Boston, MA',
            'Los Angeles, CA', 'Washington, DC', 'Atlanta, GA', 'Miami, FL',
            'Philadelphia, PA', 'Dallas, TX', 'Houston, TX', 'Phoenix, AZ')

cbd_radius = 3218.69 #2 mile cbd radius in meters
meters_per_mile = 1609.34 #number of meters in a mile

#define colors
black <- "#2E2D29"; cardinal <- "#B1040E"; teal <- "#66b2b2"; green <- "#228B22"; marmalade <- "#d16002"
orange <- "#FFAE42"; magenta <- "#8B008B"; purple <- "#800080"; blue <- "#0000FF"; red <- "#FF0000"
options(repr.plot.width=10, repr.plot.height=8)

#date rage for figures
start_date='2018-01-01'
end_date = '2023-12-01'
end_date_long = '2024-08-01'

## start and end date for for cumulations
start_period = '2017-06-01' #start period of cumulation ending in 2020-03-01 exclusive
end_period = '2022-12-01' #end period for cumulation starting in 2020-03-01 inclusive

#set to your working directory
setwd('~/Documents/donut-effect-pnas/') #enter path to working directory

###########################################
## Figure 1a. Plot average donut overall
###########################################
df = read_csv('./data/external_data/mastercard/donut-effect-main.csv') %>% mutate(date = as.Date(date))
city_chars=read_csv('./data/global_city_chars.csv')
df2 = df %>% select(old_city_name, iso, date, distance_val, distance_column, donut_effect_ring, donut_effect_cumulative) %>% 
  left_join(city_chars, by=c('old_city_name', 'iso')) %>% filter(!is.na(iso), !is.na(population2), distance_val %in% c(5, 10, 30, 50)) %>%
  group_by(date, distance_column) %>% summarise(donut_effect_cumulative = weighted.mean(donut_effect_cumulative, population2, na.rm=TRUE))

###Eliminate seasonal component (decomposs ts into seasonal, trend, and irregular components using loess smoothing)
deseasonalise <- function(data) { # Function to deseasonalize time series using stl
  ts_data <- ts(data$donut_effect_cumulative, frequency = 12)  # assuming monthly data, adjust frequency accordingly
  decomposed <- stl(ts_data, s.window = "periodic")
  seasonally_adjusted <- seasadj(decomposed)
  return(data.frame(data$date, donut_deseasonalised = seasonally_adjusted))
}

# Applying the function to each group (distance_column) and binding the results back together
df3 <- df2 %>% group_by(distance_column) %>%
  group_modify(~ deseasonalise(.), .keep = TRUE) %>% ungroup()

#df3 = read_csv('./data/output/fig1a.csv')
df3 %>% mutate(emph='b') %>% ggplot(
  aes(x=data.date, y = donut_deseasonalised, color=distance_column, linetype = as.factor(emph), 
      alpha = as.factor(emph), size = as.factor(emph)), guide='none') +
  geom_line() +
  scale_x_date(date_labels="%Y", date_breaks  ="1 year") +
  xlim(as.Date(start_date), as.Date(end_date_long)) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = distance_column), method = list(cex = 1.8,"last.points","bumpup")) +
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "City center - outer ring\n Difference in normalized spending",
       size = 15
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-03-01'), y=-20, size = 8, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=24),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 24),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)
  )
#save file
df3 %>% write_csv('./data/output/fig1a.csv')
ggsave('./figures-tables/fig1a_donut_average.png', plot = last_plot(), width = 10, height = 8)

###########################################
## Figure 2a. Donut effect for USPS population flows
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
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= cbd_radius, ntile(density2019[dist_to_cbd>=cbd_radius], 10)),
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
  geom_dl(aes(label = name), method = list(cex = 1.8, "last.points","bumpup")) +   
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Cumulative devs from pre-pandemic flow\n% points",
       size = 15
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-03-01'), y=-2, size = 8, color = 'black') +
  theme_minimal()+
  theme(text = element_text(size=24),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 24),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)
  )
#save plot
ggsave('./figures-tables/fig2a.png', plot = last_plot(), width = 10, height = 8)

###########################################
## Figure 2b. New York USPS heat maps
###########################################
#https://arilamstein.com/documentation/choroplethrZip/reference/zip_choropleth.html
#install_github('arilamstein/choroplethrZip@v1.5.0') #Run this line to use the choroplethrZip library
#Install package 'devtools' in order to run install_github()
library(choroplethrZip)

#create dataset in format required for choroplehtrZip
df <- read_csv('./data/usps_panel_zips_maps.csv', col_types = cols(zip = col_character())) %>%
  mutate(value = round(post_pop, digits = 2),
         zip = ifelse(nchar(zip)==4, paste('0', zip, sep=''), zip)) %>%
  dplyr::select(zip, value) %>%
  rename(region = zip) %>%
  filter(!is.na(zip), !is.na(value))

## Create New York map
df_choro <- df
#df_choro$value <- cut(df$value, breaks = c(-125, -6, -3, -1.5, .05, 2, 4, 70))
choro = ZipChoropleth$new(df_choro)
choro$ggplot_scale = scale_fill_brewer(name="Net inflows/pop (%)", palette = 'RdYlGn')
choro$set_zoom_zip(state_zoom=NULL, county_zoom=NULL, msa_zoom='New York-Newark-Jersey City, NY-NJ-PA', zip_zoom=NULL)
choro$render()
ggsave('./figures-tables/fig2b.png', plot = last_plot(), width = 10, height = 8)

####################################
# Figure 3. heterogeneity across US cities
####################################
chars <- read_csv('./data/zip_all_chars_cbd.csv', #read in zip code characteristics
                  col_types = cols('zip' = col_integer())) %>% dplyr::select(!estab_count)
usps <- read_csv('./data/USPS_zips.csv') #read in zip code month level USPS flow data

msa_chars <- msa_chars <- read_csv('./data/msa_all_chars.csv') %>% filter(!is.na(MsaName)) %>%
  mutate(msa_pop_rank = dense_rank(desc(`2019 Population`)),
         msa_pop_group = case_when(msa_pop_rank %in% c(1:12) ~ 'top12',
                                   msa_pop_rank %in% c(13:50) ~ 'mid',
                                   TRUE ~ 'low'))
## grip_plot is a function that filter metros on the msa_group variable to the msa_level,
## and then plots zip code level series that are aggregated to the zip_group level
#@param: msa_group = which msa-level variable we want to break plot down by
#@param: msa_level = which level of the msa_group variable we want to create the plot for
#@param: zip_group = the zip code level grouping variable to break down time series by

grid_plot <- function (msa_group, msa_level, zip_group) {
  options(repr.plot.width=12, repr.plot.height=5)
  temp <- msa_chars %>% filter(get(msa_group) == msa_level) %>%
    dplyr::select(MetroShort, msa_pop_rank) %>%
    inner_join(chars, by = 'MetroShort') %>% 
    filter(!is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
    mutate(quantile_rank = 'cbd') %>%
    group_by(MsaName) %>%
    mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= cbd_radius, ntile(get(zip_group)[dist_to_cbd>=cbd_radius], 10)),
           category = ifelse(quantile_rank == 'cbd', 'city center',
                             case_when(quantile_rank == 10 ~ 'high density',
                                       quantile_rank %in% c(6:9) ~ 'suburb',
                                       TRUE ~ 'outer suburb'))) %>%
    inner_join(usps, by = 'zip') %>% 
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date('2018-01-01'), date < as.Date(end_date)) %>%
    group_by(zip) %>% mutate(net_pop = na.approx(net_pop, na.rm=FALSE)) %>%
    group_by(category, date) %>%
    summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
    group_by(category) %>%
    mutate(net_pop_avg = zoo::rollmean(net_pop, 24, align='right', na.pad=TRUE),
           net_pop = net_pop - net_pop_avg[date == as.Date('2019-12-15')],
           net_pop = cumsum(net_pop)) %>% #comment out for monthly flows
    mutate(val = net_pop, type = category, emph = "b")
  return(temp %>% ggplot(
    aes(x=date, y = val, color = type, linetype = as.factor(emph), 
        alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
      scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
      geom_line()+
      xlim(as.Date('2018-01-01'), as.Date('2024-01-01')) + 
      ylim(-10, 2) + 
      geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
      geom_dl(aes(label = type), method = list(cex = 1.2,"last.points","bumpup")) +   
      scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
      scale_size_manual(values = c(1.8, 1), guide="none")+
      scale_alpha_manual(values = c(1, 0.8), guide="none")+
      scale_linetype(guide='none') +
      labs(title = '', 
           x  = "", 
           y = ifelse(msa_level=='top12', "Cumulative devs from pre-pandemic flow\n% points", '')
      )+
      geom_text(label="Feb 2020", x=as.Date('2019-03-01'), y=-2.5, size = 5, color = 'black') +
      coord_cartesian(clip = "off") + 
      theme_minimal()+
      theme(text = element_text(size=20),
            plot.title= element_text(hjust = 0.5, family = "serif"),
            plot.caption = element_text(size = 10),
            plot.margin = margin(t = 0, r = 10, b = 0, l = 5, unit = "pt"),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20)
      )
  )
}

a <- grid_plot('msa_pop_group', 'top12', 'density2019') #top 12 metros
b <- grid_plot('msa_pop_group', 'mid', 'density2019') #metros 13-50
c <- grid_plot('msa_pop_group', 'low', 'density2019') #metros 51-365

plot_grid(a, NULL, b, NULL, c, NULL, rel_widths = c(1, .1, 1, .1, 1, .13), 
          rel_heights = c(1, .1, 1, .1, 1, .1), nrow = 1, ncol = 6)
ggsave('./figures-tables/fig3_heterogeneity.png', plot = last_plot(), width = 15, height = 6)

###########################################
## Figure 4a. donut effect in rental market
###########################################
#read in zip code level rental index from Zillow
df <- read_csv('https://files.zillowstatic.com/research/public_csvs/zori/Zip_zori_uc_sfrcondomfr_sm_month.csv?t=1704026596', 
               col_types = cols(RegionName = col_double())) %>% rename(zip = 'RegionName') %>%
  dplyr::select(!c(RegionID, SizeRank, RegionType, StateName, State, City, Metro, CountyName)) %>% 
  pivot_longer(!zip, names_to = 'date', values_to = 'zori') %>%
  mutate(date = as.Date(as.yearmon(date)) + 14) %>%
  filter(date >= as.Date(start_date), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(zori = na.approx(zori, na.rm=FALSE),
                           triple = ifelse(zori/lag(zori, 12) > 3, 1, 0),
                           count_na = sum(is.na(zori)), num_vals=n()) %>%
  group_by(zip) %>% filter(all(triple==0 | is.na(triple)), # filter out rows with 3x growth in rents (happens when there are several consecutive missing observations that zillow imputes)
                           count_na/num_vals<.05) # filter out rows with more than 5% missing values

#read in all zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv')

#construct dataset to plot
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= cbd_radius, ntile(density2019[dist_to_cbd>=cbd_radius], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'outer suburb'))) %>%
  inner_join(df, by = 'zip') %>% 
  mutate(date = as.Date(date), zori_pop = zori*`2019 Population`) %>%
  group_by(zip) %>% mutate(zori_pop = na.approx(zori_pop, na.rm=FALSE, rule = 2)) %>% 
  group_by(category, date) %>%
  summarise(zori_pop = sum(zori_pop, na.rm = TRUE),
            population = sum(`2019 Population`, na.rm = TRUE)) %>% 
  mutate(zori = zori_pop/population) %>%
  group_by(category) %>% mutate(zori = zori/zori[date == as.Date('2020-02-15')]*100) %>%
  mutate(val = zori, type = category, emph = "b")

#make plot
temp %>% ggplot(
  aes(x=date, y = val, color = type, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date(start_date), as.Date('2025-01-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(cex = 1.8,"last.points","bumpup")) +
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Rental Index (Feb 2020=100)",
       size = 15
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-03-01'), y=110, size = 8, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=24),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 24),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)
  )

#save file
ggsave('./figures-tables/fig4a.png', plot = last_plot(), width = 10, height = 8)

###########################################
## Figure 4b. donut effect in purchase market
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
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= cbd_radius, ntile(density2019[dist_to_cbd>=cbd_radius], 10)),
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
  xlim(as.Date(start_date), as.Date('2025-01-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = type), method = list(cex = 1.8,"last.points","bumpup")) +  
  scale_colour_manual(values = c(teal, cardinal, black, green), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Home Value Index (Feb 2020=100)",
       size = 15
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-03-01'), y=110, size = 8, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=24),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 24),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)
  )

#save plot
ggsave('./figures-tables/fig4b.png', plot = last_plot(), width = 10, height = 8)

###########################################
## Figure 5a: Donut size by WFH Bucket
###########################################
# A. Global mastercard
wfh = read_excel('./data/external_data/GSWA-Figures-Data-2023.xlsx', 
                 sheet='Figure 1', range="A1:C36") %>% rename(country = Country, wfh_days = `Number of days working from home this week`)

### Read in mastercard data and merge
df = read_csv('./data/external_data/mastercard/donut-effect-main.csv') %>% mutate(date = as.Date(date))
city_chars=read_csv('./data/global_city_chars.csv') %>% group_by(iso) %>% slice_max(order_by = population2, n = 1)
df2 = df %>% select(old_city_name, iso, date, distance_column, donut_effect_cumulative) %>% 
  inner_join(city_chars, by = c('old_city_name', 'iso')) %>% filter(distance_column == '30 mile') %>%
  inner_join(wfh, by='iso') %>% select(iso, date, distance_column, donut_effect_cumulative, city, wfh_days, population2)

wfh_buckets = df2 %>% select(iso, city, wfh_days) %>% unique() %>% ungroup() %>% filter(!is.na(wfh_days)) %>%
  mutate(wfh_ntile = ntile(wfh_days, 3), wfh_bucket = case_when(wfh_ntile %in% c(1,2) ~ 'low WFH', TRUE ~ 'high WFH'))

df2 = df2 %>% left_join(wfh_buckets, by = c('iso', 'city'))

# Calculate the average donut_effect_cumulative for each date and wfh_bucket
average_data <- df2 %>% group_by(date, wfh_bucket) %>%
  summarise(avg_donut_effect = mean(donut_effect_cumulative, na.rm = TRUE))

colors <- c(cardinal, teal, green) 

#average_data = read_csv('./data/output/fig5a.csv')
average_data %>% mutate(emph='b') %>% ggplot(
  aes(x=date, y = avg_donut_effect, color=as.factor(wfh_bucket), linetype = as.factor(emph), 
      alpha = as.factor(emph), size = as.factor(emph)), guide='none') +
  geom_line() +
  scale_x_date(date_labels="%Y", date_breaks  ="1 year") +
  xlim(as.Date(start_date), as.Date('2024-06-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = wfh_bucket), method = list(cex = 1.8,"last.points","bumpup")) +
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Relative growth in spending index\ncity center-outside, % points",
       size = 10
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=95, size = 8, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=24),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 24),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)
  )

# Print the plot
average_data %>% write_csv('./data/output/fig5a.csv') 
ggsave('figures-tables/fig5a_wfh_bucket_mcard.png', width = 10, height = 8)

# B. US, USPS (calculate pooled donut within each group (like figure 3))
# Read in wfh data
wfh = read_excel('./data/external_data/remote_work_in_job_ads_signup_data.xlsx', sheet = 'us_city_by_month') %>%
  mutate(MetroShort = paste(city, state, sep = ", ")) %>%
  filter(year_month >= 2023, measurement == '1 Month Average') %>%
  group_by(MetroShort) %>% summarise(metro_wfh_share = mean(percent, na.rm=TRUE))
# Read in metro charts data
metro_pop = read_csv('./data/msa_all_chars.csv') %>% select(MetroShort, `2019 Population`) %>% rename(population = `2019 Population`)
# Merge wfh with metro char
wfh = wfh %>% inner_join(metro_pop, by = 'MetroShort')

#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% dplyr::select(!estab_count) %>%
  filter(!is.na(dist_to_cbd)) %>% mutate(county_fips = as.double(state)*1000+as.double(county)) %>%
  left_join(wfh, by = 'MetroShort') %>% filter(!is.na(population))

# read in zip code level flows
usps <- read_csv('./data/USPS_zips.csv') #read in zip code level USPS flow data

## A. population flows
#construct dataset
temp <- chars %>% filter(!is.na(`2019 Population`)) %>%
  mutate(quantile_rank = 'cbd') %>% group_by(MetroShort) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= cbd_radius, ntile(density2019[dist_to_cbd>=cbd_radius], 10)),
         category = ifelse(quantile_rank == 'cbd', 'center', 'outside')) %>%
  inner_join(usps, by = 'zip') %>% mutate(date = as.Date(date)) 


wfh_buckets = temp %>% select(MetroShort, metro_wfh_share) %>% unique() %>% ungroup() %>% filter(!is.na(metro_wfh_share)) %>%
  mutate(wfh_ntile = ntile(metro_wfh_share, 3), wfh_bucket = case_when(wfh_ntile %in% c(1, 2) ~ 'low WFH', TRUE ~ 'high WFH'))

df2 = temp %>% left_join(wfh_buckets, by = c('MetroShort')) %>% 
  group_by(wfh_bucket, category, date) %>%
  summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
  filter(date >= as.Date('2018-01-01'), category %in% c('center', 'outside'))

df3 = df2 %>% group_by(wfh_bucket, category) %>%
  mutate(net_pop_avg = zoo::rollmean(net_pop, 24, align='right', na.pad=TRUE),
         net_pop = net_pop - net_pop_avg[date == as.Date('2019-12-15')],
         net_pop = cumsum(net_pop)) %>% #comment this line out for monthly flows
  pivot_wider(id_cols = c(wfh_bucket, date), names_from = category, values_from = net_pop) %>%
  mutate(dif = `center` - outside)

colors <- c(cardinal, teal, green) 

df3 %>% mutate(emph='b') %>% ggplot(
  aes(x=date, y = dif, color=as.factor(wfh_bucket), linetype = as.factor(emph), 
      alpha = as.factor(emph), size = as.factor(emph)), guide='none') +
  geom_line() +
  scale_x_date(date_labels="%Y", date_breaks  ="1 year") +
  xlim(as.Date(start_date), as.Date('2024-06-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = wfh_bucket), method = list(cex = 1.8,"last.points","bumpup")) +
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Relative cumulative devs from pre-pandemic flow\ncity center-outside, % points",
       size = 15
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-12-01'), y=95, size = 8, color = 'black',
  ) +
  theme_minimal()+
  theme(text = element_text(size=24),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 24),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)
  )

ggsave('figures-tables/fig5b_wfh_bucket_usps.png', width = 10, height = 8)

########################################
#APPENDIX FIGURES
########################################
#Figure S1: See Barrero, Bloom and Davis (2021) or wfhresearch.com

########################################
# Figure S2. Individual cities
#######################wade#############
df = read_csv('./data/external_data/mastercard/donut-effect-main.csv') %>% mutate(date = as.Date(date))
city_chars=read_csv('./data/global_city_chars.csv')
df2 = df %>% select(old_city_name, iso, date, distance_val, distance_column, donut_effect_cumulative) %>% 
  left_join(city_chars, by=c('old_city_name', 'iso')) %>% filter(!is.na(country), distance_val %in% c(30)) %>%
  group_by(date, city) %>% summarise(donut_effect_cumulative = mean(donut_effect_cumulative, na.rm=TRUE))

###Eliminate seasonal component (decomposs ts into seasonal, trend, and irregular components using loess smoothing)
deseasonalise <- function(data) { # Function to deseasonalize time series using stl
  ts_data <- ts(data$donut_effect_cumulative, frequency = 12)  # assuming monthly data, adjust frequency accordingly
  decomposed <- stl(ts_data, s.window = "periodic")
  seasonally_adjusted <- seasadj(decomposed)
  return(data.frame(data$date, data$donut_effect_cumulative, donut_deseasonalised = seasonally_adjusted))
}

# Applying the function to each group (distance_column) and binding the results back together
df3 <- df2 %>% group_by(city) %>%
  group_modify(~ deseasonalise(.), .keep = TRUE) %>% ungroup()

cities1 = c('New York', 'Sydney', 'Toronto', 'Berlin', 'Helsinki')
cities2 = c('Budapest', 'Kuala Lumpur', 'Jakarta', 'Lima', 'Cairo')
df4 = df3 %>% filter(city %in% c(cities1, cities2))
#df4 %>% read_csv('./data/output/fig2s.csv')

df4 %>% filter(city %in% cities1) %>% mutate(emph='b') %>% ggplot(
  aes(x=data.date, y = donut_deseasonalised, color=city, linetype = as.factor(emph), 
      alpha = as.factor(emph), size = as.factor(emph)), guide='none') +
  geom_line() +
  scale_x_date(date_labels="%Y", date_breaks  ="1 year") +
  xlim(as.Date(start_date), as.Date('2024-04-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = city), method = list(cex = 1.2,"last.points","bumpup")) +
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "City center - outer ring\n Difference in normalized spending",
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

#save file
ggsave('./figures-tables/figs2a.png', plot = last_plot(), width = 10, height = 8)

df3 %>% filter(city %in% cities2) %>% mutate(emph='b') %>% ggplot(
  aes(x=data.date, y = donut_deseasonalised, color=city, linetype = as.factor(emph), 
      alpha = as.factor(emph), size = as.factor(emph)), guide='none') +
  geom_line() +
  scale_x_date(date_labels="%Y", date_breaks  ="1 year") +
  xlim(as.Date(start_date), as.Date('2024-06-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = city), method = list(cex = 1.2,"last.points","bumpup")) +
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "City center - outer ring\n Difference in normalized spending",
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
#save file
ggsave('./figures-tables/figs2b.png', plot = last_plot(), width = 10, height = 8)
df4 %>% write_csv('./data/output/fig2s.csv') 

####################################
# Figure S3: San Francisco + London heat maps
####################################
#A: San Francisco
#https://arilamstein.com/documentation/choroplethrZip/reference/zip_choropleth.html
#install_github('arilamstein/choroplethrZip@v1.5.0') #Run this line to use the choroplethrZip library
#Install package 'devtools' in order to run install_github()
library(choroplethrZip)

#create dataset in format required for choroplehtrZip
df <- read_csv('./data/usps_panel_zips_maps.csv', col_types = cols(zip = col_character())) %>%
  mutate(value = round(post_pop, digits = 2),
         zip = ifelse(nchar(zip)==4, paste('0', zip, sep=''), zip)) %>%
  dplyr::select(zip, value) %>%
  rename(region = zip) %>%
  filter(!is.na(zip), !is.na(value))

## Create San Francisco map
df_choro <- df
#df_choro$value <- cut(df_choro$value, breaks = c(-35, -10, -5, -2, 0, .5, 2, 10))
choro = ZipChoropleth$new(df_choro)
choro$ggplot_scale = scale_fill_brewer(name="Net inflows/pop (%)", palette = 'RdYlGn')
choro$set_zoom_zip(state_zoom=NULL, county_zoom=NULL, msa_zoom='San Francisco-Oakland-Hayward, CA', zip_zoom=NULL)
choro$render()
ggsave('./figures-tables/figS3a.png', plot = last_plot(), width = 10, height = 8)

# B: London from Mastercard

####################################
# Figure S4a. Donut vs distance, average val in past year, USPS
####################################
library(Hmisc)
start_date = '2018-01-01'; end_date = '2023-07-01'
chars <- read_csv('./data/zip_all_chars_cbd.csv', col_types = cols('zip' = col_integer()))
usps <- read_csv('./data/USPS_zips.csv') %>% filter(zip != 2047)

## A. population flows
#construct dataset
temp <- chars %>% filter(MetroShort %in% cities, !is.na(`2019 Population`)) %>%
  mutate(dist_rank =  cut(dist_to_cbd/meters_per_mile, 
                          breaks=c(0,2,5,10,15,20,25,30,35,40,45, 50, Inf), 
                          labels=c(2,5,10,15,20,25,30,35,40,45, 50, Inf))) %>%
  inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date(start_date), date < as.Date(end_date)) %>%
  group_by(dist_rank, zip) %>% mutate(net_pop = na.approx(net_pop, na.rm=FALSE)) 

zips_to_keep = temp %>% filter(date == '2019-12-15') %>% select(zip) #identify zip codes that don't have a Dec 2019 value even after interpolation

temp2 = temp %>% semi_join(zips_to_keep, by='zip') %>%
  mutate(net_pop = (net_pop/`2019 Population`)*100) %>%
  group_by(dist_rank, zip) %>%
  mutate(net_pop_avg = zoo::rollmean(net_pop, 24, align='right', na.pad=TRUE),
         net_pop = net_pop - net_pop_avg[date == as.Date('2019-12-15')], emph='b') %>%
  filter(date >= as.Date('2020-02-15')) %>%
  group_by(dist_rank, zip) %>% summarise(net_pop_total = sum(net_pop, na.rm=TRUE),
                                         `2019 Population` = sum(`2019 Population`, na.rm=TRUE)) %>%
  group_by(dist_rank) %>%
  summarise(net_pop_mean = wtd.mean(net_pop_total,`2019 Population`),
                                   net_pop_sd = sqrt(wtd.var(net_pop_total, `2019 Population`))/sqrt(n())) %>%
  mutate(dist_rank=as.double(as.character(dist_rank)), group='10 ranks')

temp2 = bind_rows(temp2, data.frame(dist_rank=0, net_pop_mean=0, net_pop_sd=0, group='year 3'))

temp2 %>% ggplot(aes(x = dist_rank, y = net_pop_mean, color=group)) + 
  geom_point(shape = 21, aes(fill = cardinal), size = 3) + 
  geom_ribbon(data = temp2, aes(ymin = net_pop_mean - net_pop_sd, 
                              ymax = net_pop_mean + net_pop_sd),
              alpha = 0.2) + 
  labs(x  = "Distance from city center", 
       y = "Avg devs from pre-pandemic flow (% points)",
       size = 10
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position="none"
  ) 

ggsave("./figures-tables/figs4a_usps_distance.png", width=10, height=8)

####################################
# Figure S4b. Donut vs distance, average val in past year, Mastercard
####################################
df = read_csv('./data/external_data/mastercard/donut-effect-main.csv') %>%  mutate(date = as.Date(date))
city_chars=read_csv('./data/global_city_chars.csv')
df = df %>% select(old_city_name, iso, date, distance_column, distance_val, donut_effect_ring, donut_effect_cumulative) %>% 
  left_join(city_chars, by=c('old_city_name', 'iso')) 

# March 2020 till Sep 2022 is 31 months, till May 2022 is 26 months
df2 = df %>% filter(date >= as.Date("2022-10-01"), date < as.Date("2023-10-01")) %>%
  group_by(city, distance_val) %>% summarise(donut_cumulative_post=-1*mean(donut_effect_cumulative, na.rm=TRUE),
                                             population = mean(population2, na.rm=TRUE)) %>%
  group_by(city) %>% filter(n() == 10) %>% #filters out cities that have missing values
  group_by(distance_val) %>% summarise(donut_cumulative_mean = wtd.mean(donut_cumulative_post, population),
                                       donut_cumulative_sd = sqrt(wtd.var(donut_cumulative_post, population))/sqrt(n())) %>%
  mutate(group='year 3')

df2 = rbind(df2, c(0, 0, 0, 'year 3'))
df2 = df2 %>% mutate(across(c(distance_val, donut_cumulative_mean, donut_cumulative_sd), as.numeric))

#df2 = read_csv('./data/output/figs4b.csv')

df2 %>% ggplot(aes(x = distance_val, y = donut_cumulative_mean, color = group)) + 
  geom_point() +
  geom_ribbon(data = df2, aes(ymin = donut_cumulative_mean - donut_cumulative_sd, 
                  ymax = donut_cumulative_mean + donut_cumulative_sd),
              alpha = 0.2) + 
  labs(x  = "Distance from city center", 
       y = "Average relative spending growth\nCity center - outer ring, % points",
       size = 10
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position = 'none'
  ) 
ggsave('./figures-tables/figs4b_donut_by_distance_all.png', plot = last_plot(), width = 10, height = 8)
df2 %>% write_csv('./data/output/figs4b.csv')

###########################################
## Figure S5. Data Axle donut-movers
###########################################
#### See net_outflow_distribution.R


###########################################
## Figure S6. metro level pop/bus flow charts
###########################################
## Figure S6(a) msa level chart for population flows
msa_chars <- msa_chars <- read_csv('./data/msa_all_chars.csv') %>% filter(!is.na(MsaName)) %>%
  mutate(msa_pop_rank = dense_rank(desc(`2019 Population`)),
         msa_pop_group = case_when(msa_pop_rank %in% c(1:12) ~ 'top12',
                                   msa_pop_rank %in% c(13:50) ~ '13-50',
                                   TRUE ~ '51+')) %>%
  dplyr::select(MetroShort, msa_pop_group)

#read in all zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv') %>% filter(!is.na(`2019 Population`))

#combine chars
chars_all <- chars %>% filter(!is.na(`2019 Population`)) %>% left_join(msa_chars, by='MetroShort') %>%
  mutate(msa_pop_group=replace_na(msa_pop_group, 'rural'))
#read in zip code level USPS flow data
usps <- read_csv('./data/USPS_zips.csv')

## A. population flows
#construct dataset
temp <- chars_all %>% inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date(start_date), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(net_pop = na.approx(net_pop, na.rm=FALSE)) %>%
  group_by(msa_pop_group, date) %>%
  summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100) %>%
  group_by(msa_pop_group) %>% 
  mutate(net_pop_avg = zoo::rollmean(net_pop, 24, align='right', na.rm=TRUE, na.pad=TRUE),
         net_pop = cumsum(net_pop - net_pop_avg[date == as.Date('2019-12-15')])) %>%
  mutate(emph="b", val = net_pop, name = msa_pop_group)

#make plot
temp %>% ggplot(
  aes(x=date, y = val, color = name, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date(start_date), as.Date(end_date_long)) + 
  ylim(-1, 2) + #(-.2, 2)
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = name), method = list("last.points","bumpup", cex = 1.2)) +   
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Cumulative devs from pre-pandemic flow (% points)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2020-03-01'), y=1.5, size = 6, color = 'black') +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#save plot
ggsave('./figures-tables/figs6a.png', plot = last_plot(), width = 10, height = 8)

##B. Business flows
#construct dataset
temp <- chars_all %>% inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date(start_date), date < as.Date(end_date)) %>%
  group_by(zip) %>% mutate(net_bus = na.approx(net_bus, na.rm=FALSE)) %>%
  group_by(msa_pop_group, date) %>%
  summarise(net_bus = sum(net_bus, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
  group_by(msa_pop_group) %>% 
  mutate(net_bus_avg = zoo::rollmean(net_bus, 12, align='right', na.pad=TRUE),
         net_bus = cumsum(net_bus - net_bus_avg[date == as.Date('2020-02-15')]),
         net_bus = net_bus - net_bus[date==as.Date('2020-02-15')]) %>%
  mutate(emph="b", val = net_bus, name = msa_pop_group)

#make plot
temp %>% ggplot(
  aes(x=date, y = val, color = name, linetype = as.factor(emph), 
      alpha = as.factor(emph),size = as.factor(emph)), guide='none') +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  geom_line()+
  xlim(as.Date(start_date), as.Date(end_date_long)) + 
  ylim(-.05, .05) + #(-.2, 2)
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = name), method = list("last.points","bumpup", cex = 1.2)) +   
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "Cumulative devs from pre-pandemic flow (% points)",
       size = 7
  )+
  geom_text(label="Feb 2020", x=as.Date('2019-11-01'), y=0.025, size = 6, color = 'black') +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  )
#save plot
ggsave('./figures-tables/figs6b.png', plot = last_plot(), width = 10, height = 8)


###########################################
## Figures S7. Commute data from Inrix
###########################################
## FigureS7a. trips by hour distribution, weekdays
df = read_excel('./data/external_data/inrix.xlsx')
top_metros = df %>% group_by(MetroShort) %>% summarise(max_pop = max(`2019 Population`, na.rm=TRUE)) %>% 
  arrange(desc(max_pop)) %>% slice_head(n=20)
df2 = df %>% filter(MetroShort %in% top_metros$MetroShort) %>% 
  mutate(time = TOD + bucket_start_minute/60)

df2 %>% filter(trip_day_of_week %in% c(1:5)) %>% #weekday
  group_by(year, time) %>%
  summarise(total_trips = sum(trip_count, na.rm=TRUE)/5) %>% 
  ggplot(aes(x=time, y = total_trips, fill=as.factor(year), color=as.factor(year))) +
  geom_line(size=1.5) + 
  geom_ribbon(aes(ymin=0, ymax=total_trips), alpha=0.1) +
  geom_text(label="2023", x=15, y=2.6e4, size = 6, color = cardinal) +
  geom_text(label="2019", x=11, y=5e4, size = 6, color = teal) +
  labs(x = "Hour of Day", y = "Avg number of trips per day", color = "Year") +
  theme_minimal() + 
  scale_colour_manual(values = c(teal, cardinal))+
  scale_fill_manual(values = c("lightblue", "pink"))+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position='none'
  ) 
ggsave('./figures-tables/figs7a.png', plot = last_plot(), width = 10, height = 8)

## Figure S7b. mean speed per hour
df2 %>% filter(trip_day_of_week %in% c(1:5)) %>% #weekday
  group_by(year, time) %>%
  summarise(mean_speed = weighted.mean(mean_speed_v2, na.rm=TRUE)) %>% 
  ggplot(aes(x=time, y = mean_speed, fill=as.factor(year), color=as.factor(year))) +
  geom_line(size=1.5) + 
  geom_ribbon(aes(ymin=30, ymax=mean_speed), alpha=0.1) +
  geom_text(label="2023", x=8, y=55, size = 6, color = cardinal) +
  geom_text(label="2019", x=7.5, y=36, size = 6, color = teal) +
  labs(x = "Hour of Day", y = "Mean speed of trip", color = "Year") +
  theme_minimal() + 
  scale_colour_manual(values = c(teal, cardinal))+
  scale_fill_manual(values = c("lightblue", "pink"))+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position='none'
  ) 
ggsave('./figures-tables/figs7b.png', plot = last_plot(), width = 10, height = 8)

## Extra: trips by hour distribution, weekend
df2 %>% filter(trip_day_of_week %in% c(6:7)) %>% #weekend
  group_by(year, time) %>%
  summarise(total_trips = sum(trip_count, na.rm=TRUE)/2) %>% 
  ggplot(aes(x=time, y = total_trips, fill=as.factor(year), color=as.factor(year))) +
  geom_line(size=1.5) + 
  geom_ribbon(aes(ymin=0, ymax=total_trips), alpha=0.1) +
  geom_text(label="2023", x=17, y=2.8e4, size = 6, color = cardinal) +
  geom_text(label="2019", x=14, y=2.2e4, size = 6, color = teal) +
  labs(x = "Hour of Day", y = "Avg number of trips per day", color = "Year") +
  theme_minimal() + 
  scale_colour_manual(values = c(teal, cardinal))+
  scale_fill_manual(values = c("lightblue", "pink"))+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position='none'
  ) 
ggsave('./figures-tables/figs7c.png', plot = last_plot(), width = 10, height = 8)

## Extra: mean speed per hour
df2 %>% filter(trip_day_of_week %in% c(6:7)) %>% #weekend
  group_by(year, time) %>%
  summarise(mean_speed = weighted.mean(mean_speed_v2, trip_count, na.rm=TRUE)) %>% 
  ggplot(aes(x=time, y = mean_speed, fill=as.factor(year), color=as.factor(year))) +
  geom_line(size=1.5) + 
  geom_ribbon(aes(ymin=40, ymax=mean_speed), alpha=0.1) +
  geom_text(label="2023", x=13, y=47, size = 6, color = cardinal) +
  geom_text(label="2019", x=15, y=52, size = 6, color = teal) +
  labs(x = "Hour of Day", y = "Mean speed of trip", color = "Year") +
  theme_minimal() + 
  scale_colour_manual(values = c(teal, cardinal))+
  scale_fill_manual(values = c("lightblue", "pink"))+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.position='none'
  ) 
ggsave('./figures-tables/figs7d.png', plot = last_plot(), width = 10, height = 8)


###########################################
## Figure S8a. Miles driven
# https://fred.stlouisfed.org/series/TRFVOLUSM227SFWA
###########################################
df=read_csv('https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=718&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=TRFVOLUSM227SFWA&scale=left&cosd=2000-01-01&coed=2023-10-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-12-29&revision_date=2023-12-29&nd=2000-01-01') %>% rename(date = DATE, miles = TRFVOLUSM227SFWA) %>%
  filter(date > as.Date('2015-01-01')) %>% mutate(date=as.Date(date))

# Fit a model using data from 2015 to 2019
fit <- lm(miles ~ date, data = filter(df, date <= as.Date("2019-12-01")))

# Prediction
predicted_miles <- predict(fit, newdata = data.frame(date = df$date))
gap <- df$miles[df$date == max(df$date)] - predicted_miles[df$date == max(df$date)]

# Visualization
ggplot(df, aes(x = date, y = miles, linetype = as.factor("b"), 
               alpha = as.factor("b"),size = as.factor("b")), guide='none') + 
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  xlim(as.Date('2015-01-01'), as.Date(end_date_long)) + 
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  #ylim(0, predicted_miles[df$date == max(df$date)]*1.1) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_text(label="Feb 2020", x=as.Date('2019-08-01'), y=2.25e5, size = 6, color = 'black') +
  geom_line() +  # Original data
  geom_line(aes(y = predicted_miles), linetype = "dashed", color = "red") +  # Trend line
  geom_point(x = max(df$date), y = df$miles[df$date == max(df$date)], color = "blue", size = 2) +
  geom_segment(aes(x = max(df$date), xend = max(df$date), 
                   y = df$miles[df$date == max(df$date)], 
                   yend = predicted_miles[df$date == max(df$date)]),
               linetype = "dotted", color = "blue") +
  geom_text(aes(x = max(df$date), 
                y = miles[date == max(df$date)], 
                label = "6% shortfall"),
            vjust = -2.5, hjust = 1, color = "blue", size = 4) +
  labs(x = "", y = "Miles (millions)") +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  ) 

ggsave("./figures-tables/figs2a_miles_driven.png", width=10, height=8)

###########################################
## Figure S8b. Public transport has collapsed
## data: https://www.transit.dot.gov/ntd/data-product/monthly-module-adjusted-data-release
## UPT = unlinked passenger trips (full definition here: https://www.transit.dot.gov/ntd/national-transit-database-ntd-glossary)
###########################################
df=read_excel('./data/external_data/202310-fta-adj-est.xlsx', sheet = 'UPT') %>%
  pivot_longer(cols = 11:ncol(.), names_to='date', values_to='trips') %>%
  mutate(date = as.Date(as.yearmon(date, format="%m/%Y"))) %>% rename(mode = `3 Mode`) %>%
  filter(date >= as.Date('2015-01-01'), mode %in% c('Bus', 'Rail')) %>%
  group_by(date) %>% summarise(trips = sum(trips, na.rm=TRUE)) 

# Fit a model using data from 2015 to 2019
fit <- lm(trips ~ date, data = filter(df, date <= as.Date("2019-12-01")))

# Prediction
predicted_trips <- predict(fit, newdata = data.frame(date = df$date))
gap <- df$trips[df$date == max(df$date)] - predicted_trips[df$date == max(df$date)]

# Visualization
ggplot(df, aes(x = date, y = trips, linetype = as.factor("b"), 
               alpha = as.factor("b"),size = as.factor("b")), guide='none') + 
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  xlim(as.Date('2015-01-01'), as.Date(end_date_long)) + 
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  #ylim(0, predicted_trips[df$date == max(df$date)]*1.1) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_text(label="Feb 2020", x=as.Date('2019-08-01'), y=5e8, size = 6, color = 'black') +
  geom_line() +  # Original data
  geom_line(aes(y = predicted_trips), linetype = "dashed", color = "red") +  # Trend line
  geom_point(x = max(df$date), y = df$trips[df$date == max(df$date)], color = "blue", size = 2) +
  geom_segment(aes(x = max(df$date), xend = max(df$date), 
                   y = df$trips[df$date == max(df$date)], 
                   yend = predicted_trips[df$date == max(df$date)]),
               linetype = "dotted", color = "blue") +
  geom_text(aes(x = max(df$date), 
                y = trips[date == max(df$date)]*1.05, 
                label = "30% shortfall"),
            vjust = -2.5, hjust = 1.25, color = "blue", size = 4) +
  labs(x="", y = "trips") +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  ) 

ggsave("./figures-tables/figs2b_pub_transport_trips.png", width=10, height=8)

####################################
# Figure S9a. Observed WFH from GSWA vs global donut
####################################
post_date_2yr = "2022-10-01"
post_date_3yr = "2023-10-01"
wfh = read_excel('./data/external_data/GSWA-Figures-Data-2023.xlsx', 
                 sheet='Figure 1', range="A1:C36") %>% rename(country = Country, wfh_days = `Number of days working from home this week`)

### Read in mastercard data and merge
df = read_csv('./data/external_data/mastercard/donut-effect-main.csv') %>% mutate(date = as.Date(date))
city_chars=read_csv('./data/global_city_chars.csv') %>% group_by(iso) %>% slice_max(order_by = population2, n = 1)
df2 = df %>% select(old_city_name, iso, date, distance_column, donut_effect_cumulative) %>% 
  inner_join(city_chars, by = c('old_city_name', 'iso'))

###which countries are in the wfh dataset but not in mastercard?
#wfh_mastercard = df2 %>% group_by(iso) %>% tally() %>% right_join(wfh, by = 'iso') %>% write_csv('./data/countries_in_wfh_not_mastercard.csv')

### Plot
df_post = df2 %>% filter(date >= as.Date(post_date_2yr), date < as.Date(post_date_3yr))  %>% 
  filter(distance_column == '30 mile') %>%
  group_by(city, iso) %>% summarise(donut_post=-1*mean(donut_effect_cumulative, na.rm=TRUE),
                                    population = mean(population2, na.rm=TRUE)) %>%
  inner_join(wfh, by='iso')

#df_post = read_csv('./data/output/figs9a.csv')

#get stats
model = lm(donut_post ~ wfh_days, weights=population, data = df_post)
summary_model <- summary(model)
slope <- summary_model$coefficients["wfh_days", "Estimate"]
intercept <- summary_model$coefficients["(Intercept)", "Estimate"]
r_squared <- summary_model$r.squared
t_value <- summary_model$coefficients["wfh_days", "t value"]
equation_text <- sprintf("y = %.2fx + %.2f\nR² = %.2f, slope t-stat = %.2f", slope, intercept, r_squared, t_value)

# create a scatterplot with a trendline
ggplot(df_post, aes(wfh_days, donut_post)) + 
  geom_point(size=1) + geom_smooth(method="lm", mapping = aes(weight=population), show.legend=TRUE) + 
  geom_dl(aes(label = iso), method = list(cex = 0.8, "last.points", "bumpup")) +
  annotate("text", x = .7, y = -80, label = equation_text, hjust = 0, vjust = 0, size = 5) +
  labs(x  = "WFH days per week in 2023, GSWA", 
       y = "Spending growth, outer ring - city center\nAvg from Sep 2022-23",
       size = 10
  ) +
  theme_minimal()+
  theme(text = element_text(size=20),
        plot.title= element_text(hjust = 0.5, family = "serif"),
        plot.caption = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)
  ) 

ggsave("./figures-tables/figs9a_gswa_wfh_donut.png", width=10, height=8)
df_post %>% write_csv('./data/output/figs9a.csv')

####################################
# Figure S9b. Observed WFH from WFHMap vs Donut Size, US only
####################################
wfh = read_excel('./data/external_data/remote_work_in_job_ads_signup_data.xlsx', sheet = 'us_city_by_month') %>%
  mutate(MetroShort = paste(city, state, sep = ", ")) %>%
  filter(year_month >= 2023, measurement == '1 Month Average') %>%
  group_by(MetroShort) %>% summarise(wfh_share = mean(percent, na.rm=TRUE))

#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv', 
                  col_types = cols('zip' = col_integer())) %>% dplyr::select(!estab_count) %>%
  filter(!is.na(dist_to_cbd)) %>% mutate(county_fips = as.double(state)*1000+as.double(county))

msa_chars = read_csv('./data/msa_all_chars.csv')

#read in zip code level USPS flow data
usps <- read_csv('./data/USPS_zips.csv')

## A. population flows
#construct dataset
temp <- chars %>% filter(!is.na(`2019 Population`)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MetroShort) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= cbd_radius, ntile(density2019[dist_to_cbd>=cbd_radius], 10)),
         category = ifelse(quantile_rank == 'cbd', 'city center',
                           case_when(quantile_rank == 10 ~ 'high density',
                                     quantile_rank %in% c(6:9) ~ 'suburb',
                                     TRUE ~ 'outer suburb'))) %>%
  inner_join(usps, by = 'zip') %>% 
  mutate(date = as.Date(date)) %>%
  group_by(MetroShort, category, date) %>%
  summarise(net_pop = sum(net_pop, na.rm = TRUE)/sum(`2019 Population`)*100) %>% 
  filter(date >= as.Date(start_date), category %in% c('city center', 'outer suburb'))

counts = temp %>% group_by(MetroShort, category) %>% summarise(n = n_distinct(date)) %>%
  group_by(MetroShort) %>% summarise(n = sum(n)) %>% filter(n>=132)

temp_filtered = temp %>% semi_join(counts, by = "MetroShort") %>%
  group_by(MetroShort, category) %>%
  mutate(net_pop_avg = zoo::rollmean(net_pop, 24, align='right', na.pad=TRUE),
         net_pop = cumsum(net_pop - net_pop_avg[date == as.Date('2019-12-15')])) %>%
  pivot_wider(id_cols = c(MetroShort, date), names_from = category, values_from = net_pop) %>%
  mutate(dif = `outer suburb` - `city center`) %>%
  filter(date == as.Date('2023-02-15'))

temp_filtered_inc = temp_filtered %>% inner_join(msa_chars, by = 'MetroShort') %>%
  inner_join(wfh, by='MetroShort') %>% mutate(population = `2019 Population`)
#temp_filtered_inc = read_csv('./data/output/figs9b.csv')

#prepare binscatter
model = lm(dif ~ wfh_share, weights=population, data = temp_filtered_inc)
summary_model = coeftest(model, vcov=vcovCL, cluster=~MetroShort)
slope <- summary_model["wfh_share", "Estimate"]
intercept <- summary_model["(Intercept)", "Estimate"]
t_value <- summary_model["wfh_share", "t value"]
r_squared <- summary(model)$r.squared
equation_text <- sprintf("y = %.4fx + %.2f\nR² = %.2f, slope t-stat = %.2f", slope, intercept, r_squared, t_value)

binscatter = binsreg(y=temp_filtered_inc$dif, x=temp_filtered_inc$wfh_share, nbins=30, polyreg=1,
                     bycolors = teal, weights = temp_filtered_inc$population, vce='HC1', cluster=temp_filtered_inc$MetroShort)
data.dots <- binscatter$data.plot$`Group Full Sample`$data.dots
data.line <- binscatter$data.plot$`Group Full Sample`$data.poly

ggplot() + geom_point(data=data.dots, aes(x=x, y=fit), size=3, colour=teal) +
  geom_line(data=data.line, aes(x=x, y=fit), size = 1.5, colour=teal) +
  theme_bw() + labs(x="City's observed WFH share from WFH Map, 2023 avg", y="Cumulative net pop ouflow till Feb 2023\nOuter ring - city center") +
  annotate("text", x = 20, y = 2, label = equation_text, hjust = 0, vjust = 0, size = 5) + 
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.border = element_blank())

ggsave('./figures-tables/figs9b_donut_vs_wfhmap.png', plot = last_plot(), width = 10, height = 8)
temp_filtered_inc %>% write_csv('./data/output/figs9b.csv')

####################################
# Figure S10a. Mastercard by category (World) 
####################################
df = read_csv('./data/external_data/mastercard/global-donut-spend-categories.csv') %>% mutate(date = as.Date(date)) %>% filter(distance_column == '30 mile distance')
city_chars=read_csv('./data/global_city_chars.csv')

df2 = df %>% select(cleansed_city_name, dw_merch_country_cd, industry_name, date, donut_effect_cumulative) %>% 
  rename(old_city_name=cleansed_city_name, iso=dw_merch_country_cd) %>%
  left_join(city_chars, by=c('old_city_name', 'iso')) %>% filter(!is.na(iso), !is.na(population2), date < as.Date('2024-01-01')) %>%
  group_by(industry_name, date) %>% summarise(donut_effect_cumulative = weighted.mean(donut_effect_cumulative, population2, na.rm=TRUE))

###Eliminate seasonal component (decomposs ts into seasonal, trend, and irregular components using loess smoothing)
deseasonalise <- function(data) { # Function to deseasonalize time series using stl
  ts_data <- ts(data$donut_effect_cumulative, frequency = 12)  # assuming monthly data, adjust frequency accordingly
  decomposed <- stl(ts_data, s.window = "periodic")
  seasonally_adjusted <- seasadj(decomposed)
  return(data.frame(data$date, donut_deseasonalised = seasonally_adjusted))
}

# Applying the function to each group (distance_column) and binding the results back together
df3 <- df2 %>% group_by(industry_name) %>%
  group_modify(~ deseasonalise(.), .keep = TRUE) %>% ungroup() %>%
  mutate(industry_name = sub("Grocery & Wholesale Clubs", "Grocery &\nBulk Stores", industry_name),
         industry_name = sub("Restaurants & Bars", "Restaurants &\nBars", industry_name))

#df3 = read_csv('./data/output/figs10a.csv')

df3 %>% mutate(emph='b') %>% ggplot(
  aes(x=data.date, y = donut_deseasonalised, color=industry_name, linetype = as.factor(emph), 
      alpha = as.factor(emph), size = as.factor(emph)), guide='none') +
  geom_line() +
  scale_x_date(date_labels="%Y", date_breaks  ="1 year") +
  xlim(as.Date(start_date), as.Date('2024-10-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = industry_name), method = list(cex = 1.2,"last.points","bumpup")) +
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "City center - outer ring\n Difference in normalized spending",
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

ggsave('figures-tables/figs10a_all_cities_categories.png', plot = last_plot(), width = 10, height = 8)
df3 %>% write_csv('./data/output/figs10a.csv')

####################################
# Figure S10b. Mastercard by region
####################################
df = read_csv('./data/external_data/mastercard/donut-effect-main.csv') %>% mutate(date = as.Date(date)) %>% filter(distance_column == '30 mile')
city_chars=read_csv('./data/global_city_chars_w_region.csv')

df2 = df %>% select(cleansed_city_name, iso, date, distance_column, donut_effect_cumulative) %>% 
  rename(old_city_name = cleansed_city_name) %>%
  left_join(city_chars, by=c('old_city_name', 'iso')) %>% filter(!is.na(iso), !is.na(population2), iso != 'USA') %>%
  select(old_city_name, date, donut_effect_cumulative, big_region, population2)

us = read_csv('./data/external_data/mastercard/donut-effect-usa.csv') %>% mutate(date = as.Date(date))  %>%
  filter(distance_column == '30 mile') %>% select(old_city_name, date, donut_effect_cumulative)
us_chars=read_csv('./data/us_city_chars.csv') %>% arrange(desc(`2019 Population`)) %>% slice_head(n = 12)
us2 = us %>% left_join(us_chars, by=c('old_city_name')) %>% 
  filter(!is.na(`2019 Population`)) %>%
  mutate(big_region = 'English') %>% rename(population2 = `2019 Population`) %>%
  select(old_city_name, date, donut_effect_cumulative, big_region, population2)

df3 = bind_rows(df2, us2)

df4 = df3 %>% group_by(big_region, date) %>% summarise(donut_effect_cumulative = weighted.mean(donut_effect_cumulative, population2))

###Eliminate seasonal component (decomposs ts into seasonal, trend, and irregular components using loess smoothing)
deseasonalise <- function(data) { # Function to deseasonalize time series using stl
  ts_data <- ts(data$donut_effect_cumulative, frequency = 12)  # assuming monthly data, adjust frequency accordingly
  decomposed <- stl(ts_data, s.window = "periodic")
  seasonally_adjusted <- seasadj(decomposed)
  return(data.frame(data$date, donut_deseasonalised = seasonally_adjusted))
}

# Applying the function to each group (distance_column) and binding the results back together
df5 <- df4 %>% group_by(big_region) %>%
  group_modify(~ deseasonalise(.), .keep = TRUE) %>% ungroup()

#df5 = read_csv('./data/output/figs10b.csv')

df5 %>% mutate(emph='b') %>% ggplot(
  aes(x=data.date, y = donut_deseasonalised, color=big_region, linetype = as.factor(emph), 
      alpha = as.factor(emph), size = as.factor(emph)), guide='none') +
  geom_line() +
  scale_x_date(date_labels="%Y", date_breaks  ="1 year") +
  xlim(as.Date(start_date), as.Date('2024-10-01')) + 
  geom_vline(xintercept=as.Date('2020-02-15'), size=.5, color="black") + 
  geom_dl(aes(label = big_region), method = list(cex = 1.2,"last.points","bumpup")) +
  scale_colour_manual(values = c(teal, cardinal, black, green, marmalade, blue), guide='none')+
  scale_size_manual(values = c(1.8, 1), guide="none")+
  scale_alpha_manual(values = c(1, 0.8), guide="none")+
  scale_linetype(guide='none') +
  labs(x  = "", 
       y = "City center - outer ring\n Difference in normalized spending",
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
#save file
ggsave('./figures-tables/figs10b_mastercard_regions.png', plot = last_plot(), width = 10, height = 8)
df5 %>% write_csv('./data/output/figs10b.csv')

