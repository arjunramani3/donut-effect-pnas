#####Get all MSA characteristics#####
#Msa = full metropolitan statistical area name with state
#MsaName = full metropolitan statistical are name without state
#Metro = shorter metro are name with state

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
packages <- c("tidyverse", "readxl")
lapply(packages, pkgTest);

setwd('~/Documents/donut-effect-pnas/')

##########################################

#read in density data
dens <- read_csv('./data/zip_all_chars_cbd.csv')

#WFH Dingel and Neiman 2020
wfh <- read_csv('https://raw.githubusercontent.com/jdingel/DingelNeiman-workathome/master/MSA_measures/output/MSA_workfromhome.csv') %>%
  rename(Msa = AREA_NAME,
         wfh_wage = teleworkable_wage,
         wfh_emp = teleworkable_emp) %>%
  separate(Msa, c("MsaName", "end"), ', ', remove = FALSE) %>% 
  mutate(MetroShort = sub("-.*", "", MsaName),
         MetroState = sub("-.*", "", end),
         MetroShort = paste(MetroShort, MetroState, sep = ', '))

#Zillow Research -> ZHVI at metro level (to get price levels)
df_price <- read_csv('https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv?t=1650568871') %>%
  rename(MetroShort = RegionName) %>%
  mutate(MetroShort = sub(",.*", "", MetroShort),
         MetroShort = sub("-.*", "", MetroShort),
         MetroShort = paste(MetroShort, StateName, sep = ', ')) %>% 
  pivot_longer(!c(RegionID, SizeRank, MetroShort, RegionType, StateName),
               names_to = 'date', values_to = 'zhvi') %>% 
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date('2019-01-01'), date < as.Date('2020-01-01')) %>%
  group_by(MetroShort) %>% summarise(
    price_level = mean(zhvi, na.rm=TRUE))

#CBD coordinates for each metro from Holian
cbd <- readxl::read_excel('./data/external_data/holian_cbd_geocodes.xlsx', 
                         sheet = 'copy_of_merged_data2') %>%
  mutate(cbd_lat = ifelse(!is.na(Cen82lat), Cen82lat, CityHallLat),
         cbd_lon = ifelse(!is.na(Cen82lon), Cen82lon, CityHallLon)) %>%
  filter(!is.na(cbd_lat) & !is.na(cbd_lon)) %>%
  separate(CBSA_name, c("MsaName", "end"), ', ', remove = FALSE) %>% 
  mutate(MetroShort = sub("-.*", "", MsaName),
         MetroState = sub("-.*", "", end),
         MetroState = sub(" .*", "", MetroState),
         MetroShort = paste(MetroShort, MetroState, sep = ', ')) %>%
  select(MetroShort, cbd_lon, cbd_lat)

#merge the three datasets
msa_chars <- dens %>% group_by(MetroShort) %>% 
  summarise(`2010 Population` = sum(`2010 Population`, na.rm = TRUE),
            `2019 Population` = sum(`2019 Population`, na.rm = TRUE),
            area = sum(area, na.rm = TRUE),
            land_area = sum(land_area, na.rm = TRUE),
            estab_count = sum(estab_count, na.rm = TRUE)
            ) %>%
  mutate(density = `2010 Population`/land_area,
         density2019 = `2019 Population`/land_area) %>%
  inner_join(wfh, by = 'MetroShort') %>%
  inner_join(df_price, by = 'MetroShort') %>%
  inner_join(cbd, by = 'MetroShort') %>%
  arrange(desc(`2019 Population`))

#export
write_csv(msa_chars, './data/msa_all_chars.csv')

