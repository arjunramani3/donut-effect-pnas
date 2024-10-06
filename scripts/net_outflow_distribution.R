###########################################
# USPS GSB address level dataset
# Create table of distribution of net outflows to see where people are going
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
packages <- c("tidyverse", "xtable", "stargazer")
lapply(packages, pkgTest);

cities <- c('San Francisco, CA', 'New York, NY', 'Chicago, IL', 'Boston, MA',
            'Los Angeles, CA', 'Washington, DC', 'Atlanta, GA', 'Miami, FL',
            'Philadelphia, PA', 'Dallas, TX', 'Houston, TX', 'Phoenix, AZ')

#set to your working directory
setwd('~/Documents/donut-effect-pnas/')

###########################################
# Read data directly
###########################################
df <- read_csv('./data/external_data/USPS_gsb/Zip_code_zip_code_flows_output_v3.csv') %>%
  mutate(ZIP_pre = as.double(ZIP_pre),
         ZIP_post = as.double(ZIP_post),
         total_count = count+spouses+children)
#total moves
print(sum(df$count))

#read in zip code characteristics
chars <- read_csv('./data/zip_all_chars_cbd.csv') %>%
  filter(!is.na(density2019), !is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = 'cbd') %>%
  group_by(MsaName) %>%
  mutate(quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(density2019[dist_to_cbd>=2000], 10)),
         category = ifelse(quantile_rank == 'cbd', 'cbd',
                           case_when(quantile_rank == 10 ~ 'high',
                                     quantile_rank %in% c(8:9) ~ 'mid',
                                     TRUE ~ 'low'))) %>%
  select('zip', `2019 Population`, category, 'MetroShort')

#read in metro level characteristics
msa_chars <- msa_chars <- read_csv('./data/msa_all_chars.csv') %>% filter(!is.na(MsaName)) %>%
  mutate(msa_pop_rank = dense_rank(desc(`2019 Population`)),
         msa_pop_group = ifelse(msa_pop_rank %in% c(1:12), 'top12', 'other')) %>%
  select(MetroShort, `2019 Population`, msa_pop_rank, msa_pop_group) %>%
  rename(msa_pop_19 = `2019 Population`)

#merge
df <- df %>% left_join(chars, by = c('ZIP_pre' = 'zip')) %>%
  left_join(chars, by = c('ZIP_post' = 'zip'), suffix = c('_pre', '_post')) %>%
  left_join(msa_chars, by = c('MetroShort_post' = 'MetroShort')) %>%
  mutate(msa_pop_group=replace(msa_pop_group, is.na(msa_pop_group), 'rural'),
         MetroShort_pre=replace(MetroShort_pre, is.na(MetroShort_pre), 'rural'),
         MetroShort_post=replace(MetroShort_post, is.na(MetroShort_post), 'rural'),
         metro_category_pre = ifelse((MetroShort_pre %in% cities), 'top12', 
                                     ifelse(MetroShort_pre != 'rural', 'other', 'rural')),
         metro_category_post = ifelse((MetroShort_post %in% cities), 'top12', 
                                      ifelse(MetroShort_post != 'rural', 'other', 'rural')),
         within=ifelse(MetroShort_pre==MetroShort_post, 'within', 'between'))

#share of moves within-metro
sum(df[df$within=='within', 'count'], na.rm = TRUE)/sum(df$count, na.rm = TRUE)

#Align rural categories?
df <- df %>% mutate(category_pre=replace(category_pre, MetroShort_pre=='rural', 'rural'),
                    category_post=replace(category_post, MetroShort_post=='rural', 'rural')) %>%
  filter(!((metro_category_post=='rural') & (category_post!='rural')),
         !((metro_category_pre=='rural') & (category_pre!='rural')))

#####################################
# Create OUTFLOW table
# we really have 7 destinations for people leaving city centers
# within: high, mid, low; between: high, mid, low; other (rural)

table1_out <- df %>% filter(MetroShort_pre %in% cities, category_pre=='cbd')%>%
  mutate(move_type = ifelse(within=='within', category_post, metro_category_post)) %>%
  group_by(move_type) %>% summarise(total_count=sum(total_count, na.rm=TRUE)) %>%
  pivot_wider(names_from=move_type, values_from=total_count)

# Create INFLOW table
table1_in <- df %>% filter(MetroShort_post %in% cities, category_post=='cbd')%>%
  mutate(move_type = ifelse(within=='within', category_pre, metro_category_pre)) %>%
  group_by(move_type) %>% summarise(total_count=sum(total_count, na.rm=TRUE)) %>%
  pivot_wider(names_from=move_type, values_from=total_count)

###########################################
# Pre-data
###########################################

df2 <- read_csv('./data/external_data/USPS_gsb/Zip_code_zip_code_flows__control__output_v3.csv') %>%
  mutate(ZIP_pre = as.double(ZIP_pre),
         ZIP_post = as.double(ZIP_post),
         total_count=count+spouses+children)

#total moves
print(sum(df2$count))

#merge
df2 <- df2 %>% left_join(chars, by = c('ZIP_pre' = 'zip')) %>%
  left_join(chars, by = c('ZIP_post' = 'zip'), suffix = c('_pre', '_post')) %>%
  left_join(msa_chars, by = c('MetroShort_post' = 'MetroShort')) %>%
  mutate(msa_pop_group=replace(msa_pop_group, is.na(msa_pop_group), 'rural'),
         MetroShort_pre=replace(MetroShort_pre, is.na(MetroShort_pre), 'rural'),
         MetroShort_post=replace(MetroShort_post, is.na(MetroShort_post), 'rural'),
         metro_category_pre = ifelse((MetroShort_pre %in% cities), 'top12', 
                                     ifelse(MetroShort_pre != 'rural', 'other', 'rural')),
         metro_category_post = ifelse((MetroShort_post %in% cities), 'top12', 
                                      ifelse(MetroShort_post != 'rural', 'other', 'rural')),
         within=ifelse(MetroShort_pre==MetroShort_post, 'within', 'between'))


#share of moves within-metro
sum(df2[df2$within=='within', 'total_count'], na.rm = TRUE)/sum(df2$total_count, na.rm = TRUE)

#table of moves by density group
df2 <- df2 %>% mutate(category_pre=replace(category_pre, MetroShort_pre=='rural', 'rural'),
                      category_post=replace(category_post, MetroShort_post=='rural', 'rural')
) %>%
  filter(!((metro_category_post=='rural') & (category_post!='rural')),
         !((metro_category_pre=='rural') & (category_pre!='rural')))

#####################################
# Create OUTFLOW table
# we really have 7 destinations for people leaving city centers
# within: high, mid, low; between: high, mid, low; other (rural)
table2_out <- df2 %>% filter(MetroShort_pre %in% cities, category_pre=='cbd') %>%
  mutate(move_type = ifelse(within=='within', category_post, metro_category_post)) %>%
  group_by(move_type) %>% summarise(total_count=sum(total_count, na.rm=TRUE)) %>%
  pivot_wider(names_from=move_type, values_from=total_count)

# Create INFLOW table
table2_in <- df2 %>% filter(MetroShort_post %in% cities, category_post=='cbd')%>%
  mutate(move_type = ifelse(within=='within', category_pre, metro_category_pre)) %>%
  group_by(move_type) %>% summarise(total_count=sum(total_count, na.rm=TRUE)) %>%
  pivot_wider(names_from=move_type, values_from=total_count)


###################################
# Combine two tables

tab_flow<-rbind(table1_out, table1_in, table2_out, table2_in)
tab_flow=tab_flow %>%select(-cbd)
tab_flow$total=rowSums(tab_flow)
tab_flow$names = c('post_out', 'post_in', 'pre_out', 'pre_in')
tab_flow=tab_flow[, c("names", "high", "mid", "low", 'top12', 'other', 'rural', 'total')]
tab_flow %>% write_csv('./data/output/cbd_net_flow_distribution.csv')

####################################
# Get populations

chars <- read_csv('./data/zip_all_chars_cbd.csv') %>%
  filter(!is.na(density2019), !is.na(`2019 Population`), !is.na(dist_to_cbd)) %>%
  mutate(quantile_rank = 'cbd', 
         quantile_rank = replace(quantile_rank, dist_to_cbd >= 2000, ntile(density2019[dist_to_cbd>=2000], 10)),
         category = ifelse(quantile_rank == 'cbd', 'cbd',
                           case_when(quantile_rank == 10 ~ 'high',
                                     quantile_rank %in% c(8:9) ~ 'mid',
                                     TRUE ~ 'low'))) %>%
  select('zip', `2019 Population`, category, 'MetroShort')

group <- chars %>% filter(MetroShort %in% cities) %>%
  group_by(category) %>% summarise(pop = sum(`2019 Population`))

cbd <- sum(chars %>% filter(category=='cbd', MetroShort %in% cities) %>% select(`2019 Population`))

# divide by population
tab_flow = tab_flow %>% select(-names)
tab_flow <- tab_flow/cbd*100

stargazer(tab_flow, digits=2, type = "html", 
          out="./figures-tables/inflow_outflow_city_center_new_v3.doc", summary = FALSE)

