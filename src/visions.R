rm(list=ls())

# TFC assess
# Vision CH1

# librarues
library(stringr)
#library(data.table)
#library(foreign)
library(tidyverse)
library(dplyr)
#library(readxl)
library(readr)

library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs

library(sf)

library(FAOSTAT) # to download and manipulate data from FAO
library(httr) # to download data off of Zenodo

### Set dirs-----
dir_drive <- 'G:/.shortcut-targets-by-id/18yX-16J7W2Kyq4Mn3YbU_HTjslZyr4hE/IPBES Task Force Knowledge and Data/_DATA/_TSU Internal/_ Weekly reports/Files - Yanina/TfC/visions_CH2'
dir_git <- 'C:/Users/yanis/Documents/scripts/IPBES_TSU-DATA/TfC/visions_CH2/Data'

### Load IPBES regions-----

# Download data
# recordID <- "3923633"
# url_record <- paste0("https://zenodo.org/api/records/", recordID)
# record <- httr::GET(url_record)
# record # Status 200 indicates a successful download
# url_shape <- content(record)$files[[5]]$links$download # Contains the url to download the shapefile
# 
# httr::GET(url_shape, write_disk(paste0(dir_git,"/","ipbes_regions_subregions.zip"), overwrite = T)) # Downloads shapefile
# unzip(paste0(dir_git,"/","ipbes_regions_subregions.zip")) # unzips shapefile

# Load data
ipbes_regions <- sf::st_read(paste0(dir_git,"/IPBES_Regions_Subregions/IPBES_Regions_Subregions2.shp")) 
# Russian and Fiji were nor wrap to the date line and created a number of geometry issues
# Fixes: created single features, reduced the extent and merged again Russia and Fiji and appended to other countries
ipbes_regions_fixed <- terra::vect(paste0(dir_git,"/IPBES_Regions_Subregions/IPBES_Regions_Subregions2OK.shp")) 
ipbes_regions_fixed_sf <- sf::st_read(paste0(dir_git,"/IPBES_Regions_Subregions/IPBES_Regions_Subregions2OK.shp")) %>% 
  mutate(Region = tolower(Region)) %>% 
  mutate(Sub_Region = tolower(Sub_Region)) %>% 
  dplyr::select(-layer, -path)

rm(ipbes_regions)

# Create dataframe with ISO3, Region and Sub_Region
ipbes_regions_fixed_df = ipbes_regions_fixed_sf %>%  
  st_set_geometry(NULL)
rm(ipbes_regions_fixed_sf)
#ipbes_regions_fixed %>% distinct(Sub_Region)

# aggregate by regions
# regions <- ipbes_regions_fixed %>% # this dissolves the data by region
#   dplyr::group_by(Region) %>% 
#   summarize(n=n()) %>%
#   sf::st_cast() %>% 
#   dplyr::select(Region)
# write_sf(regions,paste0(dir_git,'/regions_fixed.shp'))
regions_fixed <- terra::vect(paste0(dir_git,"/IPBES_Regions_Subregions/IPBES_Regions2OK.shp")) 
regions_fixed_sf <- sf::st_read(paste0(dir_git,"/IPBES_Regions_Subregions/IPBES_Regions2OK.shp")) %>% 
  mutate(Region = tolower(Region)) %>% 
  mutate(Sub_Region = tolower(Sub_Region)) %>% 
  dplyr::select(-layer, -path)
#regions_fixed_sf %>% distinct(Regions)
# rm(regions)

# Create dataframe with ISO3, Region and Sub_Region
regions_fixed_df = regions_fixed_sf %>%  
  st_set_geometry(NULL)
rm(regions_fixed_sf)

# aggregate by sub-regions
# sub_regions <- ipbes_regions_fixed %>% # this dissolves the data by sub-region
#   dplyr::group_by(Sub_Region) %>% 
#   summarize(n=n()) %>%
#   sf::st_cast()%>% 
#   dplyr::select(Sub_Region)
# write_sf(sub_regions,paste0(dir_git,'/sub_regions_fixed.shp'))
sub_regions_fixed <- terra::vect(paste0(dir_git,"/IPBES_Regions_Subregions/IPBES_Subregions2OK.shp")) 
sub_regions_fixed_sf <- sf::st_read(paste0(dir_git,"/IPBES_Regions_Subregions/IPBES_Subregions2OK.shp")) %>% 
  mutate(Region = tolower(Region)) %>% 
  mutate(Sub_Region = tolower(Sub_Region)) %>% 
  dplyr::select(-layer, -path)
#sub_regions_fixed_sf %>% distinct(Sub_Region)
# rm(sub_regions)

# Create dataframe with ISO3, Region and Sub_Region
sub_regions_fixed_df = sub_regions_fixed_sf %>%  
  st_set_geometry(NULL)
rm(sub_regions_fixed_sf)

# Free used memory
gc()


### Load visions with location information-----
## Location----
visions_l = read_csv(paste0(dir_git,'/visions/Ch2_MasterTableExcel_20230621_Quan_analysis.xlsx - Sample.csv'), skip = 1)
names(visions)
visions_l = dplyr::select(visions_l, 
                        "ID", "name" = "Vision Name\nName of the vision, assign one if not present in the text", 
                        "Location"="Location\nProvide place/region name", "Country" = "Country\nor countries",
                        "ISO Country","ISO Region","ISO Sub Region",
                        "Most_Transformative" = "Most Transfformative") 
# Clean visions
visions = visions %>% 
  filter(`ISO Region` != 'USA') %>% 
  mutate(`ISO Region` = gsub('ASIAN AND THE PACIFIC', 'ASIA AND THE PACIFIC',`ISO Region`)) %>% 
  mutate(`ISO Region` = gsub('AMERICA$', 'AMERICAS',`ISO Region`)) %>% 
  mutate(`ISO Region` = gsub('EUROPEAN AND CENTRAL ASIA', 'EUROPE AND CENTRAL ASIA',`ISO Region`)) %>% 
  rename('ISO_3' =`ISO Country`,'Region' =`ISO Region`,'Sub_Region' =`ISO Sub Region` ) %>% 
  mutate(Region = tolower(Region)) %>% 
  mutate(Sub_Region = tolower(Sub_Region)) %>% 
  mutate(Sub_Region = gsub('east africa and adjacents islands','east africa and adjacent islands',Sub_Region)) %>% 
  mutate(Sub_Region = gsub('eastern africa and adjacent island','east africa and adjacent islands',Sub_Region)) %>% 
  mutate(Sub_Region = gsub('eastern africa and adjacent islands','east africa and adjacent islands',Sub_Region)) %>% 
  mutate(Sub_Region = gsub('east africa and adjacent islandss','east africa and adjacent islands',Sub_Region)) %>% 
  mutate(Sub_Region = gsub('southen africa','southern africa',Sub_Region)) %>% 
  mutate(Sub_Region = gsub('south-east africa','southern africa',Sub_Region)) %>% 
  mutate(Sub_Region = gsub('northafrica','north africa',Sub_Region)) %>% 
  mutate(Sub_Region = gsub('meso america','mesoamerica',Sub_Region)) %>% 
  mutate(Sub_Region = gsub('nort-east asia','north-east asia',Sub_Region)) %>% 
  mutate(Sub_Region = gsub('north east asia','north-east asia',Sub_Region)) %>% 
  mutate(Sub_Region = gsub('south east asia','south-east asia',Sub_Region))

# checks
visions %>% distinct(Region)
visions %>% distinct(Sub_Region) %>% count()
ipbes_regions_fixed_df %>%  distinct(Sub_Region) %>% count()

## Aggregate vision by region
T_visions_rich_region = visions %>% 
  filter(!is.na(Region)) %>% 
  distinct(ID, Region, .keep_all = TRUE) %>% 
  group_by(Region) %>% 
  summarize(n_visions=n()) 

MT_visions_rich_region = visions %>% 
  filter(!is.na(Region)) %>% 
  distinct(ID, Region, .keep_all = TRUE) %>%
  filter(Most_Transformative == TRUE) %>% 
  group_by(Region) %>% 
  summarize(n_visions_most_tranf=n())
visions_rich_region = inner_join(T_visions_rich_region,MT_visions_rich_region, by = "Region")
rm(T_visions_rich_region,MT_visions_rich_region)

## Aggregate vision by sub_region
T_visions_rich_sub_region = visions %>% 
  filter(!is.na(Sub_Region)) %>% 
  distinct(ID, Sub_Region) %>% 
  group_by(Sub_Region) %>% 
  summarize(n=n()) %>% 
  mutate(Sub_Region = tolower(Sub_Region))
MT_visions_rich_sub_region = visions %>% 
  filter(!is.na(Sub_Region)) %>% 
  distinct(ID, Sub_Region, .keep_all = TRUE) %>%
  filter(Most_Transformative == TRUE) %>% 
  group_by(Sub_Region) %>% 
  summarize(n_visions_most_tranf=n())
visions_rich_sub_region = full_join(T_visions_rich_sub_region,MT_visions_rich_sub_region, by = "Sub_Region")
rm(T_visions_rich_sub_region,MT_visions_rich_sub_region)

## Aggregate vision by country
T_visions_rich_country = visions %>% 
  filter(!is.na(ISO_3)) %>% 
  filter(!grepl(',',ISO_3)) %>% 
  distinct(ID, ISO_3) %>% 
  group_by(ISO_3) %>% 
  summarize(n=n())

MT_visions_rich_country = visions %>% 
  filter(!is.na(ISO_3)) %>% 
  distinct(ID, ISO_3, .keep_all = TRUE) %>%
  filter(Most_Transformative == TRUE) %>% 
  group_by(ISO_3) %>% 
  summarize(n_visions_most_tranf=n())
visions_rich_country = full_join(T_visions_rich_country,MT_visions_rich_country, by = "ISO_3")
rm(T_visions_rich_country,MT_visions_rich_country)


## Scope-----
visions = read_csv(paste0(dir_git,'/visions/Ch2_MasterTableExcel_20230621_Quan_analysis.xlsx - Sample (2).csv'), skip = 1)
names(visions)

# clean table
visions = visions %>% 
  # edit columns
  dplyr::select("ID", "name" = "Vision Name\nName of the vision, assign one if not present in the text", 
                "local_scope" = "Local scale (Y/N) \nThe intended scope of the vision is local",                                                                                                                                                                                                                                                                           
                "regional_scope" = "Regional scale (Y/N) \nThe intended scope of the vision is regional",                                                                                                                                                                                                                                                                    
                "national_scope" = "National scale (Y/N) \nThe intended scope of the vision is national",                                                                                                                                                                                                                                                                      
                "multinational_scope" = "Multinational scale (Y/N) \nThe intended scope of the vision is multinational",
                "global_scope"  ="Global scale (Y/N) \nThe intended scope of the vision is global",                                                                                                                                                                                                                                                                       
                "cross_scale_scope" = "Multi/cross-scale (Y/N) \nThe intended scope of the vision is multi- or cross-scale",
                "Location"="Location\nProvide place/region name", "Country" = "Country\nor countries",
                "ISO Country","ISO Region","ISO Sub Region",
                "transformative_level" = "Transformative level\nWhat is the level of transformative change targeted?",
                "Most_Transformative" = "Most Transformative")  %>% 
  # harmonize regions and subregions
  dplyr::mutate(`ISO Region` = gsub('USA', 'AMERICAS',`ISO Region`)) %>% 
  dplyr::mutate(`ISO Region` = gsub('ASIAN AND THE PACIFIC', 'ASIA AND THE PACIFIC',`ISO Region`)) %>% 
  dplyr::mutate(`ISO Region` = gsub('AMERICA$', 'AMERICAS',`ISO Region`)) %>% 
  dplyr::mutate(`ISO Region` = gsub('EUROPEAN AND CENTRAL ASIA', 'EUROPE AND CENTRAL ASIA',`ISO Region`)) %>% 
  dplyr::rename('ISO_3' =`ISO Country`,'Region' =`ISO Region`,'Sub_Region' =`ISO Sub Region` ) %>% 
  dplyr::mutate(Region = tolower(Region)) %>% 
  dplyr::mutate(Sub_Region = tolower(Sub_Region)) %>% 
  dplyr::mutate(Sub_Region = gsub('east africa and adjacents islands','east africa and adjacent islands',Sub_Region)) %>% 
  dplyr::mutate(Sub_Region = gsub('eastern africa and adjacent island','east africa and adjacent islands',Sub_Region)) %>% 
  dplyr::mutate(Sub_Region = gsub('eastern africa and adjacent islands','east africa and adjacent islands',Sub_Region)) %>% 
  dplyr::mutate(Sub_Region = gsub('east africa and adjacent islandss','east africa and adjacent islands',Sub_Region)) %>% 
  dplyr::mutate(Sub_Region = gsub('southen africa','southern africa',Sub_Region)) %>% 
  dplyr::mutate(Sub_Region = gsub('south-east africa','southern africa',Sub_Region)) %>% 
  dplyr::mutate(Sub_Region = gsub('northafrica','north africa',Sub_Region)) %>% 
  dplyr::mutate(Sub_Region = gsub('meso america','mesoamerica',Sub_Region)) %>% 
  dplyr::mutate(Sub_Region = gsub('nort-east asia','north-east asia',Sub_Region)) %>% 
  dplyr::mutate(Sub_Region = gsub('north east asia','north-east asia',Sub_Region)) %>% 
  dplyr::mutate(Sub_Region = gsub('south east asia','south-east asia',Sub_Region)) %>% 
  dplyr::mutate(Sub_Region = gsub('americas','north america',Sub_Region)) %>% 
  # Title case to match IPBES_regions
  dplyr::mutate(Sub_Region = str_to_title(Sub_Region)) %>% 
  dplyr::mutate(Region = str_to_title(Region)) %>% 
  dplyr::mutate(Sub_Region = gsub('And ','and ',Sub_Region, ignore.case = FALSE)) %>% 
  dplyr::mutate(Sub_Region = gsub('The ','the ',Sub_Region, ignore.case = FALSE)) %>% 
  dplyr::mutate(Sub_Region = gsub('and Adjacent Islands','and adjacent islands',Sub_Region, ignore.case = FALSE)) %>% 
  # clean NAs
  dplyr::filter(!is.na(Region)) %>% 
  dplyr::filter(!is.na(Sub_Region))

visions %>% distinct(Region)
visions %>% distinct(Sub_Region)


# calculate global visions 
global = visions %>% 
  # Global scope (col M == Yes, removing the ones already included in multinational scope)
  filter(global_scope == 'Yes' & multinational_scope == 'No') %>% 
  mutate(n_visions_global = n_distinct(ID)) %>%  #51
  # Most transformative (col BX == TRUE, selected in light blue)
  filter(Most_Transformative == TRUE) %>% 
  mutate(n_visions_MT_global=n_distinct(ID)) %>%  #1
  dplyr::select(n_visions_global,n_visions_MT_global) %>% 
  filter(row_number()==1)

## Aggregate visions by region------
visions_rich_region = visions %>% 
  # Multinational scope (col L == Yes)
  filter(multinational_scope == "Yes") %>% 
  group_by(Region) %>% 
  mutate(n_visions_region = n_distinct(ID)) %>% 
  ungroup() %>% 
  distinct(Region, n_visions_region)

visions_MT_rich_region = visions %>% 
  # Multinational scope (col L == Yes)
  filter(multinational_scope == "Yes") %>% 
  # Most transformative (col BX == TRUE, selected in light blue)
  filter(Most_Transformative == TRUE) %>% 
  group_by(Region) %>% 
  mutate(n_visions_MT_region = n_distinct(ID)) %>% 
  ungroup() %>%  
  distinct(Region, n_visions_MT_region)

# combine global, regional and most transformative vision
visions_region = visions_rich_region %>% 
  full_join(visions_MT_rich_region, by = "Region") %>% 
  replace(is.na(.), 0) %>% 
  # add global visions
  mutate(n_visions_global = global$n_visions_global) %>% 
  mutate(n_visions_MT_global = global$n_visions_MT_global) %>% 
  mutate(n_visions_region_g = n_visions_region + n_visions_global) %>% 
  mutate(n_visions_MT_region_g = n_visions_MT_region + n_visions_MT_global) %>% 
  # calculate ratios
  mutate(ratio_global = n_visions_global/n_visions_region_g) %>% 
  mutate(ratio_transf = n_visions_MT_region_g/n_visions_region_g) %>% 
  rbind(data.frame('Region' = 'antarctica', 'n_visions_region' = 0, 'n_visions_MT_region' = 0, 
                   'n_visions_region_g' = global$n_visions_global, 'n_visions_MT_region_g' = global$n_visions_MT_global,
                   'n_visions_global' = global$n_visions_global, 'n_visions_MT_global' = global$n_visions_MT_global,
                   'ratio_global' = global$n_visions_global/global$n_visions_global, 'ratio_transf' = global$n_visions_MT_global/global$n_visions_global)) %>% 
  mutate(Region = gsub('americas', 'Americas',Region)) %>% 
  mutate(Region = gsub('europe and central asia', 'Europe and Central Asia',Region)) %>% 
  mutate(Region = gsub('africa', 'Africa',Region)) %>% 
  mutate(Region = gsub('asia and the pacific', 'Asia and the Pacific',Region)) %>% 
  mutate(Region = gsub('antarctica', 'Antarctica',Region))

write_csv(visions_region, paste0(dir_git,'/visions_region.csv'))  

rm(T_visions_rich_region,MT_visions_rich_region)

## Aggregate visions by sub-region------
visions_rich_sub_region = visions %>% 
  # Multinational scope (col L == Yes)
  filter(multinational_scope == "Yes") %>% 
  group_by(Sub_Region) %>% 
  mutate(n_visions_sub_region = n_distinct(ID)) %>% 
  ungroup() %>% 
  distinct(Sub_Region, n_visions_sub_region)

visions_MT_rich_sub_region = visions %>% 
  # Multinational scope (col L == Yes)
  filter(multinational_scope == "Yes") %>% 
  # Most transformative (col BX == TRUE, selected in light blue)
  filter(Most_Transformative == TRUE) %>% 
  group_by(Sub_Region) %>% 
  mutate(n_visions_MT_sub_region = n_distinct(ID)) %>% 
  ungroup() %>%  
  distinct(Sub_Region, n_visions_MT_sub_region)

# combine global, regional and most transformative vision
visions_sub_region = visions_rich_sub_region %>% 
  full_join(visions_MT_rich_sub_region, by = "Sub_Region") %>% 
  replace(is.na(.), 0) %>% 
  # add global visions
  mutate(n_visions_global = global$n_visions_global) %>% 
  mutate(n_visions_MT_global = global$n_visions_MT_global) %>% 
  mutate(n_visions_sub_region_g = n_visions_sub_region + n_visions_global) %>% 
  mutate(n_visions_MT_sub_region_g = n_visions_MT_sub_region + n_visions_MT_global) %>% 
  # calculate ratios
  mutate(ratio_global = n_visions_global/n_visions_sub_region_g) %>% 
  mutate(ratio_transf = n_visions_MT_sub_region_g/n_visions_sub_region_g) %>% 
  rbind(data.frame('Sub_Region' = 'Antarctica', 'n_visions_sub_region' = 0, 'n_visions_MT_sub_region' = 0, 
                   'n_visions_sub_region_g' = global$n_visions_global, 'n_visions_MT_sub_region_g' = global$n_visions_MT_global,
                   'n_visions_global' = global$n_visions_global, 'n_visions_MT_global' = global$n_visions_MT_global,
                   'ratio_global' = global$n_visions_global/global$n_visions_global, 'ratio_transf' = global$n_visions_MT_global/global$n_visions_global)) 

write_csv(visions_sub_region, paste0(dir_git,'/visions_sub_region.csv')) 
# joins with spatial data is done directly in QGIS

### Load Population data-----

# download FAO data
# FAOSTAT::download_faostat_bulk("http://fenixservices.fao.org/faostat/static/bulkdownloads/Population_E_All_Data_(Normalized).zip", getwd())
# pop_raw <- FAOSTAT::read_faostat_bulk("Population_E_All_Data_(Normalized).zip") # load the population data using FAOSTAT's built in function 
# pop_2022 <- FAOSTAT::translateCountryCode(data = pop_2022, from = "FAO", to = "ISO3", "area_code") 

pop_fao = read_csv(paste0(dir_git, '/Population_FAO/FAOSTAT_Pop_by_Country.csv'))

# aggregate population by regions
pop_foa_regions_2018 <- ipbes_regions_fixed_df %>% 
  left_join(pop_fao, by = "Area") %>% 
  filter(!is.na(Domain)) %>%  # some fail!!
  filter(Year == 2018) %>% 
  dplyr::group_by(Region) %>% # Grouping by regions
  dplyr::mutate(region_total_pop_M = sum(Value)/1000) %>% # calculates total population (millions) per region
  dplyr::ungroup() %>% 
  dplyr::group_by(Sub_Region) %>% # Grouping by sub-region 
  dplyr::mutate(sub_region_total_pop_M = sum(Value)/1000) %>% # calculates total population (millions) per sub-region
  dplyr::ungroup() %>% 
  dplyr::select(ISO_3,Area,Region,Sub_Region,Year,Unit,Value, region_total_pop_M, sub_region_total_pop_M)


### Load indicators (Nexus ch2)-----
indic_dir = 'C:/Users/yanis/Documents/scripts/ipbes_nexus_chp2/Data/Nexus_Indicators/all_harmonized'
indic <- lapply(as.list
                (list.files(indic_dir,full.names = T)
                ), 
                terra::rast)

### Extract indic by regions-----

# function to extract mean value of indic per region
extract_r = function(r) {
  return(terra::extract(r, ref,          #change ref according to regions
                        method="simple", 
                        ID=TRUE,         #keep ID of regions
                        fun = mean)      #calculate mean value of pixels
  )
}

# biodiversity
B_011_soil_biodiversity = indic[[4]]
B_038_Species_Richness = indic[[5]]
B_038_Threatened_Species_Richness = indic[[6]]

region_B_038_Species_Richness_mean = terra::extract(B_038_Species_Richness, regions_fixed,  
                          method="simple", 
                          ID=TRUE,
                          fun = mean, na.rm=TRUE)
region_B_038_Threatened_Species_Richness_mean = terra::extract(B_038_Threatened_Species_Richness, regions_fixed,  
                          method="simple", 
                          ID=TRUE,
                          fun = mean, na.rm=TRUE)

sub_regions_B_038_Species_Richness_mean = terra::extract(B_038_Species_Richness, sub_regions_fixed,  
                                             method="simple", 
                                             ID=TRUE,
                                             fun = mean, na.rm=TRUE)
sub_regions_B_038_Threatened_Species_Richness_mean = terra::extract(B_038_Threatened_Species_Richness, sub_regions_fixed,  
                                                        method="simple", 
                                                        ID=TRUE,
                                                        fun = mean, na.rm=TRUE)
#climate
C_007_TempVelocity_terr = indic[[7]]
regions_C_007_TempVelocity_terr_mean = terra::extract(C_007_TempVelocity_terr, regions_fixed,  
                                             method="simple", 
                                             ID=TRUE,
                                             fun = mean, na.rm=TRUE)
sub_regions_C_007_TempVelocity_terr_mean = terra::extract(C_007_TempVelocity_terr, sub_regions_fixed,  
                                              method="simple", 
                                              ID=TRUE,
                                              fun = mean, na.rm=TRUE)

#Food
F_008_Monfreda_prod_sum_allcrops = indic[[8]]
regions_F_008_Monfreda_prod_sum_allcrops_mean = terra::extract(F_008_Monfreda_prod_sum_allcrops, regions_fixed,  
                                              method="simple", 
                                              ID=TRUE,
                                              fun = mean, na.rm=TRUE)
sub_regions_F_008_Monfreda_prod_sum_allcrops_mean = terra::extract(F_008_Monfreda_prod_sum_allcrops, sub_regions_fixed,  
                                                       method="simple", 
                                                       ID=TRUE,
                                                       fun = mean, na.rm=TRUE)
F_017_IHME_dbl_burden_malnut = indic[[9]]
regions_F_017_IHME_dbl_burden_malnut_mean = terra::extract(F_017_IHME_dbl_burden_malnut, regions_fixed,  
                                              method="simple", 
                                              ID=TRUE,
                                              fun = mean, na.rm=TRUE)
sub_regions_F_017_IHME_dbl_burden_malnut_mean = terra::extract(F_017_IHME_dbl_burden_malnut, sub_regions_fixed,  
                                                   method="simple", 
                                                   ID=TRUE,
                                                   fun = mean, na.rm=TRUE)
F_019_Herrero_lvstk_kg = indic[[10]]
F_0819_food_prod_terr = indic[[12]]
F_0819_food_prod_terr_lim =indic[[13]]

#health
H_026_life_expectancy = indic[[15]]
regions_H_026_life_expectancy_mean = terra::extract(H_026_life_expectancy, regions_fixed,  
                                                   method="simple", 
                                                   ID=TRUE,
                                                   fun = mean, na.rm=TRUE)
sub_regions_H_026_life_expectancy_mean = terra::extract(H_026_life_expectancy, sub_regions_fixed,  
                                            method="simple", 
                                            ID=TRUE,
                                            fun = mean, na.rm=TRUE)
H_016_DALYS =indic[[14]]
regions_H_016_DALYS_mean = terra::extract(H_016_DALYS, regions_fixed,  
                                                   method="simple", 
                                                   ID=TRUE,
                                                   fun = mean, na.rm=TRUE)
sub_regions_H_016_DALYS_mean = terra::extract(H_016_DALYS, sub_regions_fixed,  
                                  method="simple", 
                                  ID=TRUE,
                                  fun = mean, na.rm=TRUE)
#water
W_007_water_scarcity = indic[[17]]
regions_W_007_water_scarcity_mean = terra::extract(W_007_water_scarcity, regions_fixed,  
                                  method="simple", 
                                  ID=TRUE,
                                  fun = mean, na.rm=TRUE)
sub_regions_W_007_water_scarcity_mean = terra::extract(W_007_water_scarcity, sub_regions_fixed,  
                                           method="simple", 
                                           ID=TRUE,
                                           fun = mean, na.rm=TRUE)
W_034_GroundWater_GeomaticDeq50 = indic[[18]]
regions_W_034_GroundWater_GeomaticDeq50_mean = terra::extract(W_034_GroundWater_GeomaticDeq50, regions_fixed,  
                                           method="simple", 
                                           ID=TRUE,
                                           fun = mean, na.rm=TRUE)
sub_regions_W_034_GroundWater_GeomaticDeq50_mean = terra::extract(W_034_GroundWater_GeomaticDeq50, sub_regions_fixed,  
                                                      method="simple", 
                                                      ID=TRUE,
                                                      fun = mean, na.rm=TRUE)
W_034_GroundWater_RcghDeq50 = indic[[19]]
W_034_GroundWater_RchgDeq50 =indic[[20]]

# append all indic by region

indic_regions = region_B_038_Species_Richness_mean %>% 
  left_join(region_B_038_Threatened_Species_Richness_mean, by = 'ID') %>% 
  left_join(regions_C_007_TempVelocity_terr_mean, by = 'ID') %>% 
  left_join(regions_F_008_Monfreda_prod_sum_allcrops_mean, by = 'ID') %>% 
  left_join(regions_F_017_IHME_dbl_burden_malnut_mean, by = 'ID') %>% 
  left_join(regions_H_026_life_expectancy_mean, by = 'ID') %>% 
  left_join(regions_H_016_DALYS_mean, by = 'ID') %>% 
  left_join(regions_W_007_water_scarcity_mean, by = 'ID') %>% 
  left_join(regions_W_034_GroundWater_GeomaticDeq50_mean, by = 'ID')

names(indic_regions)= c('ID','B_038_Species_Richness_mean','B_038_Threatened_Species_Richness_mean',
                       'C_007_TempVelocity_terr_mean',
                       'F_008_Monfreda_prod_sum_allcrops_mean','F_017_IHME_dbl_burden_malnut_mean',
                       'H_026_life_expectancy_mean','H_016_DALYS_mean',
                       'W_007_water_scarcity_mean', 'W_034_GroundWater_GeomaticDeq50_mean')
#write_csv(indic_regions, 'C:/Users/yanis/Documents/scripts/IPBES_TSU-DATA/TfC/visions_CH2/outputs/indic_regions.csv')
indic_regions = read_csv('C:/Users/yanis/Documents/scripts/IPBES_TSU-DATA/TfC/visions_CH2/outputs/indic_regions.csv')

indic_sub_regions = sub_regions_B_038_Species_Richness_mean %>% 
  left_join(sub_regions_B_038_Threatened_Species_Richness_mean, by = 'ID') %>% 
  left_join(sub_regions_C_007_TempVelocity_terr_mean, by = 'ID') %>% 
  left_join(sub_regions_F_008_Monfreda_prod_sum_allcrops_mean, by = 'ID') %>% 
  left_join(sub_regions_F_017_IHME_dbl_burden_malnut_mean, by = 'ID') %>% 
  left_join(sub_regions_H_026_life_expectancy_mean, by = 'ID') %>% 
  left_join(sub_regions_H_016_DALYS_mean, by = 'ID') %>% 
  left_join(sub_regions_W_007_water_scarcity_mean, by = 'ID') %>% 
  left_join(sub_regions_W_034_GroundWater_GeomaticDeq50_mean, by = 'ID')

names(indic_sub_regions)= c('ID','B_038_Species_Richness_mean','B_038_Threatened_Species_Richness_mean',
                        'C_007_TempVelocity_terr_mean',
                        'F_008_Monfreda_prod_sum_allcrops_mean','F_017_IHME_dbl_burden_malnut_mean',
                        'H_026_life_expectancy_mean','H_016_DALYS_mean',
                        'W_007_water_scarcity_mean', 'W_034_GroundWater_GeomaticDeq50_mean')
write_csv(indic_sub_regions, 'C:/Users/yanis/Documents/scripts/IPBES_TSU-DATA/TfC/visions_CH2/outputs/indic_sub_regions.csv')
indic_sub_regions = read_csv('C:/Users/yanis/Documents/scripts/IPBES_TSU-DATA/TfC/visions_CH2/outputs/indic_sub_regions.csv')

regions_sub_regions_ID =  regions_fixed_df %>%
  mutate(ID = row_number()) %>% 
  mutate(Sub_Region = NA) %>% 
  dplyr::select(ID, Region, Sub_Region) %>% 
  rbind(sub_regions_fixed_df %>%
          mutate(ID = row_number()) %>% 
          mutate(Region = NA) %>% 
          dplyr::select(ID, Region, Sub_Region))

### Join visions, regions, pop, indicators-----

data_region_sf <- regions_fixed_sf %>% 
  dplyr::select(Region) %>% 
  # add IDs
  dplyr::left_join(regions_sub_regions_ID %>% filter(!is.na(Region)), by = 'Region') %>% 
  dplyr::select(-Sub_Region) %>% 
  # add visions
  dplyr::left_join(visions_rich_region, by = 'Region') %>% 
  # add pop
  dplyr::left_join(
    pop_foa_regions_2018 %>% distinct(Region, region_total_pop_M), by = 'Region') %>% 
  # add indic
  dplyr::left_join(indic_regions, by = 'ID') 
data_region_df = data_region_sf %>% st_set_geometry(NULL)
write_csv(data_region_df, 'C:/Users/yanis/Documents/scripts/IPBES_TSU-DATA/TfC/visions_CH2/outputs/data_region_df.csv')
write_sf(data_region_sf, 'C:/Users/yanis/Documents/scripts/IPBES_TSU-DATA/TfC/visions_CH2/outputs/data_region.shp')

 data_sub_region_df <- sub_regions_fixed_df %>% 
  dplyr::select(Sub_Region) %>% 
  # add IDs
  dplyr::left_join(regions_sub_regions_ID %>% filter(!is.na(Sub_Region)), by = 'Sub_Region') %>% 
  dplyr::select(-Region) %>% 
  # add visions
  dplyr::left_join(visions_rich_sub_region, by = 'Sub_Region') %>% 
  # add pop
  dplyr::left_join(
    pop_foa_regions_2018 %>% distinct(Sub_Region, sub_region_total_pop_M), by = 'Sub_Region') %>% 
  # add indic
  dplyr::left_join(indic_sub_regions, by = 'ID') 
data_sub_region_df = data_sub_region_sf %>% st_set_geometry(NULL)
write_csv(data_sub_region_df, 'C:/Users/yanis/Documents/scripts/IPBES_TSU-DATA/TfC/visions_CH2/outputs/data_sub_region_df.csv')
write_sf(data_sub_region_sf, 'C:/Users/yanis/Documents/scripts/IPBES_TSU-DATA/TfC/visions_CH2/outputs/data_sub_region.shp')

# data_country <- ipbes_regions_fixed_sf %>% 
#   # visions
#   dplyr::inner_join(visions_rich_country, by = 'ISO_3') %>% 
#   # pop
#   dplyr::left_join(pop_foa_regions_2018, by = 'ISO_3') %>% 
#   #indic
#   dplyr::left_join(indic_regions, by = 'ISO_3') 


