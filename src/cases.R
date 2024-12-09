rm(list=ls())

# TFC assess
# This script works with the TFC cases database (Lucas)

# libraries
library(tidyr)
library(readr)
library(dplyr)
library(stringr)
#library(data.table)
#library(tidyverse)

library(sf)
library(terra)
library(raster)

library(tidyterra)
library(ggplot2)
library(ggthemes)
library(corrplot)

# library(geodata) #gadm() and world()
# library(MazamaSpatialUtils) #convert iso2 into iso3
#library(rnaturalearth)
#library(rnaturalearthdata)

#library(FAOSTAT) # to download and manipulate data from FAO
#library(httr) # to download data (e.g. from Zenodo)


### Set dirs-----
dir_git <- 'C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_TCA_ch2_visions/'

### Load IPBES regions-----

# Download data from zenodo
# recordID <- "3923633"
# url_record <- paste0("https://zenodo.org/api/records/", recordID)
# record <- httr::GET(url_record)
# record # Status 200 indicates a successful download
# url_shape <- content(record)$files[[5]]$links$download # Contains the url to download the shapefile
# 
# httr::GET(url_shape, write_disk(paste0(dir_git,"/","ipbes_regions_subregions.zip"), overwrite = T)) # Downloads shapefile
# unzip(paste0(dir_git,"/","ipbes_regions_subregions.zip")) # unzips shapefile

# Russian and Fiji were nor wrap to the date line and created a number of geometry issues
# Fixed in QGIS: created single features, reduced the extent and merged again Russia and Fiji and appended to other countries
# still need to upload the new version

ipbes_regions_fixed <- sf::st_read(paste0(dir_git,"data/IPBES_regions_subregions/IPBES_Regions_Subregions2OK_simp.gpkg")) %>% 
  mutate(name = tolower(name)) %>% 
  dplyr::select(-layer)
ipbes_regions_fixed_df = ipbes_regions_fixed %>% 
  st_set_geometry(NULL)

ipbes_regions_countries_df <- read_csv(paste0(dir_git,"/data/IPBES_regions_subregions/IPBES_Regions_Subregions2.csv")) %>% 
  mutate(country = tolower(Area)) %>% 
  mutate(region = tolower(Region)) %>% 
  mutate(subregion = tolower(Sub_Region)) %>% 
  dplyr::select(-Area, -Region, -Sub_Region)

ipbes_countries <- sf::st_read(paste0(dir_git,"/data/IPBES_regions_subregions/gadm36_0_noCaspian_cea_simple_20200121.shp")) %>% 
  mutate(country = tolower(NAME_0)) %>% 
  left_join(ipbes_regions_countries_df, by = 'GID_0') %>% 
  dplyr::select("country"="country.y","GID_0","ISO_3","region","subregion")

#transform to robinson
robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
ipbes_regions_fixed_robin <- sf::st_transform(ipbes_regions_fixed, crs = robin) # changes the projection
ipbes_countries_robin <- sf::st_transform(ipbes_countries, crs = robin) # changes the projection

# transform to latlon
latlon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
ipbes_countries_latlon <- sf::st_transform(ipbes_countries, crs = latlon) # changes the projection

# plots
ggplot() + 
  geom_sf(data = filter(ipbes_regions_fixed, is.na(parent_id)), mapping = aes(fill = name)) + #regions
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
  ) +
  scale_fill_viridis_d(, name = 'IPBES Regions') +
  #coord_sf(crs = 4326) #latlong
  coord_sf(crs = robin)

# plots
ggplot() + 
  geom_sf(data = ipbes_countries_robin) + 
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
  ) +
  scale_fill_viridis_d(, name = 'Countries') +
  #coord_sf(crs = 4326) #latlong
  coord_sf(crs = robin)

### Load cases -----

## Location
#cases1 = read_csv(paste0(dir_git,'data/cases/TC_Casos_ordenados5.csv'))
cases2 = read_csv(paste0(dir_git,'data/cases/TC_Casos_ordenados5_locationOK.csv'))
cases_final = read_csv(paste0(dir_git,'data/cases/2_IPBES_TCA_DMR_Case studies database_(B).csv'))

names(cases_final)

# clean cases
cases_clean = dplyr::select(cases_final,
                        "name" = "1- Title of the example with transformative potential",
                        "continent"="5.a- Location - Continent", "country" = "5.b- Location - Country", "region_state" = "5.c- Location - Region/State within country",
                        "start_date" ="8.a- Period of initiation","ongoing"="8.b- Is it still ongoing?",
                        "end_date" = "8.c- If finished, in which year did it finish?","time_frame"="8.d- Time-frame for the desired changes: main changes occurred over...",
                        "sector" =  "9.a- Which economic sector is/was chiefly involved? Select more than one if applicable", "other_sector"= "9.b- If you chose 'Other', please provide further details.",
                        "habitat"="10- What type(s) of habitat(s) was/were involved/affected in the example? Select more than one option, if applicable.",
                        "scope" = "11- Scale of change attempted by the example.",
                        "people_pos_affected" = "12- How many people have been affected in this example so far? (Estimated) [Positively]",                                                                                                                                              
                        "people_neg_affected" = "12- How many people have been affected in this example so far? (Estimated) [Negatively]"
)
names(cases_clean)

# checks
#cases_clean %>% distinct(country) %>%   View()
cases_clean %>% filter(is.na(country)) 

#cases_clean %>% distinct(continent) %>%  View()
cases_clean %>% filter(is.na(continent)) 

ipbes_regions_countries_df %>% distinct(region)
ipbes_regions_countries_df %>% distinct(subregion)

# Harmonize location of cases to match ipbes format
cases_harm = cases_clean %>% 
  #fix misspellings
  mutate(country = tolower(country)) %>% 
  mutate(country = gsub('worldwide','global',country)) %>% 
  mutate(country = gsub('n[/]a','',country)) %>% 
  mutate(country = gsub('[;]',',',country)) %>% 
  mutate(country = gsub('hq in switzerland','switzerland',country)) %>% 
  mutate(country = gsub(' and ',', ',country)) %>% 
  mutate(country = gsub('[,][,]',',',country)) %>%
  mutate(country = gsub('all countries','global',country)) %>% 
  mutate(country = gsub('the movement started in sweden','sweden',country)) %>% 
  mutate(country = gsub('global [(]started in austria[,] now based in germany[)]','global',country)) %>% 
  mutate(country = gsub('started in madagascar','madagascar',country)) %>% 
  mutate(country = gsub('european countries','europe',country)) %>% 
  mutate(country = gsub('from global to local','global',country)) %>% 
  mutate(country = gsub('middle east: jordan israel, palestine','jordan, israel, palestine',country)) %>% 
  mutate(country = gsub('multiple[-] approximately 12 at the moment[.] ','',country)) %>% 
  mutate(country = gsub('countries within the congo basin','congo (the),congo (the democratic republic of the),central african republic (the),cameroon,equatorial guinea,gabon',country)) %>% 
  mutate(country = gsub('central africa','angola, burundi, chad, equatorial guinea, gabon, cameroon,central african republic (the), congo (the),congo (the democratic republic of the), rwanda,sao tome and principe',country)) %>% 
  
  
  mutate(country = str_trim(country)) %>% 
  # unnest comma separated items
  mutate(country = strsplit(as.character(country), ",")) %>% 
  unnest(country) %>% 
  
  # match IPBES countries
  mutate(country = str_trim(country)) %>% 
  mutate(country = gsub('the netherlands','netherlands (the)',country)) %>% 
  mutate(country = gsub('^taiwan$','taiwan (province of china)',country)) %>% 
  mutate(country = gsub('^united kingdom$','united kingdom of great britain and northern ireland (the)',country)) %>% 
  mutate(country = gsub('^uk$','united kingdom of great britain and northern ireland (the)',country)) %>% 
  mutate(country = gsub('^méxico$','mexico',country)) %>% 
  mutate(country = gsub('^tanzania$','tanzania, the united republic of',country)) %>% 
  mutate(country = gsub('^usa$','united states of america (the)',country)) %>% 
  mutate(country = gsub('kenia','kenya',country)) %>% 
  mutate(country = gsub('alemania','germany',country)) %>% 
  mutate(country = gsub('londres','united kingdom of great britain and northern ireland (the)',country)) %>% 
  mutate(country = gsub('bolivia','bolivia (plurinational state of)',country)) %>% 
  mutate(country = gsub('the gambia','gambia (the)',country)) %>% 
  mutate(country = gsub('vietnam','viet nam',country)) %>% 
  mutate(country = gsub('cape verde','cabo verde',country)) %>% 
  mutate(country = gsub('^niger$','niger (the)',country)) %>% 
  mutate(country = gsub('^sudan$','sudan (the)',country)) %>% 
  mutate(country = gsub('^united states of america$','united states of america (the)',country)) %>% 
  mutate(country = gsub('new[-]zealand','new zealand',country)) %>% 
  mutate(country = gsub('salomon island','solomon island',country)) %>% 
  mutate(country = gsub('scotland','united kingdom of great britain and northern ireland (the)',country)) %>% 
  mutate(country = gsub('the us','united states of america (the)',country)) %>% 
  mutate(country = gsub('^united states$','united states of america (the)',country)) %>% 
  mutate(country = gsub('curitiba','brazil',country)) %>% 
  mutate(country = gsub('cadiz','spain',country)) %>% 
  mutate(country = gsub('galicia','spain',country)) %>% 
  mutate(country = gsub('vancouver','canada',country)) %>% 
  mutate(country = gsub('california','united states of america (the)',country)) %>% 
  mutate(country = gsub('^bahamas$','bahamas (the)',country)) %>% 
  mutate(country = gsub('^england$','united kingdom of great britain and northern ireland (the)',country)) %>% 
  mutate(country = gsub('^congo$','congo (the)',country)) %>% 
  mutate(country = gsub('^the drc$','congo (the democratic republic of the)',country)) %>% 
  mutate(country = gsub('^palestine$','palestine, state of',country)) %>% 
  mutate(country = gsub('^moldova$','moldova (the republic of)',country)) %>%  
  mutate(country = gsub('^netherlands$','netherlands (the)',country)) %>% 
  mutate(country = gsub('^laos$',"lao people's democratic republic (the)",country)) %>% 
  mutate(country = gsub('^brunei$','brunei darussalam',country)) %>% 
  mutate(country = gsub('^timor$','timor-leste',country)) %>% 
  mutate(country = gsub('^philippines$','philippines (the)',country)) %>% 
  mutate(country = gsub('^antigua$','antigua and barbuda',country)) %>% 
  mutate(country = gsub('^barbuda$','antigua and barbuda',country)) %>% 
  mutate(country = gsub('^belice$','belize',country)) %>% 
  mutate(country = gsub('^dominican rep[.]','dominican republic (the)',country)) %>% 
  mutate(country = gsub('^granada$','grenada',country)) %>% 
  mutate(country = gsub('^saint kitts','saint kitts and nevis',country)) %>% 
  mutate(country = gsub('^nevis$','saint kitts and nevis',country)) %>% 
  mutate(country = gsub('santa lucía','saint lucia',country)) %>% 
  mutate(country = gsub('sant vicent','saint vincent and the grenadines',country)) %>% 
  mutate(country = gsub('^grenadines$','saint vincent and the grenadines',country)) %>% 
  mutate(country = gsub('^trinidad$','trinidad and tobago',country)) %>% 
  mutate(country = gsub('^tobago$','trinidad and tobago',country)) %>% 
  mutate(country = gsub('^gambia$','gambia (the)',country)) %>% 
  mutate(country = gsub('^letonia$','latvia',country)) %>% 
  mutate(country = gsub('^ivory coast$',"côte d'ivoire",country)) %>% 
  mutate(country = gsub('^sao tom[é] e principe$','sao tome and principe',country)) %>% 
  mutate(country = gsub('^cook islands$','cook islands (the)',country)) %>% 
  mutate(country = gsub('^venezuela$','venezuela (bolivarian republic of)',country)) %>% 
  mutate(country = gsub('^comoros$','comoros (the)',country)) %>% 
  mutate(country = gsub('guinnesa[-]bissau','guinea-bissau',country)) %>% 
  mutate(country = gsub('^russia$','russian federation (the)',country)) %>% 
  mutate(country = gsub('sao tome and principen republic [(]the[)]','sao tome and principe',country)) %>% 
  mutate(country = gsub('^phillippines$','philippines (the)',country)) %>% 
  mutate(country = gsub('^jordania$','jordan',country)) %>% 
  mutate(country = gsub('^all$','global',country)) %>% 
  mutate(country = gsub('"','',country)) %>% 
  mutate(country = gsub('austria\nbelgium\nbulgaria\ncroatia\ncyprus\nczechia\ndenmark\nestonia\nfinland\nfrance\ngermany\ngreece\nhungary\nireland\nitaly\nlatvia\nlithuania\nluxembourg\nmalta\nnetherlands\npoland\nportugal\nromania\nslovakia\nslovenia\nspain\nsweden','austria,belgium,bulgaria,croatia,cyprus,czechia,denmark,estonia,finland,france,germany,greece,hungary,ireland,italy,latvia,lithuania,luxembourg,malta,netherlands,poland,portugal,romania,slovakia,slovenia,spain,sweden',country)) %>% 
  mutate(country = gsub('austria\nbelgium\nbulgaria\ncroatia\ncyprus\nczechia\ndenmark\nestonia\nfinland\nfrance\ngermany\ngreece\nhungary\nireland\nitaly\nlatvia\nlithuania\nluxembourg\nmalta\nnetherlands\npoland\nportugal\nromania\nslovakia\nslovenia\nspain\nsweden\n','austria,belgium,bulgaria,croatia,cyprus,czechia,denmark,estonia,finland,france,germany,greece,hungary,ireland,italy,latvia,lithuania,luxembourg,malta,netherlands,poland,portugal,romania,slovakia,slovenia,spain,sweden',country)) %>% 
  mutate(country = gsub('^nigeria[.]$','jordan',country))

cases_harm[393,1:3]
cases_harm[305,1:3]

cases_fix = cases_harm %>% 
  filter(name %in% c('European Green Deal (EGD)', 'AKTEA' )) %>% 
  mutate(country = str_split(country, ",")) %>%
  unnest(country) %>% 
  rbind(filter(cases_harm, !name %in% c('European Green Deal (EGD)', 'AKTEA' ))) %>% 
  mutate(country = gsub('^sweden\n$','sweden',country)) %>% 
  mutate(country = gsub('^netherlands$','netherlands (the)',country)) %>% 
  distinct(name, country)

write_csv(cases_fix,paste0(dir_git,'output/cases/cases_harmonized.csv'))
cases_fix = read_csv(paste0(dir_git,'output/cases/cases_harmonized.csv'))

# identify cases with wrong country assignment  
country_errors = anti_join(cases_fix,ipbes_regions_countries_df, by = 'country') 
country_errors %>% distinct(name) %>% count() # 67
# mostly global cases and some regions e.g. europa, european union member states, africa, borneo, brittish overseas terretories

country_errors = country_errors  %>% 
  filter(country != 'global') 
#write_csv(country_errors, paste0(dir_git,'cases_need_country_fixes.csv'))

# calculate global cases and total cases 
global_cases = cases_fix %>% filter(country == 'global') %>% count() #60
total_cases = cases_fix %>% distinct(name) %>% count() #390

# checks 
cases_fix %>% distinct(country) %>%  count() #150
cases_fix %>% distinct(name, country) %>%  count() #648

# ### Match cases locations with ipbes countries----
# 
# # countries
# cases_harm_ipbes_countries = cases_fix %>% 
#   # match with ipbes regions
#   inner_join(ipbes_regions_countries_df, by = 'country') %>% 
#   # get country counts
#   distinct(name, country, .keep_all = TRUE) %>% 
#   group_by(country) %>% 
#   add_count() %>% 
#   ungroup() %>% 
#   distinct(country, n, .keep_all = TRUE) %>% 
#   dplyr::select(country, n_cases_country = n) %>% 
#   # add global cases
#   dplyr::mutate(n_cases_country = n_cases_country + global_cases$n) %>% 
#   # calculate proportion
#   dplyr::mutate(prop_cases_country = n_cases_country/total_cases$n) 
# 
# write_csv(cases_harm_ipbes_countries, paste0(dir_git, 'output/cases/cases_harm_ipbes_countries.csv'))  
# cases_harm_ipbes_countries = read_csv(paste0(dir_git, 'output/cases/cases_harm_ipbes_countries.csv'))  
# 
# cases_countries = ipbes_countries_robin %>% 
#   inner_join(cases_harm_ipbes_countries, by = 'country')
# 
# #absolute numbers
# ggplot() + 
#   geom_sf(data = cases_countries, mapping = aes(fill = n_cases_country)) + 
#   theme(
#     panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), # sets latitude and longitude lines 
#     panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
#   ) +
#   scale_fill_viridis_c(direction = -1, name = 'Number of cases per country') +
#   #coord_sf(crs = 4326) #latlong
#   coord_sf(crs = robin)
# 
# #Proportions
# ggplot() + 
#   geom_sf(data = cases_countries, mapping = aes(fill = prop_cases_country)) + 
#   theme(
#     panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
#     panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
#   ) +
#   scale_fill_viridis_c(, name = 'Proportion of cases\nper country') +
#   #coord_sf(crs = 4326) #latlong
#   coord_sf(crs = robin)
#   
### Match cases locations with ipbes subregions----
cases_harm_ipbes_subregions = cases_fix %>% 
  # match with ipbes regions
  inner_join(ipbes_regions_countries_df, by = 'country') %>% 
  # get subregions count
  distinct(name, subregion, .keep_all = TRUE) %>% 
  group_by(subregion) %>% 
  add_count() %>% 
  ungroup() %>% 
  distinct(subregion, n, .keep_all = TRUE) %>% 
  dplyr::select(subregion, n_cases_subregion = n) %>% 
  # add global cases
  dplyr::mutate(n_cases_subregion = n_cases_subregion + global_cases$n) %>% 
  # calculate proportion
  dplyr::mutate(prop_cases_subregion = n_cases_subregion/total_cases$n) %>% 
  # add antarctic
  rbind(data.frame(subregion = 'antarctica', n_cases_subregion = NA, prop_cases_subregion = 0))

cases_subregions = ipbes_regions_fixed_robin %>% 
  inner_join(cases_harm_ipbes_subregions, by = c('name'='subregion'))

figure = ggplot() + 
  geom_sf(data = cases_subregions, mapping = aes(fill = n_cases_subregion)) + 
  theme(
    #panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), # sets latitude and longitude lines
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color
  ) +
  scale_fill_viridis_c(direction = -1, na.value = 'white', name = 'Number of cases\nper IPBES subregion') +
  #coord_sf(crs = 4326) #latlong
  coord_sf(crs = robin)
ggsave(file=paste0(dir_git,"output/cases/n_cases_final.svg"), plot=figure, width=8, height=8, dpi = 300)
ggsave(file=paste0(dir_git,"output/cases/n_cases_final.eps"), plot=figure, width=8, height=8, dpi = 300)
ggsave(file=paste0(dir_git,"output/cases/n_cases_final.pdf"), plot=figure, width=8, height=8, dpi = 300)

# ggplot() + 
#   geom_sf(data = cases_subregions, mapping = aes(fill = prop_cases_subregion)) + 
#   theme(
#     panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
#     panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
#   ) +
#   scale_fill_viridis_c(, name = 'Proportion of cases\nper IPBES subregion') +
#   #coord_sf(crs = 4326) #latlong
#   coord_sf(crs = robin)

### Match cases locations with ipbes regions----
# cases_harm_ipbes_regions = cases_harm %>% 
#   # match with ipbes regions
#   inner_join(ipbes_regions_countries_df, by = 'country') %>% 
#   # get regions count
#   distinct(name, region, .keep_all = TRUE) %>% 
#   group_by(region) %>% 
#   add_count() %>% 
#   ungroup() %>% 
#   distinct(region, n, .keep_all = TRUE) %>% 
#   dplyr::rename(n_cases_region = n) %>% 
#   # add global cases
#   dplyr::mutate(n_cases_region = n_cases_region + global_cases$n) %>% 
#   # calculate proportion
#   dplyr::mutate(prop_cases_region = n_cases_region/total_cases$n)
# 
# cases_regions = ipbes_regions_fixed_robin %>% 
#   inner_join(cases_harm_ipbes_regions, by = c('name'='region'))
# 
# ggplot() + 
#   geom_sf(data = cases_regions, mapping = aes(fill = n_cases_region)) + 
#   theme(
#     panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), # sets latitude and longitude lines 
#     panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
#   ) +
#   scale_fill_viridis_c(, name = 'Number of cases\nper IPBES region') +
#   #coord_sf(crs = 4326) #latlong
#   coord_sf(crs = robin)
# 
# ggplot() + 
#   geom_sf(data = cases_regions, mapping = aes(fill = prop_cases_region)) + 
#   theme(
#     panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), # sets latitude and longitude lines 
#     panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
#   ) +
#   scale_fill_viridis_c(, name = 'Proportion of cases\nper IPBES region') +
#   #coord_sf(crs = 4326) #latlong
#   coord_sf(crs = robin)
# 
# ## Join cases by country with quantitative indicators-----
# indic_countries1 = read_csv(paste0(dir_git, 'output/country_values_nexus.csv'))
# indic_countries2 = read_csv(paste0(dir_git, 'output/country_values_hf_ghm.csv'))
# 
# names(indic_countries)
# indic_countries_cases = indic_countries1 %>% 
#   left_join(indic_countries2, by=c('country', 'GID_0','ISO_3','region', 'subregion', 'ID')) %>% 
#   left_join(cases_harm_ipbes_countries, by= 'country') %>% 
#   dplyr::select(-"GroundWater_RcghDeq50",-"GroundWater_RchgDeq50",-"Species_Richness_3taxa",
#                 -"food_prod_terr", -"food_prod_terr_lim",-"soil_biodiversity",
#                 -"ADM2_ADM1_ADM0_2010",-"TerrStat_hotspots_sum_quant0.1", -'ID') %>% 
#   rename('GroundWater'="GroundWater_GeomaticDeq50",'MarRich'="MSpecies_Richness_9taxa",
#          'ThreatMarRich'="TMSpecies_Richness_8taxa","TerrRich"="Species_Richness_4taxa",
#          "ThreatTerrRich"="TSpecies_Richness_4taxa",
#          "TempVelocity"="TempVelocity_terr_all","crop_prod"="Monfreda_prod_sum_allcrops",
#          "burden_malnut"="IHME_dbl_burden_malnut","lvstk_prod"="Herrero_lvstk_kg",
#          "fish_prod"="Watson_fish_ind_nind_0004","life-exp"="life_expectancy_SHDI_2000",
#          "nxsHotspot"="TerrStat_hotspots_bin_sum_quant0.1")
# names(indic_countries_cases)
# write_csv(indic_countries_cases,paste0(dir_git, 'output/indic_countries_cases2.csv'))
# names(indic_countries_cases)
# 
# # calculate correlation among landscape metrics
# matrix = indic_countries_cases %>% 
#   dplyr::select(-country,-"GID_0",-"ISO_3",-"region",-"subregion",-"Built1994",-"HFP1993",
#                 -"HFP1993_int",-"HFP2009_int",-"Lights1994",-"NavWater1994",-"Pasture1993",
#                 -"Popdensity1990",-"Popdensity2010",-"croplands1992",-"fish_prod",
#                 -"MarRich",-"ThreatMarRich",-"NavWater2009", -"Railways",-"Lights2009")
# names(matrix)
# # correlation for all variables
# corr_spearman = as.data.frame(round(cor(matrix, use = "complete.obs", method = "spearman"),
#                                     digits = 2 # rounded to 2 decimals
# ))
# plot.new(); dev.off()
# corrplot(cor(matrix, use = "complete.obs", method = "pearson"),
#          method = "number",
#          type = "upper" # show only upper side
# )
# 
# # scatterplots
# pairs(dplyr::select(matrix, "n_cases"="n_cases_country","prop_cases"="prop_cases_country",
#                     "TerrRich", "ThreatTerrRich","TempVelocity","crop_prod",
#                     "nxsHotspot","ghm","Built2009","HFP2009"))
# 
# 
# 
# 
# 
# 
# 
# 
