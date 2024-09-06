rm(list=ls())

# TFC assess
# This script works with the TFC cases database (Lucas)

# libraries

library(tidyr)
library(readr)
library(dplyr)
library(stringr)

library(sf)
library(terra)
library(raster)

library(tidyterra)
library(ggplot2)
#library(ggthemes)
library(scales)
library(svglite)

library(rnaturalearth)
library(rnaturalearthdata)

### Set dirs-----
dir_git <- 'C:/Users/JLU-SU/Documents/GitHub/IPBES-Data/IPBES_TCA_ch2_visions/'
dir_drive <- 'G:/My Drive/regions/IPBES_regions_subregions/'
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

ipbes_subregions_fixed <- sf::st_read(paste0(dir_drive,"IPBES_Subregions2OK.shp")) %>% 
  rename(region = Region, subregion = Sub_Region)

ipbes_regions_countries_df <- read_csv(paste0(dir_drive,"IPBES_Regions_Subregions2.csv")) %>% 
  mutate(country = tolower(Area)) %>% 
  mutate(region = tolower(Region)) %>% 
  mutate(subregion = tolower(Sub_Region)) %>% 
  dplyr::select(-Area, -Region, -Sub_Region)

#transform to robinson
robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
ipbes_subregions_fixed_robin <- sf::st_transform(ipbes_subregions_fixed, crs = robin) # changes the projection

# transform to latlon
#latlon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#ipbes_countries_latlon <- sf::st_transform(ipbes_countries, crs = latlon) # changes the projection

# plot 
ggplot() + 
  geom_sf(data = ipbes_subregions_fixed_robin, mapping = aes(fill = subregion)) + #subregions
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
  ) +
  scale_fill_viridis_d(, name = 'IPBES Subegions') +
  #coord_sf(crs = 4326) #latlong
  coord_sf(crs = robin)

### Load cases -----

## Location
cases1 = read_csv(paste0(dir_git,'data/cases/TC_Casos_ordenados5.csv'))
cases2 = read_csv(paste0(dir_git,'data/cases/TC_Casos_ordenados5_locationOK.csv'))
cases3 = read_csv(paste0(dir_git,'data/cases/TC - Casos ordenados 02(2)-Ordenadas.csv'))

names(cases3)

# clean cases
cases_clean = dplyr::select(cases3,
                        "name" = "1- Title of the example with transformative potential",
                        "continent"="5.a- Location - Continent",
                        "country" = "5.b- Location - Country"
)



# checks
#cases_clean %>% distinct(country) %>%   View()
cases_clean %>% filter(is.na(country)) 

#cases_clean %>% distinct(continent) %>%  View()
cases_clean %>% filter(is.na(continent)) 

ipbes_regions_countries_df %>% distinct(region)
ipbes_regions_countries_df %>% distinct(subregion)

# Harmonize location of cases to match ipbes format
cases_harm = cases_clean %>% 
  mutate(country_old = country) %>% 
  #fix misspellings
  mutate(country = tolower(country)) %>% 
  mutate(country = gsub('worldwide','global',country)) %>% 
  mutate(country = gsub('n[/]a','',country)) %>% 
  mutate(country = gsub('[;]',',',country)) %>% 
  mutate(country = gsub('hq in switzerland','switzerland',country)) %>% 
  mutate(country = gsub(' and ',', ',country)) %>% 
  mutate(country = gsub('[,][,]',',',country)) %>%
  mutate(country = gsub('all countries','global',country)) %>% 
  mutate(country = gsub('all','global',country)) %>% 
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
  mutate(country = gsub('nigeria[.]','nigeria',country)) %>% 
  filter(country != 'borneo')


# identify cases with wrong country assignment  
country_errors = anti_join(cases_harm,ipbes_regions_countries_df) 
country_errors %>% distinct(name) %>% count() # 65
# mostly global cases...some regions are still left e.g.
country_errors = country_errors  %>% 
  filter(country != 'global') 
### I added these cases to the count of cases per subregion

# save harmonized layer and issues
write_csv(country_errors, paste0(dir_git,'data/cases/cases_need_country_fixes.csv'))
write_csv(cases_harm,paste0(dir_git,'data/cases/cases_harmonized.csv'))

# calculate global cases and total cases 
global_cases = cases_harm %>% filter(country == 'global') %>% count() #62

# calculate errors
errors_africa = filter(country_errors, continent == 'Africa') %>% count()
errors_europa = filter(country_errors, country == 'europe') %>% count()
errors_eu = filter(country_errors, country == 'european union member states') %>% count()

### Match cases locations with ipbes regions, subregions via countries----

# countries
cases_harm_ipbes_countries = cases_harm %>% 
  # match with ipbes regions
  inner_join(ipbes_regions_countries_df, by = 'country') %>% 
  dplyr::select(-'country_old', -"continent",-"ISO_3") %>% 
  # get country counts
  distinct(name, country, .keep_all = TRUE) %>% 
  group_by(country) %>% 
  add_count() %>% 
  ungroup() %>% 
  distinct(country, n, .keep_all = TRUE) %>% 
  dplyr::select(country, n_cases_country = n) %>% 
  # add global cases
  dplyr::mutate(n_cases_country = n_cases_country + global_cases$n)

#write_csv(cases_harm_ipbes_countries, paste0(dir_git, 'output/cases/cases_harm_ipbes_countries.csv'))  
cases_harm_ipbes_countries = read_csv(paste0(dir_git, 'output/cases/cases_harm_ipbes_countries.csv'))  

cases_countries = ipbes_countries_robin %>% 
  inner_join(cases_harm_ipbes_countries, by = 'country')

#absolute numbers
ggplot() + 
  geom_sf(data = cases_countries, mapping = aes(fill = n_cases_country)) + 
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
  ) +
  scale_fill_viridis_c(direction = -1, name = 'Number of cases per country') +
  #coord_sf(crs = 4326) #latlong
  coord_sf(crs = robin)

  
### Match cases locations with ipbes subregions----
cases_harm_ipbes_subregions = cases_harm %>% 
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
  # add errors
  dplyr::mutate(n_cases_subregion = if_else(subregion %in% c('east africa and adjacent islands','central africa','north africa','southern africa','west africa'),
                                            true = n_cases_subregion + errors_africa$n,
                                            false = n_cases_subregion)) %>% 
  dplyr::mutate(n_cases_subregion = if_else(subregion %in% c('central and western europe','eastern europe'),
                                            true = n_cases_subregion + errors_europa$n,
                                            false = n_cases_subregion)) %>% 
  dplyr::mutate(n_cases_subregion = if_else(subregion %in% c('central and western europe'),
                                            true = n_cases_subregion + errors_eu$n,
                                            false = n_cases_subregion))
                  

cases_subregions = ipbes_subregions_fixed_robin %>% 
  mutate(subregion = tolower(subregion)) %>% 
  inner_join(cases_harm_ipbes_subregions, by = 'subregion')

# get global data
world <- ne_countries(scale = "medium", returnclass = "sf")

plot = ggplot() + 
  # world
  geom_sf(data = filter(world, admin == 'Antarctica'), fill = "#BFBFBF", color = "#F2F2F2") +
  # cases in subregions
  geom_sf(data = cases_subregions, mapping = aes(fill = n_cases_subregion), color = "#F2F2F2") + 
  theme(
    #panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "white") # sets background panel color 
  ) +
  scale_fill_viridis_c(, name = 'Number of cases\nper IPBES subregion', direction = -1) +
  coord_sf(crs = robin)

ggsave(file=paste0(dir_git,"output/cases/cases_subregions.png"), plot=plot, width=10, height=8, dpi = 300)









