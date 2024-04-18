rm(list=ls())

# This script reads in multiple quantitative indicators and extracts them by countries

# libraries
library(tidyr)
library(readr)
library(dplyr)
library(stringr)
#library(data.table)
#library(tidyverse)

library(sf)
library(terra)
#library(raster)

library(tidyterra)
library(ggplot2)
library(ggthemes)

# library(geodata) #gadm() and world()
# library(MazamaSpatialUtils) #convert iso2 into iso3


#library(rnaturalearth) # to easily plor global maps
#library(rnaturalearthdata)

#library(FAOSTAT) # to download and manipulate data from FAO
#library(httr) # to download data (e.g. from Zenodo)


### Set dirs-----
dir_drive <- 'G:/.shortcut-targets-by-id/18yX-16J7W2Kyq4Mn3YbU_HTjslZyr4hE/IPBES Task Force Knowledge and Data/_DATA/_TSU Internal/_ Weekly reports/Files - Yanina/TfC/cases_CH2'
dir_git <- 'C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_TCA_ch2_visions/'


### Harmonization functions-----

# Latlong coordinate system
ref <- rast(nlyrs=1, 
            xmin=-180, xmax=180, 
            ymin=-90, ymax=90, 
            crs='GEOGCRS["WGS 84",
    ENSEMBLE["World Geodetic System 1984 ensemble",
        MEMBER["World Geodetic System 1984 (Transit)"],
        MEMBER["World Geodetic System 1984 (G730)"],
        MEMBER["World Geodetic System 1984 (G873)"],
        MEMBER["World Geodetic System 1984 (G1150)"],
        MEMBER["World Geodetic System 1984 (G1674)"],
        MEMBER["World Geodetic System 1984 (G1762)"],
        MEMBER["World Geodetic System 1984 (G2139)"],
        ELLIPSOID["WGS 84",6378137,298.257223563,
            LENGTHUNIT["metre",1]],
        ENSEMBLEACCURACY[2.0]],
    PRIMEM["Greenwich",0,
        ANGLEUNIT["degree",0.0174532925199433]],
    CS[ellipsoidal,2],
        AXIS["geodetic latitude (Lat)",north,
            ORDER[1],
            ANGLEUNIT["degree",0.0174532925199433]],
        AXIS["geodetic longitude (Lon)",east,
            ORDER[2],
            ANGLEUNIT["degree",0.0174532925199433]],
    USAGE[
        SCOPE["Horizontal component of 3D system."],
        AREA["World."],
        BBOX[-90,-180,90,180]],
    ID["EPSG",4326]]', 
            resolution = 0.08333333)
ref <- setValues(ref, 1:ncell(ref))

# target raster properties
target_res <- terra::res(ref)
target_ext <- terra::ext(ref)
target_crs <- terra::crs(ref)

# Function to reproject rasters data using a references layer: 
project_r = function(r){
  return(terra::project(r, ref,
                        method='near',
                        align=FALSE,
                        threads=TRUE)
  )
}


# function to extract mean value of rasters per region
extract_r = function(r, region) {
  return(terra::extract(r, region,       #change according to regions
                        method="simple", 
                        ID=TRUE,         #keep ID of regions
                        fun = mean)      #calculate mean value of pixels
  )
}


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

### Load IPBES countries-----
ipbes_countries <- sf::st_read(paste0(dir_git,"/data/IPBES_regions_subregions/gadm36_0_noCaspian_cea_simple_20200121.shp")) %>% 
  mutate(country = tolower(NAME_0)) %>% 
  left_join(ipbes_regions_countries_df, by = 'GID_0') %>% 
  dplyr::select("country"="country.y","GID_0","ISO_3","region","subregion")

ipbes_regions_countries_df <- read_csv(paste0(dir_git,"/data/IPBES_regions_subregions/IPBES_Regions_Subregions2.csv")) %>% 
  mutate(country = tolower(Area)) %>% 
  mutate(region = tolower(Region)) %>% 
  mutate(subregion = tolower(Sub_Region)) %>% 
  dplyr::select(-Area, -Region, -Sub_Region)

#transform to robinson for ploting
robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#ipbes_regions_fixed_robin <- sf::st_transform(ipbes_regions_fixed, crs = robin) # changes the projection
ipbes_countries_robin <- sf::st_transform(ipbes_countries, crs = robin) # changes the projection

# transform to latlon to harmonize
latlon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
ipbes_countries_latlon <- sf::st_transform(ipbes_countries, crs = latlon) # changes the projection

# Plot 1
ggplot() + 
  geom_sf(data = filter(ipbes_regions_fixed, is.na(parent_id)), mapping = aes(fill = name)) + #regions
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
  ) +
  scale_fill_viridis_d(, name = 'IPBES Regions') +
  #coord_sf(crs = 4326) #latlong
  coord_sf(crs = robin)

# Plots
ggplot() + 
  geom_sf(data = ipbes_countries_robin) + 
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
  ) +
  scale_fill_viridis_d(, name = 'Countries') +
  #coord_sf(crs = 4326) #latlong
  coord_sf(crs = robin)

# ipbes_countries IDs
ipbes_countries_id = ipbes_countries_latlon %>% mutate(ID = row_number()) %>% st_drop_geometry()


### Quantitative indicators-----

## Nexus indicators------

# Load data
hf_indic_dir = 'C:/Users/yanis/Documents/IPBES/hf_indicators/all_harmonized'
hf_indic <- lapply(as.list
                      (list.files(hf_indic_dir,full.names = T)
                      ), 
                      terra::rast)
hf_indic_layer = c('Species_Richness_3taxa','MSpecies_Richness_9taxa',
                      'TMSpecies_Richness_8taxa','soil_biodiversity',
                      'Species_Richness_4taxa','TSpecies_Richness_4taxa',
                      'TempVelocity_terr_all','Monfreda_prod_sum_allcrops',
                      'IHME_dbl_burden_malnut','Herrero_lvstk_kg',
                      'Watson_fish_ind_nind_0004','food_prod_terr',
                      'food_prod_terr_lim','DALYS_2000','life_expectancy_SHDI_2000',
                      'ADM2_ADM1_ADM0_2010','ws_avg','GroundWater_GeomaticDeq50',
                      'GroundWater_RcghDeq50','GroundWater_RchgDeq50') 
# Extract data
for (i in 1:length(hf_indic)){
  print(i)
  hf_indic_layer[i]
  # reproject if neccesary
  if (crs(hf_indic[[i]]) != target_crs){
    print('reprojecting')
    projected = project_r(hf_indic[[i]])
    # extract mean indicator value
    print('extracting')
    values = extract_r(projected, ipbes_countries_latlon,  
                       method="simple", 
                       ID=TRUE,
                       fun = mean, na.rm=TRUE)
    names(values) = c('ID', hf_indic_layer[i])
    ipbes_countries_id = full_join(ipbes_countries_id,values, by = 'ID')
  }else{
    print('No need to reproject, extracting...')
    # extract mean indicator value
    values = terra::extract(hf_indic[[i]], ipbes_countries_latlon,  
                            method="simple", 
                            ID=TRUE,
                            fun = mean, na.rm=TRUE)
    names(values) = c('ID', hf_indic_layer[i])
    ipbes_countries_id = full_join(ipbes_countries_id,values, by = 'ID')
  }
}


## Nexus hospots-------
# Load data
nexus_hot_dir = 'C:/Users/yanis/Documents/IPBES/hf_indicators/_hotspots_agg/raster/'
nexus_hot <- lapply(as.list
                    (list.files(nexus_hot_dir,full.names = T, pattern = '^TerrStat_hotspots*')
                    ), 
                    terra::rast)
# [[2]]TerrStat_hotspots_bin_sum_quant0.1.tif 
# [[8]]TerrStat_hotspots_sum_quant0.1.tif 
nexus_hot = nexus_hot[c(2,8)]
nexus_hot_layer = c('TerrStat_hotspots_bin_sum_quant0.1','TerrStat_hotspots_sum_quant0.1')

# Extract data

# Extract all values together
for (i in 1:length(nexus_hot)){
  print(i)
  nexus_hot_layer[i]
  # reproject if neccesary
  if (crs(nexus_hot[[i]]) != target_crs){
    print('reprojecting')
    projected = project_r(nexus_hot[[i]])
    # extract mean indicator value
    print('extracting')
    # extract mean indicator value
    values = extract_r(projected, ipbes_countries_latlon,  
                       method="simple", 
                       ID=TRUE,
                       fun = max, na.rm=TRUE)
    names(values) = c('ID', nexus_hot_layer[i])
    ipbes_countries_id = full_join(ipbes_countries_id,values, by = 'ID')
  }else{
    print('No need to reproject, extracting...')
    # extract mean indicator value
    values = terra::extract(nexus_hot[[i]], ipbes_countries_latlon,  
                            method="simple", 
                            ID=TRUE,
                            fun = max, na.rm=TRUE)
    names(values) = c('ID', nexus_hot_layer[i])
    ipbes_countries_id = full_join(ipbes_countries_id,values, by = 'ID')
  }
}



## Anthropogenic threat complex ATC-----
atc_indic_dir = 'C:/Users/yanis/Documents/IPBES/human_modification_indic/anthropogenic_threat_complex_ATC/Bowler_ATCs_Figs_SOM/'
atc <- terra::rast(paste0(atc_indic_dir,'Figure_4.tif'))
#plot(atc)
stack <- terra::rast(paste0(atc_indic_dir,'Figure_6.grd'))
# names       : Clima~hange, Human_use, Human~ation, Pollution, Alien~ntial, Cumulative 
# min values  :           0,         0,           0,         0,           0,          0 
# max values  :           4,         5,           1,         4,           1,         11 

# harm projection
atc = project_r(atc)
cc = project_r(stack[[1]])
hu = project_r(stack[[2]])
hp = project_r(stack[[3]])
po = project_r(stack[[4]])
ap = project_r(stack[[5]])
cum = project_r(stack[[6]])
rm(stack)

atc_all = list(atc,cc,hu,hp,po,ap,cum)
atc_layers = c('atc', 'climate_change', 'human_use', 'Human_population', 'pollution', 'alien_potential', 'cumulative')

# Extract data
values_atc = terra::extract(atc, ipbes_countries_latlon,
                     method="simple", 
                     ID=TRUE,         #keep ID of regions
                     fun = table,
                     cells = TRUE)


for (i in 1:length(atc_all)){ #did not work for input layers
  print(i)
  print(atc_layers[i])
  # reproject if neccesary
  if (crs(atc_all[[i]]) != target_crs){
    print('reprojecting')
    projected = project_r(atc_all[[i]])
    # extract mean indicator value
    print('extracting')
    values = extract_r(projected, ipbes_countries_latlon)
    names(values) = c('ID', atc_layers[i])
    ipbes_countries_id = full_join(ipbes_countries_id,values, by = 'ID')
  }else{
    print('No need to reproject, extracting...')
    # extract mean indicator value
    values = terra::extract(atc_all[[i]], ipbes_countries_latlon)
    names(values) = c('ID', atc_layers[i])
    ipbes_countries_id = full_join(ipbes_countries_id,values, by = 'ID')
  }
}


## Global Human Modification-----
ghm = rast('C:/Users/yanis/Documents/IPBES/human_modification_indic/gHM/gHM/gHM.tif')

# harm projection
projected_ghm = project_r(ghm)

#extract values by country
values_ghm = terra::extract(projected_ghm, ipbes_countries_latlon,
                             fun=mean, method="simple",
                             ID=TRUE,na.rm=TRUE)
names(values_ghm) = c('ID', 'ghm')
ipbes_countries_id = full_join(ipbes_countries_id,values_ghm, by = 'ID')

## Human footprint------

# Load data
hf_indic_dir = 'C:/Users/yanis/Documents/IPBES/human_modification_indic/human_footprint/Dryadv3/Maps/'
hf_indic <- lapply(as.list
                      (list.files(hf_indic_dir,full.names = T, pattern = '.tif$')
                      ), 
                      terra::rast)
hf_indic_layer = c('Built1994','Built2009','HFP1993','HFP1993_int','HFP2009','HFP2009_int','Lights1994','Lights2009',
                   'NavWater1994','NavWater2009','Pasture1993','Pasture2009','Popdensity1990','Popdensity2010',
                   'Railways','Roads','croplands1992','croplands2005') 
# Extract data
for (i in 1:length(hf_indic)){
  print(i)
  print(hf_indic_layer[i])
  # reproject if neccesary
  if (crs(hf_indic[[i]]) != target_crs){
    print('reprojecting')
    projected = project_r(hf_indic[[i]])
    # extract mean indicator value
    print('extracting')
    values = terra::extract(projected, ipbes_countries_latlon,
                            fun=mean, method="simple",
                            ID=TRUE,na.rm=TRUE)
    names(values) = c('ID', hf_indic_layer[i])
    ipbes_countries_id = full_join(ipbes_countries_id,values, by = 'ID')
  }else{
    print('No need to reproject, extracting...')
    # extract mean indicator value
    values = terra::extract(hf_indic[[i]], ipbes_countries_latlon,
                            fun=mean, method="simple",
                            ID=TRUE,na.rm=TRUE)
    names(values) = c('ID', hf_indic_layer[i])
    ipbes_countries_id = full_join(ipbes_countries_id,values, by = 'ID')
  }
}


### Save extract indic----
write_csv(ipbes_countries_id, paste0(dir_git, 'output/country_values.csv'))

#ipbes_countries_id = read_csv(paste0(dir_git, 'output/country_values.csv'))
#ipbes_countries_id2 = full_join(ipbes_countries_id1,ipbes_countries_id, by = c('country','GID_0','ISO_3','region','subregion','ID'))
                               