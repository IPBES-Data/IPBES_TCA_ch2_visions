rm(list=ls())

# TFC assess
# TfC cases from the Corpus (CH2)

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
library(patchwork) # To display 2 charts together
library(hrbrthemes)

library(sf)
library(tidyterra)

#library(FAOSTAT) # to download and manipulate data from FAO
#library(httr) # to download data off of Zenodo
# library(geodata) #gadm() and world()
# library(MazamaSpatialUtils) #convert iso2 into iso3
#library(rnaturalearth)
#library(rnaturalearthdata)

### Set dirs-----
dir_drive <- 'G:/.shortcut-targets-by-id/18yX-16J7W2Kyq4Mn3YbU_HTjslZyr4hE/IPBES Task Force Knowledge and Data/_DATA/_TSU Internal/_ Weekly reports/Files - Yanina/TfC/visions_CH2'
dir_git <- 'C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_TCA_ch2_visions/'

# Load ocean mask
# mask_ocean = terra::rast('C:/Users/yanis/Documents/IPBES/nexus_indicators/all_harmonized/F_008_Monfreda_prod_sum_allcrops_harm.tif')
# m <- c(0.0, 0.0, 1, 0.0, 827665.2, 1) #min max values
# rclmat <- matrix(m, ncol=3, byrow=TRUE)
# mask_ocean <- terra::classify(mask_ocean, rclmat)
# reclass_matrix <- rbind(c(0, 1))
# mask_ocean <- terra::classify(mask_ocean, reclass_matrix)
# terra::plot(mask_ocean)
# terra::writeRaster(mask_ocean, 'C:/Users/yanis/Documents/regions/ocean_mask.tif')

mask_ocean = terra::rast('C:/Users/yanis/Documents/regions/ocean_mask.tif')

### Load anthromes change data-----
ant_change_dir = 'C:/Users/yanis/Documents/IPBES/human_modification_indic/anthrome_change/fraccover_class_100/fraccover_class_100/'

#### Class 190: Urban----

urban_change <- lapply(as.list
                   (list.files(paste0(ant_change_dir, 'fraccover_190_100/'),full.names = T, pattern = '.tif$')
                   ), 
                   terra::rast)
change_layer = as.character(seq(from = 1992, to = 2018, by = 1))

#terra::plot(urban_change[[1]])

# resample mask to match modification layer
mask_ocean_p = terra::project(mask_ocean, urban_change[[1]],
                               method='near',
                               align=TRUE,
                               threads=TRUE)

# calculate global mean
LCchange1 = data.frame()       
for (i in 1:length(change_layer)){
  print(i)
  print(urban_change_layer[i])
  # mask oceans
  masked = terra::mask(urban_change[[i]], mask_ocean_p)
  # get global
  mean_value = data.frame(x = urban_change_layer[i], 
                          y = terra::global(masked, "mean", na.rm=TRUE)[,1])
  names(mean_value) = c('year', 'urban_mean')
  LCchange1 = rbind(LCchange1,mean_value)
}
terra::plot(urban_change[[27]])

#### Class 10-20-30-40: Agriculture------

cl10_change <- lapply(as.list
                       (list.files(paste0(ant_change_dir, 'fraccover_10_100/'),full.names = T, pattern = '.tif$')
                       ), 
                       terra::rast)
cl20_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir, 'fraccover_20_100/'),full.names = T, pattern = '.tif$')
                      ), 
                      terra::rast)
cl30_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir, 'fraccover_30_100/'),full.names = T, pattern = '.tif$')
                      ), 
                      terra::rast)
cl40_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir, 'fraccover_40_100/'),full.names = T, pattern = '.tif$')
                      ), 
                      terra::rast)
#change_layer = as.character(seq(from = 1992, to = 2020, by = 1))

# add percentages of different agricultural classes
agri = list()
for (j in 1:length(change_layer)){
  print(j)
  agri[j] = as.list(sum(cl10_change[[j]],cl20_change[[j]],
                        cl30_change[[j]],cl40_change[[j]]))
}

# calculate global mean
LCchange2 = data.frame()       
for (i in 1:length(agri)){
  print(i)
  print(change_layer[i])
  # mask oceans
  masked = terra::mask(agri[[i]], mask_ocean_p)
  # get gloabl mean
  mean_value = data.frame(x = change_layer[i], 
                          y = terra::global(masked, "mean", na.rm=TRUE)[,1])
  names(mean_value) = c('year', 'agri_mean')
  LCchange2 = rbind(LCchange2,mean_value)
}

# add percentages of different agricultural classes AND urban
anthropo = list()
for (j in 1:length(change_layer)){
  print(j)
  anthropo[j] = as.list(sum(urban_change[[j]], cl10_change[[j]],
                            cl20_change[[j]],cl30_change[[j]],cl40_change[[j]]))
}

# calculate global mean
LCchange3 = data.frame()       
for (i in 1:length(anthropo)){
  print(i)
  print(change_layer[i])
  # mask oceans
  masked = terra::mask(anthropo[[i]], mask_ocean_p)
  # get gloabl mean
  mean_value = data.frame(x = change_layer[i], 
                          y = terra::global(masked, "mean", na.rm=TRUE)[,1])
  names(mean_value) = c('year', 'anthropo_mean')
  LCchange3 = rbind(LCchange3,mean_value)
}


#### Class 50 - 100, 160 & 170: Forest------

cl50_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir, 'fraccover_50_100/'),full.names = T, pattern = '.tif$')
                      ), 
                      terra::rast)
cl60_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir, 'fraccover_60_100/'),full.names = T, pattern = '.tif$')
                      ), 
                      terra::rast)
cl70_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir, 'fraccover_70_100/'),full.names = T, pattern = '.tif$')
                      ), 
                      terra::rast)
cl80_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir, 'fraccover_80_100/'),full.names = T, pattern = '.tif$')
                      ), 
                      terra::rast)
cl90_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir, 'fraccover_90_100/'),full.names = T, pattern = '.tif$')
                      ), 
                      terra::rast)
cl100_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir, 'fraccover_100_100/'),full.names = T, pattern = '.tif$')
                      ), 
                      terra::rast)
cl160_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir, 'fraccover_160_100/'),full.names = T, pattern = '.tif$')
                      ), 
                      terra::rast)
cl170_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir, 'fraccover_170_100/'),full.names = T, pattern = '.tif$')
                      ), 
                      terra::rast)
#change_layer = as.character(seq(from = 1992, to = 2020, by = 1))

# add percentages of different forest classes
forest = list()
for (j in 1:length(change_layer)){
  print(j)
  forest[j] = as.list(sum(cl50_change[[j]],cl60_change[[j]],cl70_change[[j]],
                          cl80_change[[j]],cl90_change[[j]],cl100_change[[j]],
                          cl160_change[[j]],cl170_change[[j]]))
}
terra::plot(cl50_change[[1]])
terra::plot(cl50_change[[27]])

# calculate global mean with ocean mask
LCchange4 = data.frame()       
for (i in 1:length(forest)){
  print(i)
  print(change_layer[i])
  # mask oceans
  masked = terra::mask(forest[[i]], mask_ocean_p)
  # get global mean
  mean_value = data.frame(x = change_layer[i], 
                          y = terra::global(masked, "mean", na.rm=TRUE)[,1])
  names(mean_value) = c('year', 'forest_mean')
  LCchange4 = rbind(LCchange4,mean_value)
}

# calculate global mean no mask
# LCchange5 = data.frame()       
# for (i in 1:length(forest)){
#   print(i)
#   print(change_layer[i])
#   # get gloabl mean
#   mean_value = data.frame(x = change_layer[i], 
#                           y = terra::global(forest[[i]], "mean", na.rm=TRUE)[,1])
#   names(mean_value) = c('year', 'forest_mean_noMask')
#   LCchange5 = rbind(LCchange5,mean_value)
# }

#### Join all types of changes
land_cover_change = LCchange1 %>% 
  inner_join(LCchange2, by = 'year') %>% 
  inner_join(LCchange3, by = 'year') %>% 
  inner_join(LCchange4, by = 'year') 

#### Calculate rate of change-----

land_cover_change = land_cover_change %>% 
  mutate(anthropo_mean_lag = lag(anthropo_mean, default = first(anthropo_mean)),
         antropo_change = anthropo_mean - anthropo_mean_lag) %>% 
  mutate(forest_mean_lag = lag(forest_mean, default = first(forest_mean)),
         forest_change = forest_mean - forest_mean_lag) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(anthropo_mean_0 = anthropo_mean - first(anthropo_mean)) %>% 
  mutate(forest_mean_0 = forest_mean - first(forest_mean)) %>% 
  dplyr::select(-anthropo_mean_lag, -forest_mean_lag)
write_csv(land_cover_change, 'C:/Users/yanis/Documents/IPBES/human_modification_indic/anthrome_change/fraccover_class_100/global_lc_change.csv')

#land_cover_change = read_csv('C:/Users/yanis/Documents/IPBES/human_modification_indic/anthrome_change/fraccover_class_100/global_lc_change.csv')


# Plot
range(land_cover_change$anthropo_mean)
range(land_cover_change$forest_mean)

# Change Separate line charts
p1 <- ggplot(land_cover_change, aes(x=year, y=antropo_change)) +
  geom_line(color="#69b3a2", size=2) +
  ggtitle("Anthropization") +
  theme_ipsum()

p2 <- ggplot(land_cover_change, aes(x=year, y=forest_change)) +
  geom_line(color="grey",size=2) +
  ggtitle("Forest") +
  theme_ipsum()
p1 + p2

# Mean in separate charts
p3 <- ggplot(land_cover_change, aes(x=year, y=anthropo_mean)) +
  geom_line(color="grey",linewidth=1) +
  ylim(15.5, 17.5) +
  ggtitle("Athropo change") +
  theme_ipsum()
p3
p4 <- ggplot(land_cover_change, aes(x=year, y=forest_mean)) +
  geom_line(color="grey",linewidth=1) +
  ylim(32, 33) +
  ggtitle("Forest change") +
  theme_ipsum()
p4


# Forest 2018
forest_2018 = terra::mask(forest[[27]],mask_ocean_p)
forest_2018_df <- as.data.frame(forest_2018, xy = TRUE)
names(forest_2018_df) = c('x','y','forest_2018')
forest_2018_df <- forest_2018_df %>%
  mutate(layer_cl = cut(forest_2018, breaks = 10))

forest_2018_plot =
  ggplot() +
  #geom_sf(data = world) + 
  geom_raster(data = forest_2018_df , aes(x = x, y = y, fill = forest_2018)) +
  scale_fill_viridis_c(na.value = "transparent", name = 'Forest 2018') 
forest_2018_plot

+
  #geom_sf(data = ej_data_sp_robin, aes(color = cluster)) + 
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5) # sets panel border
  )

### Load Fishery data-----
fish = read_csv(paste0(dir_git, 'data/corpus_cases/capture_fisheries_production_WDI/API_ER.FSH.CAPT.MT_DS2_en_csv_v2_45470.csv'))

# world
world_fish = fish %>% 
  filter(`Country Name`=='World') %>% 
  dplyr::select(-`Country Name`, -`Country Code`,-`Indicator Code`) %>% 
  pivot_longer(cols = -`Indicator Name`, names_to = 'year') %>% 
  dplyr::select(-`Indicator Name`, year, 'cap_fisherie'='value') %>% 
  mutate(year = as.integer(year)) %>% 
  filter(!is.na(cap_fisherie))


### Load TFC cases from Corpus-----

oa = readRDS(paste0(dir_git, 'data/corpus_cases/oa_count.rds'))
corpus = oa$oa_years %>% # whole oa corpus
  dplyr::select(year = publication_year, total_count = count, -p, -p_cum)
corpus_cases = oa$tca_case_years %>% # cases within the oa corpus
  dplyr::select(year = publication_year, cases_count = count,-p, -p_cum) %>% 
  dplyr::inner_join(corpus, by = 'year') %>% 
  dplyr::mutate(prop_cases = cases_count / total_count) #prop of cases from all corpus
#corpus_cases = counts from corpus_cases / counts from corpus




### Join land cover change and fisheries with TFC cases----
temp_analisys = land_cover_change %>% 
  full_join(world_fish) %>% 
  left_join(corpus_cases, by = c('year' = 'publication_year'))
  
names(temp_analisys)
write_csv(temp_analisys,paste0(dir_git, 'data/corpus_cases/oa_count_temp_analisys.csv'))


# Plot-----
# Separate line charts
p1 <- ggplot(temp_analisys, aes(x=year, y=anthropo_mean_0)) +
  geom_line(color="#69b3a2", linewidth=2) +
  ggtitle("Anthropization \n(Change in urban and agricultural areas) ") +
  theme_ipsum()
p1
p2 <- ggplot(temp_analisys, aes(x=year, y=cumsum(count)/max(count))) +
  geom_line(color="grey",linewidth=2) +
  ggtitle("TFC cases \n(New TFC cases in literature)") +
  theme_ipsum()
p2
p3 <- ggplot(temp_analisys, aes(x=year, y=forest_mean_0)) +
  geom_line(color="grey",linewidth=2) +
  ggtitle("Forest change \n(Change in forest land areas)") +
  theme_ipsum()
p3
p4 <- ggplot(temp_analisys, aes(x=year, y=cap_fisherie)) +
  geom_line(color="grey",linewidth=2) +
  ggtitle("Capture fisheries production (metric tons)") +
  theme_ipsum()
p4

range(temp_analisys$anthropo_mean_0)
range(temp_analisys$forest_mean_0)
range(temp_analisys$cumCount_0)
range(temp_analisys$propCumCount_0)

## Display charts together
# One Y-axes

temp_analisys_forest = temp_analisys %>% 
  filter(!is.na(forest_mean)) %>% 
  arrange(year) %>% 
  mutate(propCount = count/sum(count)) %>% 
  mutate(cumCount = cumsum(count)) %>% 
  mutate(cumCount_0 = cumCount - first(count)) %>% 
  mutate(propCumCount_0 = cumCount_0/sum(count))

ggplot(temp_analisys_forest, aes(x = year)) + 
  geom_line(aes(y = forest_mean_0*-1, colour = "Forest land cover Loss"), size=1.5, linetype = "dashed") + 
  geom_line(aes(y = propCumCount_0, colour = "TFC corpus"), size=1.5) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #geom_vline(xintercept=2017,linetype=3, size = 1) + 
  scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 2)) +
  ylim(0,1)

ggplot(temp_analisys_forest, aes(x = year)) + 
  geom_line(aes(y = anthropo_mean_0, colour = "Anthopogenic land cover expansion"), size=1.5, linetype = "dashed") + 
  geom_line(aes(y = propCumCount_0, colour = "TFC corpus"), size=1.5) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #geom_vline(xintercept=2017,linetype=3, size = 1) + 
  scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 2)) +
  ylim(0,1)

temp_analisys_fish = temp_analisys %>% 
  filter(!is.na(cap_fisherie)) %>% 
  arrange(year) %>% 
  mutate(propCount = count/sum(count)) %>% 
  mutate(cumCount = cumsum(count)) %>% 
  mutate(cumCount_0 = cumCount - first(count)) %>% 
  mutate(propCumCount_0 = cumCount_0/sum(count))

ggplot(temp_analisys_fish, aes(x = year)) + 
  geom_line(aes(y = cap_fisherie/max(cap_fisherie), colour = "Capture fisheries production"), size=1.5, linetype = "dashed") + 
  geom_line(aes(y = propCumCount_0, colour = "TFC corpus"), size=1.5) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #geom_vline(xintercept=2017,linetype=3, size = 1) + 
  scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1960, to = 2021, by = 5)) +
  ylim(0,1)


#Show 2 series on the same line chart thanks to sec.axis()
# We can use this sec.axis mathematical transformation to display 2 series that have a different range.
# ipsum theme to remove the black background and improve the general style, add a title, customize the Y axes to pair them with their related line.

# Value used to transform the data
coeff <- 152299001

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

# land cover
ggplot(temp_analisys, aes(x=year)) +
  
  geom_line( aes(y=anthropo_mean_0), linewidth=2, color=temperatureColor) + 
  geom_line( aes(y=cumCount_0 / coeff), linewidth=2, color=priceColor) +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 2)) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Anthropogenic land cover expansion",
    limits = c(0,1),
    n.breaks=6,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="TFC cases in literature")
  ) + 
  geom_hline(yintercept=0, color="black", linetype=2) +
  annotate("text", x = 1992, y = -0.005, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0)+
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=20),
    axis.title.y.right = element_text(color = priceColor, size=20) 
  )

# forest land
ggplot(temp_analisys, aes(x=year)) +
  
  geom_line( aes(y=forest_mean_0*-1), linewidth=2, color=temperatureColor) + 
  geom_line( aes(y=cumCount_0 / coeff), linewidth=2, color=priceColor) +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 2)) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Forest land cover loss",
    limits = c(0,1),
    n.breaks=6,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="TFC cases in literature")
  ) + 
  geom_hline(yintercept=0, color="black", linetype=2) +
  annotate("text", x = 1992, y = 0, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0)+
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=20),
    axis.title.y.right = element_text(color = priceColor, size=20) 
  )

# One Y-axes
ggplot(temp_analisys, aes(x = year)) + 
  geom_line(aes(y = forest_mean_0*-1, colour = "Forest land cover Loss"), size=1.5, linetype = "dashed") + 
  geom_line(aes(y = propCumCount_0, colour = "TFC corpus"), size=1.5) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #geom_vline(xintercept=2017,linetype=3, size = 1) + 
  scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 2)) +
  ylim(0,1)

ggplot(temp_analisys, aes(x = year)) + 
  geom_line(aes(y = anthropo_mean_0, colour = "Anthopogenic land cover expansion"), size=1.5, linetype = "dashed") + 
  geom_line(aes(y = propCumCount_0, colour = "TFC corpus"), size=1.5) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #geom_vline(xintercept=2017,linetype=3, size = 1) + 
  scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 2)) +
  ylim(0,1)

ggplot(temp_analisys, aes(x = year)) + 
  geom_line(aes(y = cap_fisherie/max(cap_fisherie), colour = "Capture fisheries production"), size=1.5, linetype = "dashed") + 
  geom_line(aes(y = propCumCount_0, colour = "TFC corpus"), size=1.5) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #geom_vline(xintercept=2017,linetype=3, size = 1) + 
  scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 2)) +
  ylim(0,1)
