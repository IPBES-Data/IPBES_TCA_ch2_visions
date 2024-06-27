rm(list=ls())

# TFC assess
# TfC cases from the Corpus (CH2)

# librarues
library(stringr)
#library(data.table)
#library(foreign)
library(tidyverse)
library(dplyr)
library(readxl)
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
dir_git <- 'C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_TCA_ch2_visions/'

# load oceans mask
mask_ocean = terra::rast('C:/Users/yanis/Documents/regions/ocean_mask.tif')

### Load anthromes change data-----
# Data available to download ere: https://zenodo.org/records/7561948

ant_change_dir = 'C:/Users/yanis/Documents/IPBES/human_modification_indic/anthrome_change/fraccover_class_100/'

#### Class 190: Urban----

urban <- lapply(as.list
                   (list.files(paste0(ant_change_dir),full.names = T, pattern = '__190__A100_.img$')
                   ), 
                   terra::rast)
#checks
# urban[1]
# terra::sources(urban[[1]])
# terra::plot(urban[[1]])

# extract years from sources and make sure all files are arranged in the way
change_layer = data.frame()
for (j in 1:length(urban)){
  print(j)
  layer = word(terra::sources(urban[[j]]),2,sep = "-P1Y-")
  year = word(layer, 1,sep = "-")
  urban_df = data.frame(id = j, 
                              file = layer,
                              year = year)
  change_layer = rbind(change_layer,urban_df)
}

#### Class 10-20-30-40: Agriculture------

cl10_change <- lapply(as.list
                       (list.files(paste0(ant_change_dir),full.names = T, pattern = '__10__A100_.img$')
                       ), 
                       terra::rast)
cl20_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir),full.names = T, pattern = '__20__A100_.img$')
                      ), 
                      terra::rast)
cl30_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir),full.names = T, pattern = '__30__A100_.img$')
                      ), 
                      terra::rast)
cl40_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir),full.names = T, pattern = '__40__A100_.img$')
                      ), 
                      terra::rast)

# checks
cl30_change[1]
terra::sources(cl30_change[[1]])
terra::plot(cl30_change[[1]])

# extract years from sources and make sure all files are arranged in the way
for (j in 1:length(cl40_change)){
  print(j)
  layer = word(terra::sources(cl40_change[[j]]),2,sep = "-P1Y-")
  year = word(layer, 1,sep = "-")
  cl40_change_df = data.frame(id = j, 
                          file = layer,
                          year = year)
  change_layer = rbind(change_layer,cl40_change_df)
}

for (j in 1:length(cl30_change)){
  print(j)
  layer = word(terra::sources(cl30_change[[j]]),2,sep = "-P1Y-")
  year = word(layer, 1,sep = "-")
  cl30_change_df = data.frame(id = j, 
                              file = layer,
                              year = year)
  change_layer = rbind(change_layer,cl30_change_df)
}
for (j in 1:length(cl20_change)){
  print(j)
  layer = word(terra::sources(cl20_change[[j]]),2,sep = "-P1Y-")
  year = word(layer, 1,sep = "-")
  cl20_change_df = data.frame(id = j, 
                              file = layer,
                              year = year)
  change_layer = rbind(change_layer,cl20_change_df)
}

for (j in 1:length(cl10_change)){
  print(j)
  layer = word(terra::sources(cl10_change[[j]]),2,sep = "-P1Y-")
  year = word(layer, 1,sep = "-")
  cl10_change_df = data.frame(id = j, 
                              file = layer,
                              year = year)
  change_layer = rbind(change_layer,cl10_change_df)
}

#### Class 50 - 100, 160 & 170: Forest------

cl50_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir),full.names = T, pattern = '__50__A100_.img$')
                      ), 
                      terra::rast)
cl60_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir),full.names = T, pattern = '__60__A100_.img$')
                      ), 
                      terra::rast)
cl70_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir),full.names = T, pattern = '__70__A100_.img$')
                      ), 
                      terra::rast)
cl80_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir),full.names = T, pattern = '__80__A100_.img$')
                      ), 
                      terra::rast)
cl90_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir),full.names = T, pattern = '__90__A100_.img$')
                      ), 
                      terra::rast)
cl100_change <- lapply(as.list
                       (list.files(paste0(ant_change_dir),full.names = T, pattern = '__100__A100_.img$')
                       ), 
                       terra::rast)
cl160_change <- lapply(as.list
                       (list.files(paste0(ant_change_dir),full.names = T, pattern = '__160__A100_.img$')
                       ), 
                       terra::rast)
cl170_change <- lapply(as.list
                       (list.files(paste0(ant_change_dir),full.names = T, pattern = '__170__A100_.img$')
                       ), 
                       terra::rast)

# checks
cl170_change[1]
terra::sources(cl170_change[[1]])
terra::plot(cl170_change[[1]])

# extract years from sources and make sure all files are arranged in the way
for (j in 1:length(cl50_change)){
  print(j)
  layer = word(terra::sources(cl50_change[[j]]),2,sep = "-P1Y-")
  year = word(layer, 1,sep = "-")
  cl50_change_df = data.frame(id = j, 
                              file = layer,
                              year = year)
  change_layer = rbind(change_layer,cl50_change_df)
}

for (j in 1:length(cl60_change)){
  print(j)
  layer = word(terra::sources(cl60_change[[j]]),2,sep = "-P1Y-")
  year = word(layer, 1,sep = "-")
  cl60_change_df = data.frame(id = j, 
                              file = layer,
                              year = year)
  change_layer = rbind(change_layer,cl60_change_df)
}
for (j in 1:length(cl70_change)){
  print(j)
  layer = word(terra::sources(cl70_change[[j]]),2,sep = "-P1Y-")
  year = word(layer, 1,sep = "-")
  cl70_change_df = data.frame(id = j, 
                              file = layer,
                              year = year)
  change_layer = rbind(change_layer,cl70_change_df)
}

for (j in 1:length(cl80_change)){
  print(j)
  layer = word(terra::sources(cl80_change[[j]]),2,sep = "-P1Y-")
  year = word(layer, 1,sep = "-")
  cl80_change_df = data.frame(id = j, 
                              file = layer,
                              year = year)
  change_layer = rbind(change_layer,cl80_change_df)
}
for (j in 1:length(cl90_change)){
  print(j)
  layer = word(terra::sources(cl90_change[[j]]),2,sep = "-P1Y-")
  year = word(layer, 1,sep = "-")
  cl90_change_df = data.frame(id = j, 
                              file = layer,
                              year = year)
  change_layer = rbind(change_layer,cl90_change_df)
}

for (j in 1:length(cl100_change)){
  print(j)
  layer = word(terra::sources(cl100_change[[j]]),2,sep = "-P1Y-")
  year = word(layer, 1,sep = "-")
  cl100_change_df = data.frame(id = j, 
                              file = layer,
                              year = year)
  change_layer = rbind(change_layer,cl100_change_df)
}
for (j in 1:length(cl160_change)){
  print(j)
  layer = word(terra::sources(cl160_change[[j]]),2,sep = "-P1Y-")
  year = word(layer, 1,sep = "-")
  cl160_change_df = data.frame(id = j, 
                              file = layer,
                              year = year)
  change_layer = rbind(change_layer,cl160_change_df)
}

for (j in 1:length(cl170_change)){
  print(j)
  layer = word(terra::sources(cl170_change[[j]]),2,sep = "-P1Y-")
  year = word(layer, 1,sep = "-")
  cl170_change_df = data.frame(id = j, 
                              file = layer,
                              year = year)
  change_layer = rbind(change_layer,cl170_change_df)
}

change_layer = change_layer %>%  group_by(year,id) %>% count()
# all files are order and named in the same way (2016 - 2015)

#### Mask oceans and get global mean values------

# resample ocean mask to match modification layer
mask_ocean_p = terra::project(mask_ocean, urban[[1]],
                              method='near',
                              align=TRUE,
                              threads=TRUE)

#### URBAN----
# calculate global mean of urban land
LCchange1 = data.frame()       
for (i in 1:length(urban)){
  print(i)
  # get year from layer
  layer = word(terra::sources(urban[[i]]),2,sep = "-P1Y-")
  year = word(layer, 1,sep = "-")
  # mask oceans
  masked = terra::mask(urban[[i]], mask_ocean_p)
  # get global
  mean_value = data.frame(x = year, 
                          y = terra::global(masked, "mean", na.rm=TRUE)[,1])
  names(mean_value) = c('year', 'urban_mean')
  LCchange1 = rbind(LCchange1,mean_value)
}

#### AGRI----
# add percentages of different agricultural classes
agri = list()
for (j in 1:length(change_layer$year)){
  print(j)
  agri[j] = as.list(sum(cl10_change[[j]],cl20_change[[j]],
                        cl30_change[[j]],cl40_change[[j]]))
}
#checks
agri[1] #no sources are kept
terra::plot(cl10_change[[1]])
terra::plot(cl20_change[[1]])
terra::plot(cl30_change[[1]])
terra::plot(cl40_change[[1]])
terra::plot(agri[[1]])

# calculate global mean
LCchange2 = data.frame()       
for (i in 1:length(agri)){
  print(i)
  # get year from change_layer where it matches i
  year = subset(change_layer, id == i)$year
  # mask oceans
  masked = terra::mask(agri[[i]], mask_ocean_p)
  # get global mean
  mean_value = data.frame(x = year, 
                          y = terra::global(masked, "mean", na.rm=TRUE)[,1])
  names(mean_value) = c('year', 'agri_mean')
  LCchange2 = rbind(LCchange2,mean_value)
}

#### ANTHROPO----
# add percentages of different agricultural classes AND urban
anthropo = list()
for (j in 1:length(change_layer$year)){
  print(j)
  anthropo[j] = as.list(sum(urban[[j]], cl10_change[[j]],
                            cl20_change[[j]],cl30_change[[j]],cl40_change[[j]]))
}

#checks
anthropo[1] #no sources are kept
terra::plot(urban[[8]])
terra::plot(cl10_change[[8]])
terra::plot(cl20_change[[8]])
terra::plot(cl30_change[[8]])
terra::plot(cl40_change[[8]])
terra::plot(agri[[8]])
terra::plot(anthropo[[8]])

# calculate global mean with ocean mask
LCchange3 = data.frame()       
for (i in 1:length(anthropo)){
  print(i)
  # get year from change_layer where it matches i
  year = subset(change_layer, id == i)$year
  # mask oceans
  masked = terra::mask(anthropo[[i]], mask_ocean_p)
  # get global mean
  mean_value = data.frame(x = year, 
                          y = terra::global(masked, "mean", na.rm=TRUE)[,1])
  names(mean_value) = c('year', 'anthropo_mean')
  LCchange3 = rbind(LCchange3,mean_value)
}


#### FOREST----

# add percentages of different forest classes
forest = list()
for (j in 1:length(change_layer$year)){
  print(j)
  forest[j] = as.list(sum(cl50_change[[j]],cl60_change[[j]],cl70_change[[j]],
                          cl80_change[[j]],cl90_change[[j]],cl100_change[[j]],
                          cl160_change[[j]],cl170_change[[j]]))
}

#checks
forest[1] #no sources are kept
terra::plot(cl50_change[[8]])
terra::plot(cl60_change[[8]])
terra::plot(cl70_change[[8]])
terra::plot(cl80_change[[8]])
terra::plot(cl90_change[[8]])
terra::plot(cl100_change[[8]])
terra::plot(cl160_change[[8]])
terra::plot(cl170_change[[8]])
terra::plot(forest[[8]])

# calculate global mean with ocean mask
LCchange4 = data.frame()       
for (i in 1:length(forest)){
  print(i)
  # get year from change_layer where it matches i
  year = subset(change_layer, id == i)$year
  # mask oceans
  masked = terra::mask(forest[[i]], mask_ocean_p)
  # get global mean
  mean_value = data.frame(x = year, 
                          y = terra::global(masked, "mean", na.rm=TRUE)[,1])
  names(mean_value) = c('year', 'forest_mean')
  LCchange4 = rbind(LCchange4,mean_value)
}

#### Join all types of changes------
land_cover_change = LCchange1 %>% 
  inner_join(LCchange2, by = 'year') %>% 
  inner_join(LCchange3, by = 'year') %>% 
  inner_join(LCchange4, by = 'year') 

#### Calculate rate of change

land_cover_change = land_cover_change %>% 
  # set year as a number
  mutate(year = as.integer(year)) %>% 
  # order by year
  arrange(year) %>% 
  # calculate dif from previous year (lag) and rate of change
  mutate(anthropo_mean_lag = lag(anthropo_mean, default = first(anthropo_mean)),
         anthropo_change = anthropo_mean - anthropo_mean_lag) %>% 
  mutate(forest_mean_lag = lag(forest_mean, default = first(forest_mean)),
         forest_change = forest_mean - forest_mean_lag) %>% 
  # set 1992 as baseline
  mutate(anthropo_mean_0 = anthropo_mean - first(anthropo_mean)) %>% 
  mutate(forest_mean_0 = forest_mean - first(forest_mean)) %>% 
  # clean dataset
  dplyr::select(-anthropo_mean_lag, -forest_mean_lag) %>% 
  write_csv('C:/Users/yanis/Documents/IPBES/human_modification_indic/anthrome_change/global_lc_change_frac100.csv')

land_cover_change = read_csv('C:/Users/yanis/Documents/IPBES/human_modification_indic/anthrome_change/global_lc_change_frac100.csv')


#### Plot------
range(land_cover_change$antropo_change)
range(land_cover_change$forest_change)

# Change in separate line charts
p1 <- ggplot(land_cover_change, aes(x=year, y=antropo_change)) +
  geom_line(color="#69b3a2", linewidth=2) +
  ggtitle("Anthropization") +
  theme_ipsum()
p1
p2 <- ggplot(land_cover_change, aes(x=year, y=forest_change)) +
  geom_line(color="grey",linewidth=2) +
  ggtitle("Forest") +
  theme_ipsum()
p2
p1 + p2

# Mean in separate charts
range(land_cover_change$anthropo_mean)
range(land_cover_change$forest_mean)
p3 <- ggplot(land_cover_change, aes(x=year, y=anthropo_mean)) +
  geom_line(color="grey",linewidth=1) +
  #ylim(0, 0.3) +
  ggtitle("Athropo") +
  theme_ipsum()
p3
p4 <- ggplot(land_cover_change, aes(x=year, y=forest_mean)) +
  geom_line(color="grey",linewidth=1) +
  #ylim(0, 0.3) +
  ggtitle("Forest") +
  theme_ipsum()
p4
p3 + p4

# Forest 2022
forest_2022 = terra::mask(forest[[31]],mask_ocean_p)
forest_2022_df <- as.data.frame(forest_2022, xy = TRUE)
names(forest_2022_df) = c('x','y','forest_2022')
forest_2022_df <- forest_2022_df %>%
  mutate(layer_cl = cut(forest_2022, breaks = 10))

forest_2022_plot =
  ggplot() +
  #geom_sf(data = world) + 
  geom_raster(data = forest_2022_df , aes(x = x, y = y, fill = forest_2022)) +
  scale_fill_viridis_c(na.value = "transparent", name = 'Forest 2022') +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5) # sets panel border
  )
forest_2022_plot


# Forest 1999
forest_1999 = terra::mask(forest[[1]],mask_ocean_p)
forest_1999_df <- as.data.frame(forest_1999, xy = TRUE)
names(forest_1999_df) = c('x','y','forest_1999')
forest_1999_df <- forest_1999_df %>%
  mutate(layer_cl = cut(forest_1999, breaks = 10))

forest_1999_plot =
  ggplot() +
  #geom_sf(data = world) + 
  geom_raster(data = forest_1999_df , aes(x = x, y = y, fill = forest_1999)) +
  scale_fill_viridis_c(na.value = "transparent", name = 'Forest 1999') +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5) # sets panel border
  )
forest_1999_plot


### Load Fishery data-----

fish_stock = read_excel(paste0(dir_git, 'data/fisheries/Status of fish stocks.xlsx'), sheet = 'collapsed_overexploited')
# Data can be found in seaaroundus.org
#use the percentage of collapse + overexploited stocks (Raw 9) as the intensity of fishing fleets operating globally

fish_stock = fish_stock %>% 
  # sort by year
  arrange(year) %>% 
  # calculate dif from previous year (lag) and rate of change in fishery intensity
  rename(fish_stock = prop_collapsed_overexploited) %>% 
  mutate(fish_stock_lag = lag(fish_stock, default = first(fish_stock)),
         fish_stock_change = fish_stock - fish_stock_lag) %>% 
  # calculate cumulative sums --> NOT POSSIBLE BECAUSE WE DO NOT HAVE THE ORIGINAL DATA, WE ONLY HAVE %
  # set 1992 as baseline --> NOT POSSIBLE BECAUSE WE ONLY HAVE % AND CANNOT SUM THEM 
  write_csv(paste0(dir_git, 'data/fisheries/intensity_fishing.csv'))
fish_stock = read_csv(paste0(dir_git, 'data/fisheries/intensity_fishing.csv'))

### Load TFC cases from Corpus-----
# Data  available in https://github.com/IPBES-Data/IPBES_TCA_Corpus
oa = readRDS(paste0(dir_git, 'data/corpus_cases/oa_count_may.rds'))
corpus = oa$oa_years %>% # whole oa corpus
  dplyr::select(year = publication_year, total_count = count, -p, -p_cum)
tca_corpus = oa$tca_years %>% # whole oa corpus
  dplyr::select(year = publication_year, tca_count = count, -p, -p_cum)
corpus_cases = oa$case_years %>% # cases within the oa corpus
  dplyr::select(year = publication_year, cases_count = count,-p, -p_cum) %>% 
  # get all corpus data to calculate proportions of new cases out of the whole corpus
  dplyr::inner_join(corpus, by = 'year') %>% 
  # get TCA corpus data to calculate proportions of new cases out of the whole corpus
  dplyr::inner_join(tca_corpus, by = 'year') %>% 
  # get proportions
  dplyr::mutate(prop_new_cases_tca = cases_count / tca_count) %>% #prop of cases from TCA corpus
  dplyr::mutate(prop_new_cases = cases_count / total_count) %>% #prop of cases from all corpus
  # remove odd dates
  dplyr::filter(year <= 2023)
#corpus_cases = counts from corpus_cases / counts from corpus


### Join land covers and fisheries with TFC cases----
temp_analisys = land_cover_change %>% 
  # join with fisheries keeping all years
  full_join(fish_stock, by = 'year') %>% 
  # join with courpus keeping only years with land cover or fishery data
  left_join(corpus_cases, by = 'year') %>% 
  arrange(year) %>% 
  # calculate cumulative sums for corpus (aggregation of new cases)
  mutate(corpus_aggr = cumsum(total_count)) %>% 
  mutate(cases_aggr = cumsum(cases_count)) %>% 
  mutate(prop_aggr = cases_aggr/corpus_aggr) %>% 
  # set 1992 as baseline when aggregated
  mutate(cases_aggr_0 = cases_aggr - first(cases_aggr)) %>% 
  mutate(corpus_aggr_0 = corpus_aggr - first(corpus_aggr)) %>% 
  #mutate(prop_aggr_0 = cases_aggr_0/corpus_aggr_0) %>% 
  mutate(prop_aggr_0 = prop_aggr - first(prop_aggr)) %>% 
  # clean dataset
  dplyr::select(-corpus_aggr,-corpus_aggr_0, -cases_aggr,-cases_aggr_0) 


names(temp_analisys)
write_csv(temp_analisys,paste0(dir_git, 'data/corpus_cases/oa_fish_lcc_temp_analisys.csv'))

temp_analisys = read_csv(paste0(dir_git, 'data/corpus_cases/oa_fish_lcc_temp_analisys.csv'))


# Plots-----
#### Separate line charts
range(temp_analisys$prop_new_cases)
p1c <- ggplot(temp_analisys, aes(x=year, y=prop_new_cases)) +
  geom_line(color="grey",linewidth=2) +
  ggtitle("Prop. of new cases\nin OA corpus") +
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  theme_ipsum()
p1c

p1c_tca <- ggplot(temp_analisys, aes(x=year, y=prop_new_cases_tca)) +
  geom_line(color="grey",linewidth=2) +
  ggtitle("Prop. of new cases\nin TCA corpus") +
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  theme_ipsum()
p1c_tca

p1c + p1c_tca

p1 <- ggplot(temp_analisys, aes(x=year, y=prop_aggr_0)) +
  geom_line(color="grey",linewidth=2) +
  ggtitle("Prop. of accumulated cases\nin TCA corpus") +
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  theme_ipsum()
p1
p1c + p1 

p2 <- ggplot(temp_analisys, aes(x=year, y=anthropo_mean_0)) +
  geom_line(color="grey", linewidth=2) +
  ggtitle("Fraction of urban and\nagricultural areas") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  geom_hline(yintercept=0, color="black", linetype=2) +
  annotate("text", x = 1995, y = 0.0003, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  theme_ipsum()
p2
p2c <- ggplot(temp_analisys, aes(x=year, y=anthropo_change)) +
  geom_line(color="grey", linewidth=2) +
  ggtitle("Change in urban and\nagricultural areas") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  geom_hline(yintercept=0, color="black", linetype=2) +
  annotate("text", x = 1992, y = -0.00001, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  theme_ipsum()
p2c
p2 + p2c

p3 <- ggplot(temp_analisys, aes(x=year, y=forest_mean_0)) +
  geom_line(color="grey",linewidth=2) +
  ggtitle("Fraction of forest areas\n(in land)") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  geom_hline(yintercept=0, color="black", linetype=2) +
  annotate("text", x = 1992, y = 0.0003, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  theme_ipsum()
p3
p3c <- ggplot(temp_analisys, aes(x=year, y=forest_change)) +
  geom_line(color="grey",linewidth=2) +
  ggtitle("Change in forest areas\n(in land)") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  geom_hline(yintercept=0, color="black", linetype=2) +
  annotate("text", x = 1992, y = 0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  theme_ipsum()
p3c
p3 + p3c

p4 <- ggplot(temp_analisys, aes(x=year, y=fish_stock)) +
  geom_line(color="grey",linewidth=2) +
  ggtitle("Fishing intensity (percentage of\ncollapsed and overexploited fish stocks)") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  theme_ipsum()
p4
p4c <- ggplot(temp_analisys, aes(x=year, y=fish_stock_change)) +
  geom_line(color="grey",linewidth=2) +
  ggtitle("Change in the fishing intensity") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  geom_hline(yintercept=0, color="black", linetype=2) +
  annotate("text", x = 1992, y = 0.0001, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  theme_ipsum()
p4c
p4 + p4c

#### Display ALL together -----
temp_analisys = temp_analisys %>% 
  dplyr::mutate(index_forest = (forest_mean)/first(forest_mean)) %>% 
  dplyr::mutate(index_anthropo = anthropo_mean/first(anthropo_mean)) %>% 
  dplyr::mutate(index_fish = fish_stock/first(fish_stock)) %>% 
  dplyr::mutate(scaled_forest = scale(forest_mean*-1)) %>% 
  dplyr::mutate(scaled_anthropo = scale(anthropo_mean)) %>% 
  dplyr::mutate(scaled_fish = scale(fish_stock)) %>% 
  dplyr::mutate(normalized_forest = ((forest_mean*-1) - min(forest_mean*-1)) / (max(forest_mean*-1) - min(forest_mean*-1))) %>% 
  dplyr::mutate(normalized_anthropo = (anthropo_mean - min(anthropo_mean)) / (max(anthropo_mean) - min(anthropo_mean))) %>% 
  dplyr::mutate(normalized_fish = (fish_stock - min(fish_stock,na.rm = TRUE)) / (max(fish_stock,na.rm = TRUE) - min(fish_stock,na.rm = TRUE)))

p5 <- ggplot(temp_analisys, ) +
  geom_line(aes(x=year, y=index_fish), color="light grey",linewidth=2) +
  geom_line(aes(x=year, y=index_forest), color="black",linewidth=2) +
  geom_line(aes(x=year, y=index_anthropo), color="dark grey",linewidth=2) +
  #scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  ylim(0.9,2.2)+
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = 0.0001, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  theme_ipsum()
p5

p5_floss <- ggplot(temp_analisys, ) +
  geom_line(aes(x=year, y=index_fish), color="light grey",linewidth=2) +
  geom_line(aes(x=year, y=((1-index_forest)+1)), color="black",linewidth=2) +
  geom_line(aes(x=year, y=index_anthropo), color="dark grey",linewidth=2) +
  #scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  ylim(0.9,2.2)+
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = 0.0001, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  theme_ipsum()
p5_floss
p5 + p5_floss

p6 <- ggplot(temp_analisys) +
  geom_line(aes(x=year, y=scaled_fish), color="light grey",linewidth=2) +
  geom_line(aes(x=year, y=scaled_forest), color="black",linewidth=2) +
  geom_line(aes(x=year, y=scaled_anthropo), color="dark grey",linewidth=2) +
  #scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  #ylim(0.9,2.3)+
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = 0.0001, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  theme_ipsum()
p6

p7 <- ggplot(temp_analisys) +
  geom_line(aes(x=year, y=normalized_fish), color="light grey",linewidth=2) +
  geom_line(aes(x=year, y=normalized_forest), color="black",linewidth=2) +
  geom_line(aes(x=year, y=normalized_anthropo), color="dark grey",linewidth=2) +
  #scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  #ylim(0.9,2.3)+
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = 0.0001, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  theme_ipsum()
p7

p8 <- ggplot(temp_analisys) +
  geom_line(aes(x=year, y=fish_stock-first(fish_stock)), color="light grey",linewidth=2) +
  geom_line(aes(x=year, y=forest_mean_0), color="black",linewidth=2) +
  geom_line(aes(x=year, y=anthropo_mean_0), color="dark grey",linewidth=2) +
  #scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  #ylim(0.9,2.3)+
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = 0.0001, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  theme_ipsum()
p8

p8_floss <- ggplot(temp_analisys) +
  geom_line(aes(x=year, y=fish_stock-first(fish_stock), color="Collapsed and overexploited fish stocks"),linewidth=2) +
  geom_line(aes(x=year, y=forest_mean_0*-1, color="Forest land cover loss"), linewidth=2) +
  geom_line(aes(x=year, y=anthropo_mean_0, color="Agriculture and urban land use expansion"),linewidth=2) +
  #scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') +
  scale_colour_manual("", 
                      breaks = c("Forest land cover loss", "Agriculture and urban land use expansion", "Collapsed and overexploited fish stocks"),
                      values = c("grey", "grey", "grey")) +
  
  scale_x_continuous(breaks = seq(from = 1992, to = 2023, by = 5)) +
  #ylim(0.9,2.3)+
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = 0.0001, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  theme_ipsum()
p8_floss
p8 +p8_floss

p5_floss +p6 +p7+p8_floss


### ALL

coeff = 100

# relative values
ggplot(temp_analisys) + 
  geom_bar(aes(x = year, y = prop_new_cases_tca, fill = y),
           stat="identity", fill = '#bbbbbb', color='white') + 
  geom_line(aes(x = year, y = ((((1-index_forest)+1))/coeff)-1/100,color="Forest loss index", linetype = "Forest loss index"),
            linewidth=1.5, color='black') + 
  geom_line(aes(x = year, y = (index_anthropo/coeff)-1/100, color="Agriculture and urban expansion index", linetype = "Agriculture and urban expansion index"),
            linewidth=1.5, color='black') + 
  geom_line(aes(x = year, y = (index_fish/coeff)-1/100, color="Collapsed and overexploited fish stocks index", linetype = "Collapsed and overexploited fish stocks index"), 
            linewidth=2, color='black') + 
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 5)) +
  # scale_colour_manual("",
  #                     breaks = c("Forest loss index", "Agriculture and urban expansion index", "Collapsed and overexploited fish stocks index"),
  #                     values = c("black","black","black")) +
  scale_linetype_manual("", 
                        breaks = c("Forest loss index","Agriculture and urban expansion index","Collapsed and overexploited fish stocks index"),
                        values = c("solid","twodash","dotted"))+
  scale_y_continuous(
    # First axis
    labels = scales::percent,
    name = "Cases in TCA corpus",
    #limits = c(-0.001,0.013),
    n.breaks=6,
    # Second axis and specify its features
    sec.axis = sec_axis(~(.*coeff)+1, 
                        name="Indices"#,
                        #breaks = seq(from = 1, to = 3, by = 0.5),
    )) +
  labs(x = NULL, y = NULL) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = '#bbbbbb', size=20,hjust = 0.5),
    axis.title.y.right = element_text(color = "black",size=20,hjust = 0.5),
    legend.text = element_text(size=15),
    legend.position="bottom"
  )


# normalized values
plot = ggplot(temp_analisys) + 
  geom_bar(aes(x = year, y = prop_new_cases_tca, fill = y),
           stat="identity", fill = '#bbbbbb', color='white') + 
  geom_line(aes(x = year, y = normalized_forest/100,color="Forest loss index", linetype = "Forest loss index"),
            linewidth=1.5, color='black') + 
  geom_line(aes(x = year, y = normalized_anthropo/100, color="Agriculture and urban\nexpansion index", linetype = "Agriculture and urban\nexpansion index"),
            linewidth=1.5, color='black') + 
  geom_line(aes(x = year, y = normalized_fish/100, color="Collapsed and overexploited\nfish stocks index", linetype = "Collapsed and overexploited\nfish stocks index"), 
            linewidth=2, color='black') + 
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 5)) +
  # scale_colour_manual("",
  #                     breaks = c("Forest loss index", "Agriculture and urban expansion index", "Collapsed and overexploited fish stocks index"),
  #                     values = c("black","black","black")) +
  scale_linetype_manual("", 
                        breaks = c("Forest loss index","Agriculture and urban\nexpansion index","Collapsed and overexploited\nfish stocks index"),
                        values = c("solid","twodash","dotted"))+
  scale_y_continuous(
    # First axis
    labels = scales::percent,
    name = "Cases in TCA corpus",
    #limits = c(-0.001,0.013),
    n.breaks=6,
    # Second axis and specify its features
    sec.axis = sec_axis(~(.*coeff), 
                        name="Indices"#,
                        #breaks = seq(from = 1, to = 3, by = 0.5),
    )) +
  labs(x = NULL, y = NULL) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = '#bbbbbb', size=20,hjust = 0.5,face="bold"),
    axis.title.y.right = element_text(color = "black",size=20,hjust = 0.5),
    legend.text = element_text(size=13),
    legend.position="bottom"
  )
plot
ggsave(file=paste0(dir_git,"output/corpus_cases/cases_quant.svg"), plot=plot, width=8, height=8, dpi = 300)



temperatureColor <- "#EEBAB4"
priceColor <- "#7CA1CC"
coeff = 100
ggplot(temp_analisys) + 
  geom_bar(aes(x = year, y = prop_new_cases_tca, fill = y),
           stat="identity", color="white", fill = priceColor) + 
  geom_line(aes(x = year, y = normalized_forest/coeff,color="Forest land cover loss"),
            linewidth=1.5, linetype = "solid") + 
  geom_line(aes(x = year, y = normalized_anthropo/coeff, color="Agriculture and urban land use expansion"),
            linewidth=1.5, linetype = "longdash") + 
  geom_line(aes(x = year, y = normalized_fish/coeff, color="Collapsed and overexploited fish stocks"), 
             linewidth=2, linetype = "dotted") + 
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  #scale_color_manual(name = "temperatureColor", values = c("Forest land cover loss", "Agriculture and urban land use expansion", "Collapsed and overexploited fish stocks")) +
  #scale_color_manual(values = c("temperatureColor", "temperatureColor", "temperatureColor")) +
  scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 5)) +
  #scale_y_continuous(labels = scales::percent) +
  scale_y_continuous(
    labels = scales::percent,
    # Features of the first axis
    name = "Cases in TCA corpus",
    #limits = c(-0.001,0.013),
    n.breaks=6,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, 
                        name="Indices"#,
                        #labels = scales::percent
    )) +
  labs(x = NULL, y = NULL, color = "") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = priceColor, size=20),
    axis.title.y.right = element_text(color = temperatureColor, size=20)
  )

#### Display together forest-----
### accumulated (mean forest + corpus size), baseline 1992

range(temp_analisys$forest_mean_0)
range(temp_analisys$prop_aggr_0)

ggplot(temp_analisys, aes(x = year)) + 
  geom_line(aes(y = forest_mean_0*-1, colour = "Forest land cover Loss"), size=1.5, linetype = "dashed") + #convert to loss
  geom_line(aes(y = prop_aggr_0, colour = "Accumulated TFC cases in corpus"), size=1.5) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') 
  #ggtitle("Proportion of forest loss globally vs\nProportion of cases in TFC corpus")

ggplot(temp_analisys, aes(x = year)) + 
  geom_line(aes(y = forest_mean_0*-1, colour = "Forest land cover Loss"), size=1.5, linetype = "dashed") + #convert to loss
  geom_line(aes(y = prop_new_cases_0, colour = "New TFC cases in corpus"), size=1.5) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') 
#ggtitle("Proportion of forest loss globally vs\nProportion of cases in TFC corpus")

### change (forest change + new TCF cases)
# range(temp_analisys$forest_change)
# range(temp_analisys$prop_new_cases)

temperatureColor <- "#EEBAB4"
priceColor <- "#7CA1CC"

temp_analisys$prop_new_cases_0 = temp_analisys$prop_new_cases
temp_analisys[1,19] = 0

coeff <- 10
ggplot(temp_analisys, aes(x=year)) +
  # Corpus is first axis (priceColor)
  geom_line( aes(y=prop_new_cases_0), linewidth=2, color=priceColor) +
  geom_line( aes(y=forest_change*coeff), linewidth=2, color=temperatureColor) + 
  scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 5)) +
  scale_y_continuous(
    labels = scales::percent,
    # Features of the first axis
    name = "New TFC cases in literature",
    #limits = c(-0.001,0.013),
    n.breaks=6,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, 
                        name="Change in forest cover (in land)",
                        labels = scales::percent
    )) + 
  geom_hline(yintercept=0, color="black", linetype=2) +
  annotate("text", x = 1992, y = 0.0001, label = "Baseline 1992", color = 'black', vjust = 0, hjust = 0)+
  labs(x = NULL) +
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = priceColor, size=20),
    axis.title.y.right = element_text(color = temperatureColor, size=20) 
  )


### Mix (mean forest + new TCF cases), baseline 1992

range(temp_analisys$forest_mean_0)
range(temp_analisys$prop_aggr_0)

ggplot(temp_analisys) + 
  geom_bar(aes(x = year, y = prop_new_cases, fill = y),
           stat="identity", color="white", fill = priceColor) + 
  # geom_line(aes(x = year, y = forest_mean_0*-1, colour = "Forest land cover Loss"),
  #           size=1.5, linetype = "dashed") + #convert to loss
  geom_line( aes(x = year, y = forest_mean_0*-1), linewidth=2, color=temperatureColor) + 
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  #scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 5)) +
  #scale_y_continuous(labels = scales::percent) +
  scale_y_continuous(
    labels = scales::percent,
    # Features of the first axis
    name = "New TFC cases in literature",
    #limits = c(-0.001,0.013),
    n.breaks=6,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., 
                        name="Forest loss (in land)",
                        labels = scales::percent
    )) +
  labs(x = NULL, y = NULL, color = "") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = priceColor, size=20),
    axis.title.y.right = element_text(color = temperatureColor, size=20) 
  )

ggplot(temp_analisys) + 
  geom_bar(aes(x = year, y = prop_new_cases, fill = y),
           stat="identity", color="white", fill = priceColor) + 
  # geom_line(aes(x = year, y = forest_mean_0*-1, colour = "Forest land cover Loss"),
  #           size=1.5, linetype = "dashed") + #convert to loss
  geom_line( aes(x = year, y = forest_change*coeff), linewidth=2, color=temperatureColor) + 
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  #scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 5)) +
  #scale_y_continuous(labels = scales::percent) +
  scale_y_continuous(
    labels = scales::percent,
    # Features of the first axis
    name = "New TFC cases in literature",
    #limits = c(-0.001,0.013),
    n.breaks=6,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, 
                        name="Forest loss (in land)",
                        labels = scales::percent
    )) +
  labs(x = NULL, y = NULL, color = "") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = priceColor, size=20),
    axis.title.y.right = element_text(color = temperatureColor, size=20) 
  )


#### Display together anthropogenic areas-----
### accumulated (mean forest + corpus size), baseline 1992

range(temp_analisys$anthropo_mean_0)
range(temp_analisys$prop_aggr_0)

ggplot(temp_analisys, aes(x = year)) + 
  geom_line(aes(y = anthropo_mean_0, colour = "Anthopogenic land cover expansion"), size=1.5, linetype = "dashed") + #convert to loss
  geom_line(aes(y = prop_aggr_0, colour = "Accumulated TFC cases in corpus"), size=1.5) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') 
#ggtitle("Proportion of anthropogenic expansion globally vs\nProportion of cases in TFC corpus") +
  
ggplot(temp_analisys, aes(x = year)) + 
  geom_line(aes(y = anthropo_mean_0, colour = "Anthopogenic land cover expansion"), size=1.5, linetype = "dashed") + #convert to loss
  geom_line(aes(y = prop_new_cases_0, colour = "New TFC cases in corpus"), size=1.5) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') 
#ggtitle("Proportion of anthropogenic expansion globally  vs\nProportion of new cases in TFC corpus")

### change (forest change + new TCF cases)
# range(temp_analisys$forest_change)
# range(temp_analisys$prop_new_cases)

temperatureColor <- "#EEBAB4"
priceColor <- "#7CA1CC"

# temp_analisys$prop_new_cases_0 = temp_analisys$prop_new_cases
# temp_analisys[1,19] = 0

coeff <- 10
ggplot(temp_analisys, aes(x=year)) +
  # Corpus is first axis (priceColor)
  geom_line( aes(y=prop_new_cases_0), linewidth=2, color=priceColor) +
  geom_line( aes(y=anthropo_change*coeff), linewidth=2, color=temperatureColor) + 
  scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 5)) +
  scale_y_continuous(
    labels = scales::percent,
    # Features of the first axis
    name = "New TFC cases in literature",
    #limits = c(-0.001,0.013),
    n.breaks=6,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, 
                        name="Change in anthropogenic lands",
                        labels = scales::percent
    )) + 
  geom_hline(yintercept=0, color="black", linetype=2) +
  annotate("text", x = 1992, y = 0.0001, label = "Baseline 1992", color = 'black', vjust = 0, hjust = 0)+
  labs(x = NULL) +
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = priceColor, size=20),
    axis.title.y.right = element_text(color = temperatureColor, size=20) 
  )


### Mix (mean forest + new TCF cases), baseline 1992

range(temp_analisys$forest_mean_0)
range(temp_analisys$prop_aggr_0)

ggplot(temp_analisys) + 
  geom_bar(aes(x = year, y = prop_new_cases, fill = y),
           stat="identity", color="white", fill = priceColor) + 
  geom_line( aes(x = year, y = anthropo_mean_0), linewidth=2, color=temperatureColor) + 
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  #scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 5)) +
  #scale_y_continuous(labels = scales::percent) +
  scale_y_continuous(
    labels = scales::percent,
    # Features of the first axis
    name = "New TFC cases in literature",
    #limits = c(-0.001,0.013),
    n.breaks=6,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., 
                        name="Anthropogenic lands",
                        labels = scales::percent
    )) +
  labs(x = NULL, y = NULL, color = "") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = priceColor, size=20),
    axis.title.y.right = element_text(color = temperatureColor, size=20) 
  )

ggplot(temp_analisys) + 
  geom_bar(aes(x = year, y = prop_new_cases, fill = y),
           stat="identity", color="white", fill = priceColor) + 
  geom_line( aes(x = year, y = anthropo_change*coeff), linewidth=2, color=temperatureColor) + 
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  #scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 5)) +
  #scale_y_continuous(labels = scales::percent) +
  scale_y_continuous(
    labels = scales::percent,
    # Features of the first axis
    name = "New TFC cases in literature",
    #limits = c(-0.001,0.013),
    n.breaks=6,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, 
                        name="Anthropogenic change",
                        labels = scales::percent
    )) +
  labs(x = NULL, y = NULL, color = "") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = priceColor, size=20),
    axis.title.y.right = element_text(color = temperatureColor, size=20) 
  )


#### Display together fisheries-----
### accumulated (mean forest + corpus size), baseline 1992

range(temp_analisys$anthropo_mean_0)
range(temp_analisys$prop_aggr_0)

ggplot(temp_analisys, aes(x = year)) + 
  geom_line(aes(y = fish_stock, colour = "Fisheries intensitication (% collapsed + overexploited fish stocks)"), size=1.5, linetype = "dashed") + 
  geom_line(aes(y = prop_aggr_0, colour = "Accumulated TFC cases in corpus"), size=1.5) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') 
# NOT VERY EASY TO PLOT

ggplot(temp_analisys, aes(x = year)) + 
  geom_line(aes(y = fish_stock_change, colour = "Change in fisheries intensitication (% collapsed + overexploited fish stocks)"), size=1.5, linetype = "dashed") + 
  geom_line(aes(y = prop_aggr_0, colour = "Accumulated TFC cases in corpus"), size=1.5) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') 

ggplot(temp_analisys, aes(x = year)) + 
  geom_line(aes(y = fish_stock_change, colour = "Change in fisheries intensitication (% collapsed + overexploited fish stocks)"), size=1.5, linetype = "dashed") + 
  geom_line(aes(y = prop_new_cases_0, colour = "New TFC cases in corpus"), size=1.5) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "", x = '') 

### change (fisheries + new TCF cases)
# range(temp_analisys$forest_change)
# range(temp_analisys$prop_new_cases)

temperatureColor <- "#EEBAB4"
priceColor <- "#7CA1CC"

coeff <- 100
ggplot(temp_analisys, aes(x=year)) +
  # Corpus is first axis (priceColor)
  geom_line( aes(y=prop_new_cases), linewidth=2, color=priceColor) +
  geom_line( aes(y=fish_stock/coeff), linewidth=2, color=temperatureColor) + 
  scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 5)) +
  scale_y_continuous(
    labels = scales::percent,
    # Features of the first axis
    name = "New TFC cases in literature",
    #limits = c(-0.001,0.013),
    n.breaks=6,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, 
                        name="Fishing intensity",
                        labels = scales::percent
    )) + 
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = 0.0001, label = "Baseline 1992", color = 'black', vjust = 0, hjust = 0)+
  labs(x = NULL) +
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = priceColor, size=20),
    axis.title.y.right = element_text(color = temperatureColor, size=20) 
  )


### Mix (mean forest + new TCF cases), baseline 1992

range(temp_analisys$forest_mean_0)
range(temp_analisys$prop_aggr_0)

ggplot(temp_analisys) + 
  geom_bar(aes(x = year, y = prop_new_cases, fill = y),
           stat="identity", color="white", fill = priceColor) + 
  geom_line( aes(x = year, y = fish_stock_change), linewidth=2, color=temperatureColor) + 
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  #scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 5)) +
  #scale_y_continuous(labels = scales::percent) +
  scale_y_continuous(
    labels = scales::percent,
    # Features of the first axis
    name = "New TFC cases in literature",
    #limits = c(-0.001,0.013),
    n.breaks=6,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., 
                        name="Fisheries change",
                        labels = scales::percent
    )) +
  labs(x = NULL, y = NULL, color = "") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = priceColor, size=20),
    axis.title.y.right = element_text(color = temperatureColor, size=20) 
  )

ggplot(temp_analisys) + 
  geom_bar(aes(x = year, y = prop_new_cases, fill = y),
           stat="identity", color="white", fill = priceColor) + 
  geom_line( aes(x = year, y = fish_stock/coeff), linewidth=2, color=temperatureColor) + 
  #geom_hline(yintercept=0, color="black", linetype=2) +
  #annotate("text", x = 1992, y = -0.00009, label = "Baseline 1992", color = 'black', vjust = 1, hjust = 0) +
  #scale_colour_grey() +
  scale_x_continuous(breaks = seq(from = 1992, to = 2022, by = 5)) +
  #scale_y_continuous(labels = scales::percent) +
  scale_y_continuous(
    labels = scales::percent,
    # Features of the first axis
    name = "New TFC cases in literature",
    #limits = c(-0.001,0.013),
    n.breaks=6,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, 
                        name="Fisheries intensity",
                        labels = scales::percent
    )) +
  labs(x = NULL, y = NULL, color = "") +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = priceColor, size=20),
    axis.title.y.right = element_text(color = temperatureColor, size=20) 
  )











###END----------------------
