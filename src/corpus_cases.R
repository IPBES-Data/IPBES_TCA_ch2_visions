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

### Load anthromes change data-----
ant_change_dir = 'C:/Users/yanis/Documents/IPBES/human_modification_indic/anthrome_change/fraccover_classbased_100/fraccover_class_100/'

# Load ocean mask
mask_ocean = terra::rast('C:/Users/yanis/Documents/IPBES/nexus_indicators/all_harmonized/F_008_Monfreda_prod_sum_allcrops_harm.tif')
m <- c(0.0, 0.0, 1, 0.0, 827665.2, 1) #min max values
rclmat <- matrix(m, ncol=3, byrow=TRUE)
mask_ocean <- terra::classify(mask_ocean, rclmat)
reclass_matrix <- rbind(c(0, 1))
mask_ocean <- terra::classify(mask_ocean, reclass_matrix)
terra::plot(mask_ocean)

mask_ocean_p = terra::project(mask_ocean, urban_change[[1]],
                              method='near',
                              align=FALSE,
                              threads=TRUE)



# Class 190: Urban

urban_change <- lapply(as.list
                   (list.files(paste0(ant_change_dir, 'fraccover_190_100/'),full.names = T, pattern = '.tif$')
                   ), 
                   terra::rast)
urban_change_layer = as.character(seq(from = 1992, to = 2018, by = 1))

#terra::plot(cl190_change[[1]])

# calculate global mean
LCchange1 = data.frame()       
for (i in 1:length(urban_change)){
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


# Class 10-20-30-40: Agriculature

cl10_change <- lapply(as.list
                       (list.files(paste0(ant_change_dir, 'fraccover_10_100/'),full.names = T, pattern = '.tif$')
                       ), 
                       terra::rast)
cl20_change <- lapply(as.list
                      (list.files(paste0(ant_change_dir, 'fraccover_20-100/'),full.names = T, pattern = '.tif$')
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
change_layer = as.character(seq(from = 1992, to = 2018, by = 1))

# add percentages of different agricultural classes
agri = list()
for (j in 1:length(cl10_change)){
  print(j)
  agri[j] = as.list(sum(cl10_change[[j]],cl20_change[[j]],cl30_change[[j]],cl40_change[[j]]))
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
for (j in 1:length(urban_change)){
  print(j)
  anthropo[j] = as.list(sum(urban_change[[j]], cl10_change[[j]],cl20_change[[j]],cl30_change[[j]],cl40_change[[j]]))
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
anthropogenic_change = LCchange1 %>% 
  inner_join(LCchange2, by = 'year') %>% 
  inner_join(LCchange3, by = 'year') 
  
# Calculate change
range(anthropogenic_change$anthropo_mean)
anthropogenic_change = anthropogenic_change %>% 
  mutate(anthropo_mean_lag = lag(anthropo_mean, default = first(anthropo_mean)),
         change = anthropo_mean - anthropo_mean_lag) %>% 
  mutate(year = as.integer(year))

### Load TFC cases from Corpus-----

oa = readRDS(paste0(dir_git, 'data/corpus_cases/oa_count.rds'))
corpus_cases = oa$tca_case_years

# calculate total cases from 1992 to 2018 calculate relative new cases
total_cases = corpus_cases %>% 
  filter(publication_year >= 1992 & publication_year <= 2018) %>% 
  mutate(total_pub = sum(count)) %>%  
  distinct(total_pub)

corpus_cases = corpus_cases %>% 
  filter(publication_year >= 1992 & publication_year <= 2018) %>% 
  mutate(prop_count = count/total_cases$total_pub) %>% 
  dplyr::select(-p, -p_cum)

### Join land cover change with TFC cases----
temp_analisys = inner_join(anthropogenic_change, corpus_cases, by = c('year' = 'publication_year'))
names(temp_analisys)


# Plot
# Separate line charts
p1 <- ggplot(temp_analisys, aes(x=year, y=change)) +
  geom_line(color="#69b3a2", size=2) +
  ggtitle("Anthropization \n(Prop of change in urban and agricultural areas) ") +
  theme_ipsum()

p2 <- ggplot(temp_analisys, aes(x=year, y=prop_count)) +
  geom_line(color="grey",size=2) +
  ggtitle("TFC cases \n(Prop of new TFC cases in literature)") +
  theme_ipsum()
p1 + p2

range(temp_analisys$change)
range(temp_analisys$prop_count)

## Display both charts together

ggplot(temp_analisys, aes(x = year)) + 
  geom_line(aes(y = change, colour = "var0")) + 
  geom_line(aes(y = prop_count, colour = "var1")) +
  ylim(-0.03,0.11) +
  geom_hline(yintercept=0, color="blue", size=.5, alpha=0.9, linetype=2) +
  theme_ipsum()

# Add second axis

# Adding a second Y axis with sec.axis(): the idea
# sec.axis() does not allow to build an entirely new Y axis. It just builds a second Y axis 
# based on the first one, applying a mathematical transformation.

# Start with a usual ggplot2 call:

# land cover change
ggplot(temp_analisys, aes(x=year, y=change)) +
  
  # Custom the Y scales:
  scale_y_continuous(
    
    # Features of the first axis
    name = "Prop of change in urban and agricultural areas",
    limits = c(-0.03,0.11),
      
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*1, name="Prop of new TFC cases in literature")
  ) +
  
  theme_ipsum()

#Show 2 series on the same line chart thanks to sec.axis()
# We can use this sec.axis mathematical transformation to display 2 series that have a different range.
# ipsum theme to remove the black background and improve the general style, add a title, customize the Y axes to pair them with their related line.

# Value used to transform the data
coeff <- 1

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(temp_analisys, aes(x=year)) +
  
  geom_line( aes(y=change), size=2, color=temperatureColor) + 
  geom_line( aes(y=prop_count / coeff), size=2, color=priceColor) +
  scale_x_continuous(breaks = seq(from = 1992, to = 2018, by = 2)) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Prop of change in urban and agricultural areas",
    limits = c(-0.04,0.12),
    n.breaks=6,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Prop of new TFC cases in literature")
  ) + 
  geom_hline(yintercept=0, color="black", linetype=2) +
  annotate("text", x = 1992, y = -0.005, label = "Baseline 1992", color = 'black')+
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=20),
    axis.title.y.right = element_text(color = priceColor, size=20) 
  )



