## Reclassifying pedons to isotic and mixed based on Pruned Regression Tree MODEL 2 Results

## R. Hodges and D. Beaudette
## July 9, 2025
## 

## cleanup
rm(list = ls())
gc(reset = TRUE)

library(dplyr)
library(tidyr)

# set wd
setwd("C:/Users/Ryan.Hodges/OneDrive - USDA/Desktop/isotic/clean-project-files-extendedwork")

# load data
allpeds.wcoords <- readRDS('data/combined-lab-wt-mean-with-rf-and-sandfrac.rds')
allpeds.wcoords <- allpeds.wcoords %>% 
  drop_na(longitude_decimal_degrees, latitude_decimal_degrees)
allpeds.wcoords$latitude_decimal_degrees <- as.numeric(allpeds.wcoords$latitude_decimal_degrees)
allpeds.wcoords$longitude_decimal_degrees <- as.numeric(allpeds.wcoords$longitude_decimal_degrees)


write.csv(allpeds.wcoords, file = 'data/allpeds-forUSmapcomparison-originalmin-priortomodel2.csv')

# load libraries
library(tidyr)
library(data.table)
library(terra)
library(tidyverse)
library(tactile)
library(lattice)
library(latticeExtra)
library(soiltexture)
library(aqp)

###############################################################################
### create new dataframe from training dataset based on pruned regression tree 2 criteria.
## Need to filter data through each funnel, creating 3 separate dataframes.

# from left to right
# "Terminal Nodes 1" 
tn1 <- allpeds.wcoords %>% 
  filter(silica_ammonium_oxalate < 0.12)
# tn1: isotic = 192, mixed = 555 
table(tn1$which)


# "Terminal Node 2"
tn2 <- allpeds.wcoords %>% 
  filter(silica_ammonium_oxalate >= 0.12) %>%
  filter(cec_nh4_ph_7 >= 20) %>%
  filter(new_zealand_phosphorus_retent < 60)
# tn2: isotic = 6, mixed = 50
table(tn2$which)


# "Terminal Node 3"
tn3 <- allpeds.wcoords %>% 
  filter(silica_ammonium_oxalate >= 0.12) %>% 
  filter(cec_nh4_ph_7 >= 20) %>% 
  filter(new_zealand_phosphorus_retent >= 60)
# tn3: isotic = 56, mixed = 21
table(tn3$which)

# "Terminal Node 4"
tn4 <- allpeds.wcoords %>% 
  filter(silica_ammonium_oxalate >= 0.12) %>% 
  filter(cec_nh4_ph_7 < 20)
# tn4: isotic = 177, mixed = 40
table(tn4$which)

###############################################################################
### now, assign new mineralogy class to pedons within each bucket dataset
# mixed
tn1$new_min_class <- "mixed"
tn2$new_min_class <- "mixed"

# isotic
tn3$new_min_class <- "isotic"
tn4$new_min_class <- "isotic"

###############################################################################
### combine dataset "terminal nodes" into as single dataframe
x <- rbind(tn1, tn2, tn3, tn4)

# isotic = 294, mixed = 803
table(x$new_min_class)
table(x$which)

# evaluate pedons and orders
s <- readRDS('data/SC.rds')

# remove inactive series
s <- subset(s, soilseriesstatus != 'inactive')

## get pedon data from LDM snapshot

s.series <- tolower(trimws(unique(s$soilseriesname)))
chunks <- makeChunks(s.series, 1000)

# split series name vector into pieces for chunk-wise fetching of data
.seriesList <- split(s.series, chunks)

# get a chunk-worth of lab pedon data
getLabPedons <- function(i) {
  .sql <- sprintf("
SELECT
pedon_key, corr_name, corr_taxorder
FROM
lab_combine_nasis_ncss
WHERE CASE WHEN corr_name IS NOT NULL THEN LOWER(corr_name) ELSE LOWER(samp_name) END IN %s;"
                  , format_SQL_in_statement(gsub("'", "", i)))
  
  .res <- SDA_query(.sql)
  return(.res)
}

# retrieve pedon lab data by chunk, result is a data.frame
# ~ 2 minutes
all.pedons <- map_df(.seriesList, .f = getLabPedons, .progress = TRUE)

saveRDS(all.pedons, file = 'data/all.pedons.taxorder.rds')
all.pedons <- readRDS('data/all.pedons.taxorder.rds')

x <- x %>% 
  mutate(pedon_key = as.integer(pedon_key))
tn1 <- tn1 %>% 
  mutate(pedon_key = as.integer(pedon_key))
tn2 <- tn2 %>% 
  mutate(pedon_key = as.integer(pedon_key))
tn3 <- tn3 %>% 
  mutate(pedon_key = as.integer(pedon_key))
tn4 <- tn4 %>% 
  mutate(pedon_key = as.integer(pedon_key))


x <- x %>% 
  left_join(select(all.pedons, pedon_key, corr_taxorder), by = "pedon_key")
tn1 <- tn1 %>% 
  left_join(select(all.pedons, pedon_key, corr_taxorder), by = "pedon_key")
tn2 <- tn2 %>% 
  left_join(select(all.pedons, pedon_key, corr_taxorder), by = "pedon_key")
tn3 <- tn3 %>% 
  left_join(select(all.pedons, pedon_key, corr_taxorder), by = "pedon_key")
tn4 <- tn4 %>% 
  left_join(select(all.pedons, pedon_key, corr_taxorder), by = "pedon_key")

## NOW, evaluate orders by terminal nodes
table(tn3$corr_taxorder, by = tn3$which)
table(tn4$corr_taxorder)

tn4.spods <- subset(tn4, corr_taxorder == 'spodosols')

#### save
# print
library(writexl)
write.csv(x, file = "data/theoretical-min-reclass-peds-USmapdataset-model2.csv")
saveRDS(x, file = "data/theoretical-min-reclass-peds-USmapdataset-model2.rds")



