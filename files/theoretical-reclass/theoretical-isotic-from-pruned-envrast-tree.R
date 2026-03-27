## Reclassifying pedons to isotic and mixed based on Pruned environmental raster
## Regression Tree MODEL 2 Results

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
x <- readRDS('data/results-with-rf-and-sandfrac.rds')

# load libraries
library(tidyr)
library(data.table)
library(terra)
library(tidyverse)

# variables reminder
# v <- c('effective_precipitation_800m', 'final_MAAT_800m', 
#       'andisols.and.andic.subgroups')



###############################################################################
### create new dataframe from training dataset based on pruned regression tree 2 criteria.
## Need to filter data through each funnel, creating 3 separate dataframes.

# from left to right
# "terminal node 1" 
tn1 <- x %>% 
  filter(andisols.and.andic.subgroups < 3) %>% 
  filter(effective_precipitation_800m < 524)
# tn1: isotic = 79, mixed = 375 
table(tn1$which)


# "terminal node 2"
tn2 <- x %>% 
  filter(andisols.and.andic.subgroups < 3) %>% 
  filter(effective_precipitation_800m >= 524) %>% 
  filter(final_MAAT_800m >= 9.7)
# tn2: isotic = 28, mixed = 122
table(tn2$which)


# "terminal node 3"
tn3 <- x %>% 
  filter(andisols.and.andic.subgroups < 3) %>% 
  filter(effective_precipitation_800m >= 524) %>% 
  filter(final_MAAT_800m < 9.7)
# tn3: isotic = 75, mixed = 42
table(tn3$which)


# "terminal node 4"
tn4 <- x %>% 
  filter(andisols.and.andic.subgroups >= 3) %>% 
  filter(effective_precipitation_800m < -66)
# tn4: isotic = 7, mixed = 20
table(tn4$which)


# "terminal node 5"
tn5 <- x %>% 
  filter(andisols.and.andic.subgroups >= 3) %>% 
  filter(effective_precipitation_800m >= -66)
# tn5: isotic = 240, mixed = 74
table(tn5$which)

###############################################################################
### now, assign new mineralogy class to pedons within each terminal node
# mixed
tn1$new_min_class <- "mixed"
tn2$new_min_class <- "mixed"
tn3$new_min_class <- "mixed"

# isotic
tn4$new_min_class <- "isotic"
tn5$new_min_class <- "isotic"

###############################################################################
### combine dataset "terminal nodes" into as single dataframe
x <- rbind(tn1, tn2, tn3, tn4, tn5)

# isotic = 341, mixed = 721
table(x$new_min_class)
table(x$which)


#### save
# print
library(writexl)
write.csv(x, file = "data/theoretical-min-reclass-peds-USmapdataset-model2.csv")
saveRDS(x, file = "data/theoretical-min-reclass-peds-USmapdataset-model2.rds")

