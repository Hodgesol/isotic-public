## extracting land cover class data for soil property model 2 pedons
## Done to observe lcc's for pedons pedons that reclassified based on model 2 results

## Ryan Hodges and Dylan Beaudette
## Sept 12, 2025
##

## cleanup
rm(list = ls())
gc(reset = TRUE)

library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(terra)
library(lattice)
library(latticeExtra)
library(tactile)

# set wd
setwd("C:/Users/Ryan.Hodges/OneDrive - USDA/Desktop/isotic/clean-project-files-extendedwork")

# load data
x <- readRDS("data/theoretical-min-reclass-peds-USmapdataset-model2.rds")

# convert to data.frame
x <- as.data.frame(x)

# init spatVector
x <- vect(x, geom = c('longitude_decimal_degrees', 'latitude_decimal_degrees'), crs = 'EPSG:4269')

# looks right
x



# land cover class data source paths
# CONUS
base.path <- 'D:/desktop/GIS-hard/land_cover/'
lc_c <- file.path(base.path, 'CONUS land cover.tif')
lc_conus <- rast(lc_c)
names(lc_conus) <- "lcc.conus"

# Alaska
base.path <- 'D:/desktop/GIS-hard/land_cover/'
ak <- file.path(base.path, 'lc_ak.tif')
lc_ak <- rast(ak)
names(lc_ak) <- "lcc.ak"

# Hawaii
base.path <- 'D:/desktop/GIS-hard/land_cover/'
hi <- file.path(base.path, 'lc_hi.tif')
lc_hi <- rast(hi)
names(lc_hi) <- "lcc.hi"

# Puerto Rico
base.path <- 'D:/desktop/GIS-hard/land_cover/'
pr <- file.path(base.path, 'lc_pr.tif')
lc_pr <- rast(pr)
names(lc_pr) <- "lcc.pr"



#### Land cover class data extract
# different CRS: temporarily transform points to CRS of gridded data
# CONUS
e.lcc.conus <- extract(lc_conus, project(x, lc_conus), ID = FALSE)
# PR
e.lcc.pr <- extract(lc_pr, project(x, lc_pr), ID = FALSE)
# HI
e.lcc.hi <- extract(lc_hi, project(x, lc_hi), ID = FALSE)
# AK
e.lcc.ak <- extract(lc_ak, project(x, lc_ak), ID = FALSE)


# combine all extracted values as columns
e <- cbind(
  e.lcc.conus,
  e.lcc.pr,
  e.lcc.hi,
  e.lcc.ak
)


## re-combine with spatial data
z <- cbind(x, e)
# down-grade for statistical analysis
z <- as.data.frame(z)

## combine lcc variables into a single variable by max value for each unique pedon
z$lcc.conus <- as.numeric(z$lcc.conus)
z$lcc.ak <- as.numeric(z$lcc.ak)
z$lcc.hi <- as.numeric(z$lcc.hi)
z$lcc.pr <- as.numeric(z$lcc.pr)

z$lcc.us <- pmax(z$lcc.conus, z$lcc.ak, z$lcc.hi, z$lcc.pr, na.rm = TRUE)


## summarize reclassification data and lcc
# pedons which went from isotic to mixed with lcc data
subset_z <- z %>% 
  filter(which == "isotic", z$new_min_class == "mixed")
table(subset_z$lcc.us)

# pedons which went from mixed to isotic with lcc data
subset_z2 <- z %>% 
  filter(which == "mixed", z$new_min_class == "isotic")
table(subset_z2$lcc.us)



table(x$new_min_class, by = z$lcc.us)



















