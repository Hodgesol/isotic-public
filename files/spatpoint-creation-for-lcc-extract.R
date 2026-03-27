## Creating shapefile to extract land cover class for pedons of interest 
## (including rock fragments and sand fractions pedons)
## 1104 pedons
## R. Hodges, D. Beaudette
## 2025-Aug-29

## cleanup
rm(list = ls())
gc(reset = TRUE)

library(data.table)
library(terra)
library(dplyr)

library(lattice)
library(latticeExtra)
library(tactile)


## load cached data from previous step
# isotic + mixed mineralogy
# wt. mean lab data
# wide-format with coordinates
x <- readRDS('data/combined-lab-wt-mean-with-rf-and-sandfrac.rds')
head(x)



# convert to data.frame
x <- as.data.frame(x)

# init spatVector
x <- vect(x, geom = c('longitude_decimal_degrees', 'latitude_decimal_degrees'), crs = 'EPSG:4269')

# looks right
x


## land cover class data
# HI
base.path <- 'D:/desktop/GIS-hard/land_cover2/HI_landcover_wimperv_9-30-08_se5'
lcc.hi <- file.path(base.path, 'hi_landcover_wimperv_9-30-08_se5.img')
hi <- rast(lcc.hi)

# AK
base.path <- 'D:/desktop/GIS-hard/land_cover2/NLCD_2001_Land_Cover_AK_20200724'
lcc.ak <- file.path(base.path, 'NLCD_2001_Land_Cover_AK_20200724.img')
ak <- rast(lcc.ak)

# PR
base.path <- 'D:/desktop/GIS-hard/land_cover2/PR_landcover_wimperv_10-28-08_se5'
lcc.pr <- file.path(base.path, 'pr_landcover_wimperv_10-28-08_se5.img')
pr <- rast(lcc.pr)

# rest of US
base.path <- 'D:/desktop/GIS-hard/land_cover2/nlcd_2001_land_cover_l48_20210604'
lcc.conus <- file.path(base.path, 'nlcd_2001_land_cover_l48_20210604.img')
conus <- rast(lcc.conus)


# same CRS as points
e.hi <- extract(hi, x, ID = FALSE)

e.ak <- extract(ak, x, ID = FALSE)

e.pr <- extract(pr, x, ID = FALSE)

e.conus <- extract(conus, x, ID = FALSE)


# combine all extracted values as columns
# each raster is it's own column
e <- cbind(
  conus = as.character(e.conus$`NLCD Land Cover Class`),
  pr = as.character(e.pr$`Land Cover Class`),
  hi = as.character(e.hi$`Land Cover Class`),
  ak = as.character(e.ak$`NLCD Land Cover Class`)
)

# create a function that allows us to identify which variable (raster) has land 
# cover info (not NAs) by row (pedon)
pick.column <- function(i) {
  idx <- which(!is.na(i))
  if(length(idx) == 0) {
    return(NA)    
  }
  r <- i[idx]
  return(r)
}

# apply the function to the dataset
lcc.combo <- apply(e, MARGIN = 1, FUN = pick.column)

# combine the resulting compressed lcc variable, pedon key, and mineralogy class 
# assignment into new dataframe
peds.w.lcc <- data.frame(pedon_key = x$pedon_key, which = x$which, lcc = lcc.combo)



# save as RDS
saveRDS(peds.w.lcc, file = 'data/peds.w.lccdata.rds')
library(writexl)
write_xlsx(peds.w.lcc, "data/peds.w.lcc.xlsx")


# write it to view locations on map in arc
# base.path <- 'C:/Users/Ryan.Hodges/OneDrive - USDA/Desktop/isotic/clean-project-files-extendedwork/data/'
# writeVector(x, filename = file.path(base.path, "iso-project-points-model2.shp"), overwrite = TRUE)

# investigate
min_counts <- peds.w.lcc %>% 
  group_by(which) %>% 
  count(lcc)













