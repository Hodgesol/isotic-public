## Spatial overlay with data that includes rock fragments and sand fractions
## R. Hodges, D. Beaudette
## 2024-04-19

library(data.table)
library(terra)

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

# write it to view locations on map in arc
base.path <- 'D:/desktop/GIS-hard/Climate/MODIS-NPP/'
writeVector(x, filename = file.path(base.path, "iso-project-points.shp"), overwrite = TRUE)


## gridded data
base.path <- 'D:/desktop/GIS-hard/Climate/prism/prism-annual/'
f <- file.path(base.path, c('effective_precipitation_800m.tif', 'final_MAP_mm_800m.tif', 'final_MAAT_800m.tif'))
p <- rast(f)

# looks correct
p


# ET data
base.path <- 'D:/desktop/GIS-hard/Climate/ET/'
f <- file.path(base.path, 'et_rast.tif')
et <- rast(f)

# looks right
et

# npp data
base.path <- 'D:/desktop/GIS-hard/Climate/MODIS-NPP/'
f <- file.path(base.path, 'npp_mean.tif')
npp <- rast(f)


# ... other gridded data
# andic raster data
base.path <- 'D:/desktop/GIS-hard/Climate/'
a <- file.path(base.path, 'andic-soils-raster.tif')
andic <- rast(a)
names(andic) <- "andisols.and.andic.subgroups"

# land use
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


## overlay
# row-order is preserved

# PRISM stack
# same CRS as points
e.prism <- extract(p, x, ID = FALSE)

# ET data
# different CRS: temporarily transform points to CRS of gridded data
e.et <- extract(et, project(x, et), ID = FALSE)

# NPP data
# different CRS: temporarily transform points to CRS of gridded data
e.npp <- extract(npp, project(x, npp), ID = FALSE)

# andic data
# different CRS: temporarily transform points to CRS of gridded data
e.andic <- extract(andic, project(x, andic), ID = FALSE)

#### Land cover class data
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
  e.prism,
  e.et,
  e.npp,
  e.andic,
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

# remove variables lcc.conus, lcc.ak, lcc.hi, lcc.pr
z <- subset(z, select = -c(lcc.conus, lcc.ak, lcc.hi, lcc.pr))

# adjustments
# turn NAs to zeros
z$andisols.and.andic.subgroups[which(is.na(z$andisols.and.andic.subgroups))] <- 0

# isotic = 417, mixed = 646 -->
# isotic = 431, mixed = 673
sum(z$which == "isotic")
sum(z$which == "mixed")

# where is the missing data

# no missing soil property data
sum(rowSums(is.na(z[, 4:8])))

# annual NPP missing = 123 --> 135
sum(is.na(z$annualNPP))
# same for the QC
# sum(is.na(z$QC))

# precip obs missing = 38 --> 42
sum(is.na(z$effective_precipitation_800m))
# same for other climate
# sum(is.na(z$final_MAP_mm_800m))
# sum(is.na(z$final_MAAT_800m))

# ET obs missing = 37 --> 41
sum(is.na(z$et_rast))

# land class missing = 18 -> 19
sum(is.na(z$lcc.us))

## graphical check

# PRISM
bwplot(which ~ effective_precipitation_800m, data = z, par.settings = tactile.theme())
bwplot(which ~ final_MAP_mm_800m, data = z, par.settings = tactile.theme())
bwplot(which ~ final_MAAT_800m, data = z, par.settings = tactile.theme())

# ET
bwplot(which ~ et_rast, data = z, par.settings = tactile.theme())

# NPP
bwplot(which ~ annualNPP, data = z, par.settings = tactile.theme())

# with lab data
xyplot(new_zealand_phosphorus_retent ~ effective_precipitation_800m, data = z, groups = which, type = c('p', 'g', 'smooth'), auto.key = TRUE)
xyplot(new_zealand_phosphorus_retent ~ final_MAP_mm_800m, data = z, groups = which, type = c('p', 'g', 'smooth'), auto.key = TRUE)
xyplot(new_zealand_phosphorus_retent ~ final_MAAT_800m, data = z, groups = which, type = c('p', 'g', 'smooth'), auto.key = TRUE)
xyplot(new_zealand_phosphorus_retent ~ et_rast, data = z, groups = which, type = c('p', 'g', 'smooth'), auto.key = TRUE)
xyplot(new_zealand_phosphorus_retent ~ annualNPP, data = z, groups = which, type = c('p', 'g', 'smooth'), auto.key = TRUE)

xyplot(ph_naf ~ effective_precipitation_800m, data = z, groups = which, type = c('p', 'g', 'smooth'), auto.key = TRUE)
xyplot(ph_naf ~ annualNPP, data = z, groups = which, type = c('p', 'g', 'smooth'), auto.key = TRUE)
xyplot(ph_naf ~ et_rast, data = z, groups = which, type = c('p', 'g', 'smooth'), auto.key = TRUE)


## save
saveRDS(z, file = 'data/results-with-rf-and-sandfrac.rds')


## for later
library(Hmisc)

vars <- c('ph_naf', 'water_15_bar_to_clay_ratio', 'new_zealand_phosphorus_retent', 'aluminum_ammonium_oxalate', 'silica_ammonium_oxalate', 'effective_precipitation_800m', 'final_MAP_mm_800m', 'final_MAAT_800m', 'et_rast', 'annualNPP')

vc <- varclus(as.matrix(z[, vars]), similarity = 'spearman', method = 'complete')
plot(vc)



##########################################################
##########################################################

# removing observations with NA climate data

# went from 1063 obs to 572 --> 590
z.nona <- na.omit(z)

# isotic count = 263 --> 273, mixed count = 309 --> 317
sum(z.nona$which == "isotic")
sum(z.nona$which == "mixed")

# graphical check... looks similar to full dataset z
bwplot(which ~ final_MAP_mm_800m, data = z.nona, par.settings = tactile.theme())
bwplot(which ~ new_zealand_phosphorus_retent, data = z.nona, par.settings = tactile.theme(),
       main = "0-75 cm wt. mean", xlab = "NZ-P (%)")
bwplot(which ~ aluminum_ammonium_oxalate, data = z.nona, par.settings = tactile.theme(),
       main = "0-75 cm wt. mean", xlab = "Al (AO; %)")
bwplot(which ~ ph_naf, data = z.nona, par.settings = tactile.theme(),
       main = "0-75 cm wt. mean", xlab = "pH (NaF)")
bwplot(which ~ aluminum_plus_half_iron_oxalat, data = z.nona, par.settings = tactile.theme(),
       main = "0-75 cm wt. mean", xlab = "Al + 1/2Fe (AO)")

# save rds

saveRDS(z.nona, file = "data/final_pedon_climate_data-with-rf-and-sandfrac.rds")














