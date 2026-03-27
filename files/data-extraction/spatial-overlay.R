## Overlay spatial data and extract by point with pedon observations
## R. Hodges and D. Beaudette
## July 28, 2025
##

library(data.table)
library(terra)

library(lattice)
library(latticeExtra)
library(tactile)


## load cached data from previous step
# isotic + mixed mineralogy
# wt. mean lab data
# wide-format with coordinates
x <- readRDS('data/combined-lab-wt-mean.rds')
head(x)

# convert to data.frame
x <- as.data.frame(x)

# init spatVector
x <- vect(x, geom = c('longitude_decimal_degrees', 'latitude_decimal_degrees'), crs = 'EPSG:4269')

# looks right
x

# write it to view locations on map in arc
base.path <- 'D:/GIS/Climate/MODIS-NPP/'
writeVector(x, filename = file.path(base.path, "iso-project-points.shp"), overwrite = TRUE)


## gridded data
base.path <- 'D:/GIS/Climate/prism/prism-annual/'
f <- file.path(base.path, c('effective_precipitation_800m.tif', 'final_MAP_mm_800m.tif', 'final_MAAT_800m.tif'))
p <- rast(f)

# looks correct
p


# ET data
base.path <- 'D:/GIS/Climate/ET/'
f <- file.path(base.path, 'et_rast.tif')
et <- rast(f)

# looks right
et

# npp data
base.path <- 'D:/GIS/Climate/MODIS-NPP/'
f <- file.path(base.path, 'npp_mean.tif')
npp <- rast(f)


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

# combine all extracted values as columns
e <- cbind(
  e.prism,
  e.et,
  e.npp
)


## re-combine with spatial data
z <- cbind(x, e)
# down-grade for statistical analysis
# total observations (pedons) = 1215
# isotic = 466, mixed = 749
z <- as.data.frame(z)

sum(z$which == "isotic")
sum(z$which == "mixed")

# where is the missing data

# no missing soil property data
sum(rowSums(is.na(z[, 4:8])))

# annual NPP missing = 141
sum(is.na(z$annualNPP))
# same for the QC
sum(is.na(z$QC))

# precip obs missing = 44
sum(is.na(z$effective_precipitation_800m))
# same for other climate
sum(is.na(z$final_MAP_mm_800m))
sum(is.na(z$final_MAAT_800m))

# ET obs missing = 43
sum(is.na(z$et_rast))


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
saveRDS(z, file = 'data/results.rds')


## for later
library(Hmisc)

vars <- c('ph_naf', 'water_15_bar_to_clay_ratio', 'new_zealand_phosphorus_retent', 'aluminum_ammonium_oxalate', 'silica_ammonium_oxalate', 'effective_precipitation_800m', 'final_MAP_mm_800m', 'final_MAAT_800m', 'et_rast', 'annualNPP')

vc <- varclus(as.matrix(z[, vars]), similarity = 'spearman', method = 'complete')
plot(vc)



##########################################################
##########################################################

# removing observations with NA climate data

# went from 1215 obs to 676
z.nona <- na.omit(z)

# find unrealistic obs with really high Al (AO) values
z.sub <- z.nona[z.nona$aluminum_ammonium_oxalate > 500, ]
z.sub
# pedon_keys 30680, 30681, 30683
# remove them
library(dplyr)
z.nona2 <- z.nona %>%
              filter(!pedon_key %in% c(30680, 30681, 30683))

# isotic count = 305, mixed count = 368
sum(z.nona2$which == "isotic")
sum(z.nona2$which == "mixed")

# graphical check... looks similar to full dataset z
bwplot(which ~ final_MAP_mm_800m, data = z.nona2, par.settings = tactile.theme())
bwplot(which ~ new_zealand_phosphorus_retent, data = z.nona2, par.settings = tactile.theme(),
                main = "0-75 cm wt. mean", xlab = "NZ-P (%)")
bwplot(which ~ aluminum_ammonium_oxalate, data = z.nona2, par.settings = tactile.theme(),
                main = "0-75 cm wt. mean", xlab = "Al (AO; %)")
bwplot(which ~ ph_naf, data = z.nona2, par.settings = tactile.theme(),
       main = "0-75 cm wt. mean", xlab = "pH (NaF)")


# save rds

saveRDS(z.nona2, file = "data/final_pedon_climate_data.rds")














