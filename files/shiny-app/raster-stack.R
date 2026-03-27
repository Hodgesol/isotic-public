## Pruned environmental raster model for shiny app
## R. Hodges and D. Beaudette
## Dec 4, 2025
## 

## cleanup
rm(list = ls())
gc(reset = TRUE)

# libraries
library(terra)
library(rpart)

setwd("C:/Users/Ryan.Hodges/OneDrive - USDA/Desktop/isotic/clean-project-files-extendedwork")

# load model
x <- readRDS(file = "models/pruned-env-tree.rds")

## load gridded environmental rasters
## PRISM data: effective precip & MAAT
base.path <- 'D:/desktop/GIS-hard/Climate/prism/prism-annual/'
f <- file.path(base.path, c('effective_precipitation_800m.tif', 'final_MAAT_800m.tif'))
p <- rast(f)

# andic raster data
base.path <- 'C:/Users/Ryan.Hodges/OneDrive - USDA/Desktop/GIS-desk/Isotic/Isotic_1/'
a <- file.path(base.path, 'andic2effprecipextent.tif')
andic <- rast(a)
names(andic) <- "andisols.and.andic.subgroups"

# check data, crs', res., extents
print(andic)
print(p)

# view
plot(andic)
plot(p)


# --- Convert NAs of andic raster to Zeros ---

# Method 1: Using classify (Recommended for clarity)
# Define a reclassification matrix: NA becomes 0
# The matrix format is: [from value, to value, new value] or for NA: [NA, NA, new value]
reclass_matrix <- matrix(c(NA, 0), ncol = 2)

andic_no_na <- classify(andic, reclass_matrix, include.lowest=TRUE)


# Method 2: Using subst (Shorter syntax)
# This function substitutes specific values with new ones.
# subst(x, from, to)
# andic_no_na <- subst(andic, NA, 0) 


# Optional: Overwrite the original variable if you don't need the NA version
andic <- andic_no_na

# Verify that NAs have been replaced by 0s
print(andic)
plot(andic)

# 1. Define the target CRS (EPSG:6350) and resolution (~800m)
target_crs <- "EPSG:6350" 
target_res <- 800 # Resolution in meters

# Define resampling methods appropriate for data types
# 'p' is continuous climate data -> 'bilinear' or 'average'
resample_method_p <- "bilinear" 
# 'andic' might be integer/categorical soil data? Use 'near' if so, otherwise 'bilinear'
resample_method_andic <- "near" 


# 2. Project the 'p' raster first. This object will become our spatial template.
# The 'project' function is used to transform to the new CRS and set the target resolution.
p_projected <- project(p, target_crs, res = target_res, method = resample_method_p)


# 3. Project 'andic' to the target CRS, but then ALIGN IT EXACTLY to 'p_projected'
# Use the resample() function, specifying p_projected as the 'template' raster.
# This ensures that 'andic_projected' has the exact same origin, extent, and dimensions 
# as 'p_projected' [1, 2].

# First, ensure 'andic' is in the target CRS (if not already)
# andic_reprojected_crs <- project(andic, target_crs, method = resample_method_andic)

# Now, resample this reprojected 'andic' layer onto the grid of 'p_projected'
andic_aligned <- resample(andic, p_projected, method = resample_method_andic)

# Check extents again - they should now match perfectly
print(andic_aligned)
print(p_projected)

# view plots
plot(andic_aligned)
plot(p_projected)


# Now stack
my_stack <- c(andic_aligned, p_projected)

print(my_stack)

# write RDS
saveRDS(my_stack, file = "files/shiny-app/env-raster-conus-stack.rds")

# layer names: "andisols.and.andic.subgroups" "effective_precipitation_800m" "final_MAAT_800m"
# must match model variable names
print(x)
# yes

# predict. Don't technically need to set.seed
p <- predict(my_stack, model = x, na.rm = TRUE, type = "prob")
plot(p)
plot(p$isotic)

# reproject to WGS: 3857 for leaflet
p.4837 <- project(p$isotic, 'EPSG:3857', method = "bilinear")
plot(p.4837)

# save. simplify for visuals
p.rounded <- round(p.4837 * 100)
plot(p.rounded)

writeRaster(p.rounded, datatype = "INT1U", filename = "models/isotic-p-round.tif")


