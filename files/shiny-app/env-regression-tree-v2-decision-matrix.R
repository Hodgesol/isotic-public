## Decision matrix based on Pruned environmental Regression Tree version 2
## R. Hodges and D. Beaudette
## Nov 18, 2025
## 

## cleanup
rm(list = ls())
gc(reset = TRUE)

# set wd
# setwd("C:/Users/")




# load libraries
library(dplyr)
library(tidyr)
library(data.table)
library(terra)


############################
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
andic_reprojected_crs <- project(andic, target_crs, method = resample_method_andic)

# Now, resample this reprojected 'andic' layer onto the grid of 'p_projected'
andic_aligned <- resample(andic_reprojected_crs, p_projected, method = resample_method_andic)

# Check extents again - they should now match perfectly
print(andic_aligned)
print(p_projected)

# view plots
plot(andic_aligned)
plot(p_projected)


# Now stack
my_stack <- c(andic_aligned, p_projected)

print(my_stack)



###############################################################################
### create dataframe where new column indicates grid cell does or does not meet
### raster criteria to be isotic
set.seed(2016)

# Make sure you have your combined raster stack named 'my_stack' first:
# my_stack <- c(p_resampled, andic_cropped) # (assuming previous steps are done)

classify_decision_tree <- function(values) {
  # Assign the input values to variable names for readability
  andic <- values["andisols.and.andic.subgroups"]
  precip <- values["effective_precipitation_800m"]
  temp <- values["final_MAAT_800m"]
  
  if (is.na(andic) || is.na(precip) || is.na(temp)) {
    return(NA) # Handle cases where data is missing for any layer
  }
  
  # Decision tree rules translated into R logic:
  
  # Check Terminal Node 1 conditions ("no")
  if (andic < 3 && precip < 524) {
    return(0)
  }
  
  # Check Terminal Node 2 conditions ("no")
  if (andic < 3 && precip >= 524 && temp >= 9.7) {
    return(0)
  }
  
  # Check Terminal Node 3 conditions ("yes")
  if (andic < 3 && precip >= 524 && temp < 9.7) {
    return(1)
  }
  
  # Check Terminal Node 4 conditions ("no")
  # Note: The critical value for precip in TN4 (-66) seems very low compared to TN1/TN2 (524). 
  # Assuming your logic is correct as provided:
  if (andic >= 3 && precip < -66) {
    return(0)
  }
  
  # Check Terminal Node 5 conditions ("yes")
  if (andic >= 3 && precip >= -66) {
    return(1)
  }
  
  # Default return (should ideally not be hit if logic is complete)
  return(NA)
}

# Apply the function across all cells of the raster stack
# 'cores' can be adjusted for parallel processing if your raster is large
decision_raster <- app(my_stack, classify_decision_tree, cores = 1)

# Name the new layer
names(decision_raster) <- "decision_yes_no"

# Add the new decision layer back to the main stack
my_stack <- c(my_stack, decision_raster)

print(my_stack)

# write as RDS
saveRDS(my_stack, file = "data/isotic-env-raster-stack.rds")

# Using the terra package (recommended) to save as TIFF
writeRaster(my_stack, filename = "files/shiny-app/isotic-env-raster-stack.tif", overwrite = TRUE)


###############################################################################
# # decision matrix from the environmental tree file
# # "Terminal node 1" 
# tn1 <- my_stack %>% 
#   filter(andisols.and.andic.subgroups < 3) %>% 
#   filter(effective_precipitation_800m < 524),
# then my_stack$decision == "no"
# 
# # "Terminal node 2"
# tn2 <- my_stack %>% 
#   filter(andisols.and.andic.subgroups < 3) %>% 
#   filter(effective_precipitation_800m >= 524)  %>%
#   filter(final_MAAT_800m >= 9.7),
#   then my_stack$decision == "no"
# 
# # "Terminal node 3"
# tn3 <- my_stack %>% 
#   filter(andisols.and.andic.subgroups < 3) %>% 
#   filter(effective_precipitation_800m >= 524)  %>%
#   filter(final_MAAT_800m < 9.7),
# then my_stack$decision == "yes"
# 
# # "Terminal node 4"
# tn4 <- my_stack %>% 
#   filter(andisols.and.andic.subgroups >= 3) %>% 
#   filter(effective_precipitation_800m < -66),
# then my_stack$decision == "no"
# 
# # "Terminal node 5"
# tn5 <- my_stack %>% 
#   filter(andisols.and.andic.subgroups >= 3) %>% 
#   filter(effective_precipitation_800m >= -66),
# then my_stack$decision == "yes"
###############################################################################

## static plot
# plot(my_stack$decision_yes_no)

## Or an interactive plot
library(tmap)

# Set tmap mode to interactive (for RStudio Viewer pane exploration)
tmap_mode("view") 

# Create the map using tmap syntax
tm_shape(my_stack$decision_yes_no) +
  tm_raster(palette = c("red", "green"), # Assign colors explicitly
            labels = c("No", "Yes"),
            title = "Isotic Decision Outcome") +
  tm_basemap("Esri.WorldTopoMap") + # Optional: Adds an interactive background map
  tm_layout(main.title = "Decision Tree Suitability Map")

# Set tmap mode back to plot (for static image output)
# tmap_mode("plot") 















