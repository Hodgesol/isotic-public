## R. Hodges & D. Beaudette
## reproject and save raster

library(terra)

# load raster
r <- rast("isotic-p-round.tif")

# project
r_wgs84 <- project(r, "EPSG:4326", method = "near")

# write optimized raster
writeRaster(
  r_wgs84,
  "isotic-p-round-4326.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW", "TILED+YES")
)
