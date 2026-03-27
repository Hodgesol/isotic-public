## Determine sand subclasses for sand, loamy sand, and sandy loam textures
## Ultimately use to compare general figures between raw data and those with/without sandy or sandy-skeletal pedons
## R. Hodges and D. Beaudette
## 2024-04-19

## cleanup
rm(list = ls())
gc(reset = TRUE)


library(data.table)
library(terra)

library(tactile)
library(lattice)
library(latticeExtra)
library(soiltexture)

x <- readRDS('data/results-with-rf-and-sandfrac.rds')

# texture triangle
# install.packages('soiltexture')
library(aqp)

# rename columns, required by textureTriangleSummary()
setnames(x, old = c('sand_total', 'silt_total', 'clay_total'), new = c('SAND', 'SILT', 'CLAY'))
x

textureTriangleSummary(x, cex = 0.5)

# test for bogus data for soil texture
x$sum <- rowSums(x[, c('SAND', 'SILT', 'CLAY')])
# > 5% deviation from 100%
idx <- which(abs(x$sum - 100) > 5)

# check errors: 8 pedons
x[idx, ]
# remove these
x <- x[-c(349, 350, 351, 353, 356, 357, 358, 359), ]

# texture classification for pedons
# ssc_to_texcl()
x$texture <- ssc_to_texcl(sand = x$SAND, clay = x$CLAY)

# texture modification for sandy soils
# add empty "tex_mod" column
x[, 'tex_mod'] = NA

## Sands ##
# coarse sand
x$tex_mod <- ifelse(x$texture == "s" & 
                    x$sand_coarse + x$sand_very_coarse >= 25 &
                    x$sand_medium < 50 &
                    x$sand_fine  < 50 &
                    x$sand_very_fine < 50,
                    "cos",
                    x$tex_mod)

# Sand
x$tex_mod <- ifelse(x$texture == "s" &
                    x$sand_very_coarse + x$sand_coarse + x$sand_medium >= 25 &
                    x$sand_very_coarse + x$sand_coarse < 25 &
                    x$sand_fine < 50 &
                    x$sand_very_fine < 50
                    |
                    x$texture == "s" &
                    x$sand_very_coarse + x$sand_coarse >= 25 &
                    x$sand_medium >= 50,
                    "s",
                    x$tex_mod)

# Fine Sand
x$tex_mod <- ifelse(x$texture == "s" &
                    x$sand_fine >= 50 &
                    x$sand_fine > x$sand_very_fine
                    |
                    x$texture == "s" &
                    x$sand_very_coarse + x$sand_coarse + x$sand_medium < 25 &
                    x$sand_very_fine < 50,
                    "fs",
                    x$tex_mod)


## Loamy Sands ##
# loamy coarse sand
x$tex_mod <- ifelse(x$texture == "ls" &
                    x$sand_very_coarse + x$sand_coarse >= 25 &
                    x$sand_medium < 50 &
                    x$sand_fine < 50 &
                    x$sand_very_fine < 50,
                    "lcos",
                    x$tex_mod)
  
# loamy sand
x$tex_mod <- ifelse(x$texture == "ls" &
                    x$sand_very_coarse + x$sand_coarse + x$sand_medium >= 25 &
                    x$sand_very_coarse + x$sand_coarse < 25 &
                    x$sand_fine < 50 &
                    x$sand_very_fine < 50
                    |
                    x$texture == "ls" &
                    x$sand_very_coarse + x$sand_coarse >= 25 &
                    x$sand_medium >= 50,
                    "ls",
                    x$tex_mod)

# loamy fine sand
x$tex_mod <- ifelse(x$texture == "ls" &
                    x$sand_fine >= 50 &
                    x$sand_very_coarse + x$sand_coarse + x$sand_medium < 25
                    |
                    x$texture == "ls" &
                    x$sand_very_fine < 50 &
                    x$sand_very_coarse + x$sand_coarse + x$sand_medium < 25,
                    "lfs",
                    x$tex_mod)

#loamy very fine sand
x$tex_mod <- ifelse(x$texture == "ls" &
                    x$sand_very_fine >= 50,
                    "lvfs",
                    x$tex_mod)


## Sandy Loams ##
#coarse sandy loam
x$tex_mod <- ifelse(x$texture == "sl" &
                    x$sand_very_coarse + x$sand_coarse >= 25 &
                    x$sand_medium < 50 &
                    x$sand_fine < 50 &
                    x$sand_very_fine < 50
                    |
                    x$texture == "sl" &
                    x$sand_very_coarse + x$sand_coarse + x$sand_medium >= 30 &
                    (x$sand_very_fine >= 30 &
                    x$sand_very_fine < 50),
                    "cosl",
                    x$tex_mod)

# sandy loam
x$tex_mod <- ifelse(x$texture == "sl" &
                    x$sand_very_coarse + x$sand_coarse + x$sand_medium >= 30 &
                    x$sand_very_coarse + x$sand_coarse < 25 &
                    x$sand_fine < 30 &
                    x$sand_very_fine < 30
                    |
                    x$texture == "sl" &
                    x$sand_very_coarse + x$sand_coarse + x$sand_medium < 15 &
                    x$sand_fine < 30 &
                    x$sand_very_fine < 30 &
                    x$sand_fine + x$sand_very_fine <= 40
                    |
                    x$texture == "sl" &
                    x$sand_very_coarse + x$sand_coarse > 25 &
                    x$sand_medium >= 50,
                    "sl",
                    x$tex_mod)

#fine sandy loam
x$tex_mod <- ifelse(x$texture == "sl" &
                    x$sand_fine >= 30 &
                    x$sand_very_fine < 30 &
                    x$sand_very_coarse + x$sand_coarse < 25
                    |
                    x$texture == "sl" &
                    (x$sand_very_coarse + x$sand_coarse + x$sand_medium >= 15 &
                     x$sand_very_coarse + x$sand_coarse + x$sand_medium < 30) &
                    x$sand_very_coarse + x$sand_coarse < 25
                    |
                    x$texture == "sl" &
                    x$sand_fine + x$sand_very_fine >= 40 &
                    x$sand_fine >= x$sand_very_fine &
                    x$sand_very_coarse + x$sand_coarse + x$sand_medium <= 15
                    |
                    x$sand_very_coarse + x$sand_coarse >= 25 &
                    x$sand_fine >= 50,
                    "fsl",
                    x$tex_mod)
    
# very fine sandy loam
x$tex_mod <- ifelse(x$texture == "sl" &
                    x$sand_very_fine >= 30 &
                    x$sand_very_coarse + x$sand_coarse + x$sand_medium < 15
                    |
                    x$texture == "sl" &
                    x$sand_fine + x$sand_very_fine >= 40 &
                    x$sand_very_fine > x$sand_fine &
                    x$sand_very_coarse + x$sand_coarse + x$sand_medium < 15
                    |
                    x$texture == "sl" &
                    x$sand_very_fine >= 50 &
                    x$sand_very_coarse + x$sand_coarse >= 25
                    |
                    x$texture == "sl" &
                    x$sand_very_coarse + x$sand_coarse + x$sand_medium >= 30 &
                    x$sand_very_fine >= 50,
                    "vfsl",
                    x$tex_mod)

saveRDS(x, file = "data/pedons-with-s-ls-sl-subclasses.rds")

## cleanup
rm(list = ls())
gc(reset = TRUE)










