## Determine particle size class for sandy and sandy-skeletal pedons only
## R. Hodges and D. Beaudette
## 2024-04-24


library(data.table)
library(terra)
library(tactile)
library(lattice)
library(latticeExtra)
library(soiltexture)
library(aqp)

x <- readRDS('data/pedons-with-s-ls-sl-subclasses.rds')

# add rock fragment classes for total percent
x$rf_total <- x$wf_25 + x$wf_520 + x$wf_2075

# Determine sandy and sandy-skeletal particle-size class for relevant pedons
# NA for others
# 
# SANDY particle size class
x$ps_class <- ifelse((x$tex_mod == "cos" | 
                     x$tex_mod == "s" |
                     x$tex_mod == "fs" |
                     x$tex_mod == "lcos" |
                     x$tex_mod == "ls" |
                     x$tex_mod == "lfs") &
                     x$rf_total < 35,
                     "Sandy",
                     NA)

# SANDY-SKELETAL particle size class
x$ps_class <- ifelse((x$tex_mod == "cos" | 
                      x$tex_mod == "s" |
                      x$tex_mod == "fs" |
                      x$tex_mod == "lcos" |
                      x$tex_mod == "ls" |
                      x$tex_mod == "lfs") &
                      x$rf_total >= 35,
                     "Sandy-Skeletal",
                     x$ps_class)

saveRDS(x, file = "data/pedons-with-sandy-and-skeletal-psc.rds")                     

## cleanup
rm(list = ls())
gc(reset = TRUE)







