library(aqp)
library(lattice)
library(tactile)
library(data.table)


## isotic soils
iso <- readRDS('data/isotic_pedons_properties_rf_sand.rds')

## mixed soils
mx <- readRDS('data/mixed_pedons_properties_with_rf_and_sandfrac.rds')

## check for overlapping pedon_keys
u.iso <- unique(iso$pedon_key)
u.mx <- unique(mx$pedon_key)


## TODO: this is probably caused by "isotic over mixed" and similar mineralogy classes
##       ---> check and fix in previous steps

# !!! 15 pedons occur in both sets
idx <- which(u.iso %in% u.mx)
length(idx)
head(iso[which(iso$pedon_key %in% u.iso[idx]), ])

# for now, remove from iso
iso <- iso[which(!iso$pedon_key %in% u.iso[idx]), ]


## combine
x <- make.groups(isotic = iso, mixed = mx)


## init SoilProfileCollection
depths(x) <- pedon_key ~ hzn_top + hzn_bot
hzdesgnname(x) <- 'hzn_desgn'

# move site-level data
site(x) <- ~ pedlabsampnum + pedoniid + upedonid + longitude_decimal_degrees + 
  latitude_decimal_degrees + samp_name + corr_name + which


## check horizon depth logic
# ~ 2130 --> 4403 affected profiles
v <- checkHzDepthLogic(x)
head(v)
table(v$valid)

## attempt fixing old-style O horizon depths
x <- accumulateDepths(x)

## truncate at 200cm in case there are wacky profiles
# preliminary investigation demonstrated that there were
x <- trunc(x, 0, 200)

## fill horizon gaps
# even those at the "top" of a profile
# this can be useful for creating conformal profiles
# probably not essential here
x <- fillHzGaps(x, to_top = 0)

# this will create horizons with no data
# give bogus horizon names that we can filter out later
x$hzn_desgn[x$.filledGap] <- 'Z'

# how many horizons needed to be added in order to fill gaps?
table(x$.filledGap)


## re-check horizon logic
# ~ # affected profiles
v <- checkHzDepthLogic(x)


# inspect various types of errors

# FALSE  TRUE 
# 203 15816
table(v$valid)

# FALSE  TRUE 
# 15921    98
table(v$depthLogic)

# FALSE  TRUE 
# 15881   138 
table(v$sameDepth)

# FALSE 
# 16019 
table(v$missingDepth)

# FALSE  TRUE 
# 15921    98
table(v$overlapOrGap)


## filter out pedons with depth logic errors
x <- HzDepthLogicSubset(x)

# check on removed profiles: 
# metadata(x)$removed.profiles


## set aqp plotSPC() options
# less typing later
# don't forget to reset this later
.args <- list(width = 0.38, name.style = 'center-center', cex.names = 0.8, print.id = FALSE)
options(.aqp.plotSPC.args = .args)

par(mar = c(0, 0, 3, 2))

# check data availability on a random sampling of profiles
.idx <- sample(1:length(x), size = 20)

# note that "Z" horizons are fake horizons, added to create conformal profiles
# we ignore them later
plotSPC(x[.idx, ], color = 'ph_naf')
plotSPC(x[.idx, ], color = 'water_15_bar_to_clay_ratio')
plotSPC(x[.idx, ], color = 'new_zealand_phosphorus_retent')
plotSPC(x[.idx, ], color = 'aluminum_na_pyro_phosphate')


## figures related to data sparsity
set.seed(10101)

# check data availability on a random sampling of profiles
.idx <- sample(1:length(x), size = 12)

.args <- list(width = 0.38, name.style = 'center-center', cex.names = 1, print.id = FALSE, max.depth = 100, lwd = 0.5)
options(.aqp.plotSPC.args = .args)


# note that "Z" horizons are fake horizons, added to create conformal profiles
# we ignore them later

# ragg::agg_png(filename = 'figures/missing-data-example.png', width = 1000, height = 500, scaling = 1.25)

par(mar = c(0, 0, 3, 2.3))

plotSPC(x[.idx, ], color = 'water_15_bar_to_clay_ratio', col.label = 'Water Retention (15 bar) to Clay Ratio', col.legend.cex = 1.25)

dev.off()


## notes on filtering:
# missing data
# missing below reasonable depth of interest
# single horizons
# parent material horizons
# O horizons
# profiles with bogus horizon depths


## truncate to depth of interest
# anything >75cm not likely useful
.maxdepth <- 75
x <- trunc(x, 0, .maxdepth)

str(x)

## define horizon level variables of interest
.v <- c('ph_naf', 'water_15_bar_to_clay_ratio', 'new_zealand_phosphorus_retent', 
        'aluminum_plus_half_iron_oxalat', 'aluminum_ammonium_oxalate', 
        'silica_ammonium_oxalate', 'cec_nh4_ph_7', 'water_retention_15_bar')


## evaluate missing data
# filtering out those horizons with horizon designation matching p (REGEX pattern)
# on whole profile basis
x$non.missing.relative <- evalMissingData(x, vars = .v, p = 'Cr|Cd|R|O|Z', method = 'relative')
x$non.missing.absolute <- evalMissingData(x, vars = .v, p = 'Cr|Cd|R|O|Z', method = 'absolute')

# on a horizon basis
x$non.missing.fraction <- evalMissingData(x, vars = .v, p = 'Cr|Cd|R|O|Z', method = 'horizon')

# graphical eval of non-missing data indices
histogram(x$non.missing.relative, par.settings = tactile.theme())
histogram(x$non.missing.absolute, par.settings = tactile.theme())


# iterate over random samples of 20 profiles to check
.idx <- sample(1:length(x), size = 20)
plotSPC(x[.idx, ], color = 'non.missing.fraction')

# figures for slides

set.seed(1)
.idx <- sample(1:length(x), size = 20)


par(mar = c(0.25, 0, 3, 0))
plotSPC(x[.idx, ], color = 'non.missing.fraction', width = 0.42, max.depth = 70, lwd = 0.5, cex.names = 0.9, col.legend.cex = 1.25, col.label = 'Data Availability Fraction', y.offset = -10, depth.axis = list(line = -4), n.legend = 4)

dev.off()

## keep only 100% complete profiles
## 1443 profiles
z <- subset(x, non.missing.relative  == 1)
length(z)

# iterate over random samples of 20 profiles to check
.idx <- sample(1:length(z), size = 20)
plotSPC(z[.idx, ], color = 'non.missing.fraction')

# across collections?
table(z$which)

# isotic  mixed 
# 558     880 -->
# 560     883


## apply a more complex filter, accomplished with a custom function

# function applied to each profile in a collection
# require A, E, B horizon
# require at least 50cm of soil material
.testIt <- function(i, min.thick = 50) {
  
  # just horizon data
  h <- horizons(i)
  
  # find and keep A, E, B horizons
  h <- h[grep('A|E|B', h$hzn_desgn), ]
  
  # find and keep just those horizons with variables of interest
  h <- h[which(!is.na(h$non.missing.fraction)), ]
  
  # if no remaining horizons
  # FALSE
  if(nrow(h) < 1) {
    return(FALSE)
  }
  
  # total thickness filter
  # TRUE if meets all requirements
  # FALSE if not
  .res <- sum((h$hzn_bot - h$hzn_top), na.rm = TRUE) >= min.thick
  
  return(.res)
}

## apply compound filter to all profiles
z$thickness.test <- profileApply(z, FUN = .testIt)
table(z$thickness.test)


## keep subset
# for clarity, but expression is logical
# 1063 profiles retained, 414 dropped
# now 1104 (Aug 22, 2025)
zz <- subset(z, thickness.test == TRUE)
length(zz)

# check
.idx <- sample(1:length(zz), size = 20)
plotSPC(zz[.idx, ], color = '.filledGap')


## reset plotSPC defaults
options(.aqp.plotSPC.args = NULL)


## weighted mean calculations with slab()
# results in long-format
a <- slab(
  zz, 
  fm = pedon_key ~ ph_naf + water_15_bar_to_clay_ratio + new_zealand_phosphorus_retent + aluminum_ammonium_oxalate + silica_ammonium_oxalate + aluminum_plus_half_iron_oxalat + sand_total + silt_total + clay_total + 
    sand_coarse + sand_very_coarse + sand_fine + sand_medium + sand_very_fine +
    wf_2075 + wf_25 + wf_520 + cec_nh4_ph_7 + water_retention_15_bar,
  slab.structure = c(0, .maxdepth), 
  slab.fun = mean, 
  na.rm = TRUE
)

head(a)

# inspect results
histogram(~ value | variable, data = a, as.table = TRUE, par.settings = tactile.theme(), scales = list(relation = 'free'), breaks = 30)

# very large values in Al-Ox and Si-Ox extract
a[a$value > 500, ]

# ignore this. No longer pedons with high values of Al-Ox and Si-Ox

# remove these values temporarily
# better
histogram(~ value | variable, data = a, as.table = TRUE, par.settings = tactile.theme(), scales = list(relation = 'free'), breaks = 30, subset = value < 500)


# use data.table vs. reshape2 implementation
# requires temporary conversion to data.table object
# to avoid annoying messages
# rows: pedon_key, top, bottom
# columns: variables
a <- data.table(a)
w <- dcast(a, pedon_key + top + bottom ~ variable, value.var = 'value')

# check: looks good
# 1063 rows
# now 1104
head(w)
nrow(w)


## re-attach site-level data
w <- merge(w, site(zz), by = 'pedon_key', all.x = TRUE, sort = FALSE)


## how many pedons are left?
table(w$which)

# isotic  mixed 
# 416    646 -->
# 431    673


## save
library(dplyr)
w <- w %>%
  mutate(across(c(longitude_decimal_degrees, latitude_decimal_degrees), as.numeric))
  
saveRDS(w, file = 'data/combined-lab-wt-mean-with-rf-and-sandfrac.rds')
write.csv(w, file = 'data/combined-lab-wt-mean-with-rf-and-sandfrac.csv')
library(writexl)
write_xlsx(w, 'data/combined-lab-wt-mean-with-rf-and-sandfrac.xlsx')

## mineralogy interpretation
## analytical data and XRD interp
h <- horizons(x)

h <- h[, c('pedon_key', 'labsampnum', 'clay_mineral_interpretation', .v)]
head(h)

# add category to recognize horizons with no clay mineral interpretation
h$clay_mineral_interpretation[which(is.na(h$clay_mineral_interpretation))] <- 'missing'
head(h)

h <- subset(h, subset = pedon_key %in% w$pedon_key)

## save
saveRDS(h, file = 'data/all-pedons-w-claymininterp.rds')

## cleanup
rm(list = ls())
gc(reset = TRUE)

