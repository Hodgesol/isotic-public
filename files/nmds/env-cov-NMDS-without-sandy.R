## running NMDS. Extracting pedons within 90% ellipsoids.
## D. Beaudette and R. Hodges
##
##

## cleanup
rm(list = ls())
gc(reset = TRUE)

library(vegan)
library(MASS)
library(hexbin)
library(lattice)
library(latticeExtra)
library(cluster)
library(rms)
library(mixtools)
library(dplyr)

# previously created data
# filtered, but greatly reduced due to NA in NPP grid
# x <- readRDS('data/final_pedon_climate_data.rds')

# not filtered for NA or extreme values
x <- readRDS('data/results-with-rf-and-sandfrac.rds')

# reminder
str(x)


# switch levels of our factor for model
x$isotic <- x$which == 'isotic'


# remove observations with NA values for environmental covariates of interest
MISSING <-  is.na(x$effective_precipitation_800m) | 
            is.na(x$final_MAAT_800m) | 
            is.na(x$et_rast) | 
            is.na(x$annualNPP) | 
            is.na(x$andisols.and.andic.subgroups)

# losing 147 observations
sum(MISSING)

# went from 1124 to 977 observations 
x <- subset(x,
            subset = !MISSING)
      

## multivariate context
# just env properties except MAP
# vars <- c('effective_precipitation_800m', 'final_MAAT_800m', 'et_rast', 'annualNPP', 
#           'andisols.and.andic.subgroups')
# 
# .x <- x[, vars]
# names(.x) <- c('Effective Precip.', 'MAAT', 'aET', 'Annual NPP', 'Andic')

vars <- c('effective_precipitation_800m', 'final_MAAT_800m', 
          'andisols.and.andic.subgroups')

.x <- x[, vars]
names(.x) <- c('Effective Precip.', 'MAAT', 'Andic')



# plotting
hexplom(.x, trans = log, inv = exp, colramp = hcl.colors, axis.text.cex = 0.5, varname.cex = 0.8, varname.font = 2)

cols <- rgb(t(col2rgb(1:2)) / 255, alpha = 0.25)
cols <- scales::alpha(c(grey(0.6), 'firebrick'), alpha = 0.25)

# trick, based on preservation of row-order between x and .x
splom(.x, cex = 0.5, pch = 16, col = cols[as.numeric(x$isotic) + 1], axis.text.cex = 0.5, varname.cex = 0.8, varname.font = 2)



## ordination


# k = 2
set.seed(101010)
o2 <- metaMDS(.x, distance = "gower", k = 2, autotransform = FALSE)

# eval ordination, seems reasonable
stressplot(o2)
o2$stress

# ## save
# saveRDS(o, file = 'data/soilprop-nosandy-nmds-k2.rds')


# k = 3
# set.seed(101010)
# o3 <- metaMDS(.x[, 1:5], distance = "gower", k = 3, autotransform = FALSE)
# 
# # eval ordination, seems reasonable, stress = 0.10
# stressplot(o3)
# o3$stress

# # k = 4
# set.seed(101010)
# o4 <- metaMDS(.x[, 1:5], distance = "gower", k = 4, autotransform = TRUE)
# 
# # k = 5
# set.seed(101010)
# o5 <- metaMDS(.x[, 1:5], distance = "gower", k = 5, autotransform = TRUE)
# 
# # eval ordination, seems reasonable
# stressplot(o5) 
# o5$stress

## logistic regression predictions might be a useful annotation
dd <- datadist(.x)
options(datadist = 'dd')

m.full <- lrm(
  isotic ~ ., 
  data = data.frame(isotic = x$isotic, .x)
)

p <- predict(m.full, type = 'fitted.ind')

# colors: adjust as needed
.nrcs_blue <- '#19567B'
.ncss_gray <- '#a09074'
# .nrcs_brown <- '#724324'
# .grey <- c(grey(0.6))
# .ppt_purple <- '#A02B93'
# .ppt_orange <- '#FF9933'
.cols <- c(.nrcs_blue, .ncss_gray)

cols <- scales::alpha(.cols, alpha = 0.5)

ragg::agg_png(filename = 'figures/nmds-nosandy-env-properties-k2-andic-mat-effmap.png', width = 1000, height = 1000, scaling = 1.25)

par(mar = c(3, 3, 3, 3))
plot(o2$points[, 1:2], las = 1, axes = FALSE, type = 'n', xlab = '', ylab = '')
abline(h = 0, v = 0, lty = 2)
points(o2$points[, 1:2], col = cols[as.numeric(x$isotic) + 1], pch = 16, cex = 2)

mtext('NMDS1', side = 1, cex = 2, line = 1.5)
mtext('NMDS2', side = 2, cex = 2, line = 1)

legend(
  'topleft', 
  legend = c('Isotic', 'Mixed'), 
  col = .cols, 
  pch = 16, 
  pt.cex = 2.5, 
  cex = 2, 
  bty = 'n'
)

# box()

# Pr(isotic)
# ordisurf(o, p, add = TRUE, col = 'firebrick', levels = seq(0, 1, by = 0.1))

# NaF pH
# ordisurf(o, .x$pH_NaF, add = TRUE, col = 'blue')

dev.off()


## combine NMDS scores with original data
x$nMDS1 <- o2$points[, 1]
x$nMDS2 <- o2$points[, 2]
# x$nMDS3 <- o3$points[, 3]

# notes on distance calculation by metaMDS:
# Gower's generalized distance metric
# not transformed
# o3$data

## producing the figure for the 90% mean ellipse and saving figure

ragg::agg_png(filename = 'figures/env-nMDS-wellipse-nosandy-3-andic-mat-effmap.png', width = 1000, height = 1000, scaling = 1.25)

par(mar = c(3, 3, 3, 3))
plot(x$nMDS1, x$nMDS2, las = 1, axes = FALSE, type = 'n', xlab = '', ylab = '')
abline(h = 0, v = 0, lty = 2)
points(x$nMDS1, x$nMDS2, col = cols[as.numeric(x$isotic) + 1], pch = 16, cex = 2)

mtext('NMDS1', side = 1, cex = 2, line = 1.5)
mtext('NMDS2', side = 2, cex = 2, line = 1)

legend(
  'topright',
  legend = c('Isotic', 'Mixed'),
  col = .cols,
  pch = 16,
  pt.cex = 2.5,
  cex = 2,
  bty = 'n'
)

# 90% probability density ellipse of dataset
sigma <- cov(x[, c('nMDS1', 'nMDS2')])
mu <- c(mean(x$nMDS1), mean(x$nMDS2))

addEllipse(mu, sigma, p.interval = 0.9, col = 'firebrick',
           lwd = 2)

dev.off()




###############################################################################
## extract pedons within ellipse
library(ggplot2)
library(ggforce)
library(sp)
library(raster)
library(tidyverse)
library(ellipse)

library(SIBER)

# creating ellipse and isolating pedons within ellipse


# ragg::agg_png(filename = 'figures/env-win90ellipse-nosand.png', width = 1000, height = 1000, scaling = 1.25)

# isolate the nMDS columns
dat1 <- x[, c('nMDS1', 'nMDS2')]

plot(dat1[, c('nMDS1', 'nMDS2')], asp = 1)


# 90% probability density ellipse of dataset
sigma <- cov(dat1[, c('nMDS1', 'nMDS2')])
mu <- c(mean(dat1$nMDS1), mean(dat1$nMDS2))

# addEllipse(mu, sigma, p.interval = 0.9, col = "blue", lty = 3)

Z <- pointsToEllipsoid(dat1, sigma, mu) # converts the data to ellipsoid coordinates
out <- !ellipseInOut(Z, p = 0.9) # logical vector
(outliers <- dat1[out,]) # finds the points outside the ellipse

points(outliers, col = "red", pch = 19)

dat.in <- dat1[!out,] # use the out vector to remove unwanted rows. 

dev.off()



## now remove mixed and isotic outliers from original dataset and save for stats
dat <- x[, c('pedon_key', 'nMDS1', 'nMDS2')]
dat.in <- dat[!out,] # use the out vector to remove unwanted rows. 


x.new <- x[(x$pedon_key %in% dat.in$pedon_key), ]


# # check new data frame with ellipses. Looks good
# x.new.plot <- x.new1[, c(47:48)]
# plot(x.new.plot)
# addEllipse(mu, sigma, p.interval = 0.9, col = "blue", lty = 3)


## save
saveRDS(x.new, file = 'data/isotic-mixed-nosand-90-dataset-forenvtree.rds')


summary(x.new$which)






