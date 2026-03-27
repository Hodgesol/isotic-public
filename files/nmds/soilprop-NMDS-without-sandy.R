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

# previously created data
# filtered, but greatly reduced due to NA in NPP grid
# x <- readRDS('data/final_pedon_climate_data.rds')

# not filtered for NA or extreme values
x <- readRDS('data/results-without-sandy-and-skeletal.rds')

# reminder
str(x)


# switch levels of our factor for model
x$isotic <- x$which == 'isotic'


# just soil properties of interest; minus NaF pH and Al (AO)
vars.soil <- c('new_zealand_phosphorus_retent', 'silica_ammonium_oxalate',
               'cec_nh4_ph_7')

.x <- x[, vars.soil]
names(.x) <- c('NZ_P', 'Si_o', 'CEC')

# vars.soil <- c('new_zealand_phosphorus_retent', 'cec_nh4_ph_7')
# 
# .x <- x[, vars.soil]
# names(.x) <- c('NZ_P', 'CEC')

# plotting
hexplom(.x, trans = log, inv = exp, colramp = hcl.colors, axis.text.cex = 0.5, varname.cex = 0.8, varname.font = 2)

cols <- rgb(t(col2rgb(1:2)) / 255, alpha = 0.25)
cols <- scales::alpha(c(grey(0.6), 'firebrick'), alpha = 0.25)

# trick, based on preservation of row-order between x and .x
splom(.x, cex = 0.5, pch = 16, col = cols[as.numeric(x$isotic) + 1], axis.text.cex = 0.5, varname.cex = 0.8, varname.font = 2)


## ordination

# k = 1
# set.seed(101010)
# o1 <- metaMDS(.x, distance = "gower", k = 1, autotransform = TRUE)
# 
# # eval ordination, r^2 = 0.89, stress = 0.338
# stressplot(o1)
# o1$stress
# 
# ## save
# saveRDS(o1, file = 'data/soilprop-nosandy-nmds-k1.rds')


# k = 2, non-metric fit, 
set.seed(101010)
o2 <- metaMDS(.x, distance = "gower", k = 2, autotransform = FALSE)

# eval ordination, more reasonable than k = 1, r^2 = 0.992, stress = 0.092
stressplot(o2)
o2$stress

## save
saveRDS(o2, file = 'data/soilprop-nosandy-nmds-k2.rds')



## k = 3, non-metric fit, 
# set.seed(101010)
# o3 <- metaMDS(.x[, 1:5], distance = "gower", k = 3, autotransform = FALSE)
# 
# # eval ordination, seems reasonable, r^2 = 0.997, stress = 0.056
# stressplot(o3)
# o3$stress
# 
# ## save
# saveRDS(o3, file = 'data/soilprop-nosandy-nmds-k3.rds')


# k = 4, non-metric fit, r^2 = 1
# set.seed(101010)
# o4 <- metaMDS(.x[, 1:5], distance = "gower", k = 4, autotransform = TRUE)
# 
## eval ordination, seems reasonable, stress = 0.055
# stressplot(o4)
# o4$stress
# 
# ## save
# saveRDS(o4, file = 'data/soilprop-nosandy-nmds-k4.rds')
# 
# 
# # k = 5, non-metric fit, r^2 = 0.998
# set.seed(101010)
# o5 <- metaMDS(.x[, 1:5], distance = "gower", k = 5, autotransform = FALSE)
# 
# # eval ordination, seems reasonable, stress = 0.048
# stressplot(o5)
# o5$stress
# 
# ## save
# saveRDS(o5, file = 'data/soilprop-nosandy-nmds-k5.rds')
# 

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
# .nrcs_brown <- '#724324'
# .nrcs_green <- '#005440'
.ncss_gray <- '#a09074'

.cols <- c(.nrcs_blue, .ncss_gray)

cols <- scales::alpha(.cols, alpha = 0.5)

ragg::agg_png(filename = 'figures/nmds-nosandy-soil-properties-k2-CEC-NZP.png', width = 1000, height = 1000, scaling = 1.25)

par(mar = c(3, 3, 1, 1))
plot(o2$points[, 1:2], las = 1, axes = FALSE, type = 'n', xlab = '', ylab = '')
abline(h = 0, v = 0, lty = 2)
points(o2$points[, 1:2], col = cols[as.numeric(x$isotic) + 1], pch = 16, cex = 2)

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

# box()

# Pr(isotic)
# ordisurf(o, p, add = TRUE, col = 'firebrick', levels = seq(0, 1, by = 0.1))

# NaF pH
# ordisurf(o, .x$pH_NaF, add = TRUE, col = 'blue')

dev.off()


## combine NMDS scores with original data
x$nMDS1 <- o2$points[, 1]
x$nMDS2 <- o2$points[, 2]


# notes on distance calculation by metaMDS:
# Gower's generalized distance metric
# not transformed
o2$data


library(ggplot2)
library(ggforce)
library(sp)
library(raster)
library(tidyverse)
library(ellipse)

library(SIBER)

## producing the figure for the 90% mean ellipse and saving figure
dev.off()
ragg::agg_png(filename = 'figures/soilprop-nMDS-nosandy-k2-CEC-NZP-Si_o.png', width = 1000, height = 1000, scaling = 1.25)

par(mar = c(3, 3, 3, 3))
plot(x$nMDS1, x$nMDS2, las = 1, xlim = c(-0.46, 0.3), axes = FALSE, type = 'n', xlab = '', ylab = '')
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
# creating ellipse and isolating pedons within ellipse

#  ragg::agg_png(filename = 'figures/soilprop-win90ellipse-nosand.png', width = 1000, height = 1000, scaling = 1.25)

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



## now remove mixed and isotic "outliers" from original dataset and save for stats
dat <- x[, c('pedon_key', 'nMDS1', 'nMDS2')]
dat.in <- dat[!out,] # use the out vector to remove unwanted rows. 


x.new <- x[(x$pedon_key %in% dat.in$pedon_key), ]

# check new data frame with ellipses. Looks good
# x.new.plot <- x.new[, c('nMDS1', 'nMDS2', 'nMDS3')]
# plot(x.new.plot)
# addEllipse(mu, sigma, p.interval = 0.9, col = "blue", lty = 3)


## save
saveRDS(x.new, file = 'data/isotic-mixed-nosand-90-dataset.rds')

# isotic = 298, mixed = 632
summary(x.new$which)






