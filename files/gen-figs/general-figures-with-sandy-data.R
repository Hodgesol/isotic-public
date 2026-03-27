## Make figures to illustrate what we know and perhaps do not know about the isotic mineralogy class.
## 2024-04-25
## R. Hodges and D.E Beaudette

library(tactile)
library(lattice)
library(latticeExtra)

# previously created data.frame
# x <- readRDS('data/final_pedon_climate_data.rds')

# not filtered for NA or extreme values
x <- readRDS('data/pedons-with-sandy-and-skeletal-psc.rds')

str(x)

# filter extreme values
summary(x)
x <- subset(x, subset = aluminum_ammonium_oxalate < 5)


# switch levels of our factor for model
x$isotic <- x$which == 'isotic'
x$which <- factor(x$which, levels = c('mixed', 'isotic'))

## traditional definition of isotic mineralogy class
tps <- tactile.theme(superpose.symbol = list(pch = 16, cex = 1.5, alpha = 0.6))
.nrcs_blue <- '#19567B'
.nrcs_brown <- '#724324'
.nrcs_green <- '#005440'
.nrcs_orange <- '#FFCA63'
.nrcs_tan <- '#A09074'

# .ppt_purple <- '#A02B93'
# .ppt_orange <- '#FF9933'
cols <- c(.nrcs_tan, .nrcs_blue)

.xgrid <- log(c(0.3, 0.4, 0.5, 0.6, 0.8, 1, 2, 5, 10), base = 10)
.ygrid <- seq(7, 11.5, by = 0.5)

p <- xyplot(
  ph_naf ~ log(water_15_bar_to_clay_ratio, base = 10), 
  data = x, 
  groups = which, 
  par.settings = list(superpose.symbol = list(pch = 16, cex = 1.5, 
                                              col = c(.nrcs_tan, .nrcs_blue),
                                              alpha = 0.6)), 
  xlab = list(label = 'Water Retention at 1500 kPa to Clay Ratio (log base10)', cex = 1.4),
  ylab = list(label = 'Sodium Flouride pH', cex = 1.4),
  main = list('Mineralogy Class', cex = 1.5),
  auto.key = list(points = TRUE, lines = FALSE, columns = 2, cex = 1.4),
  scales = list(cex = 1, x = list(log = FALSE, at = .xgrid, labels = 10^.xgrid), y = list(at = .ygrid)),
  panel = function(...) {
    panel.abline(h = .ygrid, v = .xgrid, lty = 3, col = grey(0.5))
    panel.abline(h = 8.4, v = log(0.6, base = 10))
    panel.xyplot(...)
  }
)

p


ragg::agg_png(filename = 'figures/isotic-definition-of-pedons-with-sand-data.png', width = 1200, height = 1000, scaling = 1.5)

print(p)

dev.off()


## create dataframe without isotic pedons that have sandy or sandy-skeletal PSC
library(lessR)
library(tidyverse)

x2 <- x %>%
  filter(which == "isotic" & (ps_class == "Sandy" | ps_class == "Sandy-Skeletal"))
x3 <- x[!(x$pedon_key %in% x2$pedon_key), ]


## export iso.sub
library(writexl)
write_xlsx(x2, "data/sandy-pedons-excluded.xlsx")


# write as RDS
saveRDS(x3, file = "data/results-without-sandy-and-skeletal.rds")

# export x3 to an excel spreadsheet
# install.packages('writexl')
library(writexl)
write_xlsx(x3, "data/results-without-sandy-and-sandy-skeletal.xlsx")

## same plot but now exclude (grey out or remove) pedons that are sandy or sandy-skeletal
tps <- tactile.theme(superpose.symbol = list(pch = 16, cex = 1.5, alpha = 0.6))

.xgrid <- log(c(0.3, 0.4, 0.5, 0.6, 0.8, 1, 2, 5, 10), base = 10)
.ygrid <- seq(7, 11.5, by = 0.5)

p2 <- xyplot(
  ph_naf ~ log(water_15_bar_to_clay_ratio, base = 10), 
  data = x, 
  groups = which, 
  par.settings = list(superpose.symbol = list(pch = 16, cex = 1.5, 
                                              col = c(.nrcs_tan, .nrcs_blue),
                                              alpha = 0.6)), 
  xlab = list(label = 'Water Retention at 1500 kPa to Clay Ratio (log base10)', cex = 1.4),
  ylab = list(label = 'Sodium Flouride pH', cex = 1.4),
  main = list('Mineralogy Class', cex = 1.5),
  auto.key = list(points = TRUE, lines = FALSE, columns = 2, cex = 1.4),
  scales = list(cex = 1, x = list(log = FALSE, at = .xgrid, labels = 10^.xgrid), y = list(at = .ygrid)),
  panel = function(...) {
    panel.abline(h = .ygrid, v = .xgrid, lty = 3, col = grey(0.5))
    panel.abline(h = 8.4, v = log(0.6, base = 10))
    panel.xyplot(...)
  }
)

p2

ragg::agg_png(filename = 'figures/isotic-definition-of-pedons-minus-sandy.png', width = 1200, height = 1000, scaling = 1.5)

print(p2)

dev.off()


## 1500 kpa against Al (AO)


tps <- tactile.theme(superpose.symbol = list(pch = 16, cex = 1.5, alpha = 0.6))

.xgrid <- log(c(0.3, 0.4, 0.5, 0.6, 0.8, 1, 2, 5, 10), base = 10)
.ygrid <- log(c(0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.8, 1, 2, 3), base = 10)
#.ygrid <- seq(0, 5, by = 0.5)

p3 <- xyplot(
  log(aluminum_ammonium_oxalate, base = 10) ~ log(water_15_bar_to_clay_ratio, base = 10), 
  data = x3, 
  groups = which,
  par.settings = tps, 
  xlab = list(label = '-1500 kPa/clay', cex = 1.25),
  ylab = list(label = 'Al (%; AO)', cex = 1.25),
  main = 'Mineralogy Class',
  auto.key = list(points = TRUE, lines = FALSE, columns = 2),
  scales = list(cex = 1, x = list(log = FALSE, at = .xgrid, labels = 10^.xgrid), y = list(log = FALSE, 
                                                                                          at = .ygrid,
                                                                                          labels = 10^.ygrid)),
  panel = function(...) {
    panel.abline(h = .ygrid, v = .xgrid, lty = 3, col = grey(0.5))
    panel.abline(h = log(0.53, base = 10), v = log(0.63, base = 10))
    panel.xyplot(...)
  }
)

p3

ragg::agg_png(filename = 'figures/1500clay-vs-Alao-of-pedons-with-sand-data-minus-sandy.png', width = 1200, height = 1000, scaling = 1.5)

print(p3)


dev.off()


## How were the sandy and sandy-skeletal pedons classified under "$which"?
## 60 isotic removed
table(x3$which)
table(x$which)


## univariate summaries


# graphical check... looks similar to full dataset z
my_settings <- list(
  par.strip.text = list(cex = 1.5), # For panel titles
  scales = list(y = list(cex = 1.2), x = list(cex = 1.2)) # For axis labels
)

bwplot(which ~ final_MAP_mm_800m, data = x3, par.settings = tactile.theme())

# 

bwplot(which ~ new_zealand_phosphorus_retent, data = x3, par.settings = tactile.theme(),
       xlab = list("NZ-P Retention (%)", fontsize = 18),
       scales = list(y = list(cex = 1.5), x = list(cex = 1.2))) # axis tick labels

bwplot(which ~ aluminum_ammonium_oxalate, data = x3, par.settings = tactile.theme(),
       xlab = list("Al (AO; %)", fontsize = 18),
       scales = list(y = list(cex = 1.5), x = list(cex = 1.2))) # axis tick labels

bwplot(which ~ ph_naf, data = x3, par.settings = tactile.theme(),
       xlab = list("pH (NaF)", fontsize = 18),
       scales = list(y = list(cex = 1.5), x = list(cex = 1.2))) # axis tick labels

bwplot(which ~ water_15_bar_to_clay_ratio, data = x3, par.settings = tactile.theme(),
       xlab = list("1500 kPa/clay", fontsize = 18),
       scales = list(y = list(cex = 1.5), x = list(cex = 1.2))) # axis tick labels

bwplot(which ~ silica_ammonium_oxalate, data = x3, par.settings = tactile.theme(),
       xlab = list("Si (AO; %)", fontsize = 18),
       scales = list(y = list(cex = 1.5), x = list(cex = 1.2))) # axis tick labels

bwplot(which ~ cec_nh4_ph_7, data = x3, par.settings = tactile.theme(),
       xlab = list("CEC7", fontsize = 18),
       scales = list(y = list(cex = 1.5), x = list(cex = 1.2))) # axis tick labels

bwplot(which ~ aluminum_plus_half_iron_oxalat, data = x3, par.settings = tactile.theme(),
       xlab = list("Al + 1/2Fe (AO; %)", fontsize = 18),
       scales = list(y = list(cex = 1.5), x = list(cex = 1.2))) # axis tick labels



# combined figure
library(reshape2)

# column names
v <- c('water_15_bar_to_clay_ratio', 'ph_naf', 'new_zealand_phosphorus_retent', 'aluminum_ammonium_oxalate', 'aluminum_plus_half_iron_oxalat', 'silica_ammonium_oxalate', 'cec_nh4_ph_7')

# desired labels + units
v.labels <- c('1500 kPa / Clay Ratio', 'NaF pH', 'NZ-P Retention (%)', 'Al (AO; %)', 'Al + 1/2 Fe (AO; %)', 'Si (AO; %)', 'CEC7 cmol [+]/kg soil')

m <- melt(x3, id.vars = c('which'), measure.vars = v)

m$variable <- factor(m$variable, levels = v, labels = v.labels)


p <- bwplot(
  which ~ value | variable, 
  data = m, 
  xlab = '',
  ylab = '',
  scales = list(x = list(relation = 'free', tick.number = 6)),
  par.settings = tactile.theme(plot.symbol = list(cex = 0.25)),
  par.strip.text = list(cex = 0.95, lines = 1.5),
  layout = c(3, 3),
  as.table = TRUE,
  between = list(x = 0.25, y = 1)
)


# # TODO: save directly to a figure
ragg::agg_png(filename = 'figures/box-plots/soilprop-boxplots.png', width = 800, height = 500, scaling = 1.5)
# 
print(p)
# 
dev.off()


## cleanup
rm(list = ls())
gc(reset = TRUE)



