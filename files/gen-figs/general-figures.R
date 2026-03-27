## Make figures to illustrate what we know and perhaps do not know about the isotic mineralogy class.
## 2024-01-18
## D.E Beaudette


library(tactile)
library(lattice)
library(latticeExtra)

# previously created data.frame
# x <- readRDS('data/final_pedon_climate_data.rds')

# not filtered for NA or extreme values
x <- readRDS('data/results.rds')

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

# cols <- scales::alpha(.cols, alpha = 0.5)

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


ragg::agg_png(filename = 'figures/isotic-definition.png', width = 1200, height = 1000, scaling = 1.5)

print(p)

dev.off()


# NZ-P versus -1500 kPa/clay ratio
.xgrid2 <- log(c(0.3, 0.4, 0.5, 0.6, 0.8, 1, 2, 5, 10), base = 10)
.ygrid2 <- seq(0, 100, by = 10)

NZ_P <- xyplot(
  new_zealand_phosphorus_retent ~ log(water_15_bar_to_clay_ratio, base = 10), 
  data = x, 
  groups = which, 
  par.settings = list(superpose.symbol = list(pch = 16, cex = 1.5, 
                                              col = c(.ppt_orange, .ppt_purple),
                                              alpha = 0.6)), 
  xlab = list(label = 'Water Retention at 1500 kPa to Clay Ratio (log10)', cex = 1.4),
  ylab = list(label = 'NZ Phosphate Retention (%)', cex = 1.4),
  main = list('Mineralogy Class', cex = 1.5),
  auto.key = list(points = TRUE, lines = FALSE, columns = 2, cex = 1.4),
  scales = list(cex = 1, x = list(log = FALSE, at = .xgrid2, labels = 10^.xgrid2), y = list(at = .ygrid2)),
  panel = function(...) {
    panel.abline(h = .ygrid2, v = .xgrid2, lty = 3, col = grey(0.5))
    panel.abline(h = 33, v = log(0.66, base = 10))
    panel.xyplot(...)
  }
)

NZ_P

ragg::agg_png(filename = 'figures/nzp-vs-1500toclay.png', width = 1200, height = 1000, scaling = 1.5)

print(NZ_P)

dev.off()

# Al(AO) versus -1500 kPa/clay
.xgrid3 <- log(c(0.3, 0.4, 0.5, 0.6, 0.8, 1, 2, 5, 10), base = 10)
.ygrid3 <- log(c(0, 0.25, 0.5, 1.0, 2.0, 3, 5), base = 10)

Alao.1500 <- xyplot(
  log(aluminum_ammonium_oxalate, base = 10) ~ log(water_15_bar_to_clay_ratio, base = 10), 
  data = x, 
  groups = which, 
  par.settings = tps, 
  xlab = list(label = 'Water Retention at 15bar to Clay Ratio', cex = 1.25),
  ylab = list(label = 'Aluminum (ammonium oxalate; %)', cex = 1.25),
  main = 'Mineralogy Class',
  auto.key = list(points = TRUE, lines = FALSE, columns = 2),
  scales = list(cex = 1, x = list(log = FALSE, at = .xgrid3, labels = 10^.xgrid3), 
                         y = list(log = FALSE, at = .ygrid3, labels = 10^.ygrid3)),
  panel = function(...) {
    panel.abline(h = .ygrid3, v = .xgrid3, lty = 3, col = grey(0.5))
    panel.abline(v = log(0.6, base = 10))
    panel.xyplot(...)
  }
)

Alao.1500

ragg::agg_png(filename = 'figures/Alao-vs-1500toclay.png', width = 1200, height = 1000, scaling = 1.5)

print(Alao.1500)

dev.off()

## univariate summaries


# graphical check... looks similar to full dataset z
bwplot(which ~ final_MAP_mm_800m, data = x, par.settings = tactile.theme())

bwplot(which ~ new_zealand_phosphorus_retent, data = x, par.settings = tactile.theme(),
       main = "0-75 cm wt. mean", xlab = "NZ-P (%)")

bwplot(which ~ aluminum_ammonium_oxalate, data = x, par.settings = tactile.theme(),
       main = "0-75 cm wt. mean", xlab = "Al (AO; %)", subset = )

bwplot(which ~ ph_naf, data = x, par.settings = tactile.theme(),
       main = "0-75 cm wt. mean", xlab = "pH (NaF)")

bwplot(which ~ water_15_bar_to_clay_ratio, data = x, par.settings = tactile.theme(),
       main = "0-75 cm wt. mean", xlab = "-1500 kPa/clay")


## create dataframe without sandy and sandy-skeletal pedons
library(lessR)
# x2 <- x[, .(ps_class != "Sandy" & ps_class != "Sandy-Skeletal")]
x2 <- x[!grepl("sandy", x$corr_taxpartsize), ]

# write as RDS
saveRDS(x2, file = "data/results-without-sandy-and-skeletal.rds")

## same plot but now exclude (grey out or remove) pedons that are sandy or sandy-skeletal
tps <- tactile.theme(superpose.symbol = list(pch = 16, cex = 1.5, alpha = 0.6))

.xgrid <- log(c(0.3, 0.4, 0.5, 0.6, 0.8, 1, 2, 5, 10), base = 10)
.ygrid <- seq(7, 11.5, by = 0.5)

p2 <- xyplot(
  ph_naf ~ log(water_15_bar_to_clay_ratio, base = 10), 
  data = x2, 
  groups = which,
  par.settings = tps, 
  xlab = list(label = 'Water Retention at 15bar to Clay Ratio', cex = 1.25),
  ylab = list(label = 'Sodium Flouride pH', cex = 1.25),
  main = 'Mineralogy Class',
  auto.key = list(points = TRUE, lines = FALSE, columns = 2),
  scales = list(cex = 1, x = list(log = FALSE, at = .xgrid, labels = 10^.xgrid), y = list(at = .ygrid)),
  panel = function(...) {
    panel.abline(h = .ygrid, v = .xgrid, lty = 3, col = grey(0.5))
    panel.abline(h = 8.4, v = log(0.6, base = 10))
    panel.xyplot(...)
  }
)

p2

ragg::agg_png(filename = 'figures/isotic-definition-of-pedons--minus-classif-sandy.png', width = 1200, height = 1000, scaling = 1.5)

print(p2)

dev.off()

## How were the sandy and sandy-skeletal pedons classified under "$which"?
## 42 mixed removed, 41 isotic removed
table(x2$which)
table(x$which)





