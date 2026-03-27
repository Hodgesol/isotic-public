## Environmental rasters logistic regression analysis
## R. Hodges and D. Beaudette
## July 28, 2025

## cleanup
rm(list = ls())
gc(reset = TRUE)


library(hexbin)
library(tactile)
library(lattice)
library(latticeExtra)
library(corrplot)
library(rms)

# not filtered for NA or extreme values
# x <- readRDS('data/results.rds')

# how will the regression results look if we use this dataset?
x <- readRDS('data/results-with-rf-and-sandfrac.rds')


# switch levels of our factor for model
x$isotic <- x$which == 'isotic'


table(x$isotic)

## multivariate context

# some variables of interest
vars <- c('effective_precipitation_800m', 'final_MAAT_800m', 'et_rast', 'annualNPP', 
          'andisols.and.andic.subgroups')

# subset and shorten names
.x <- x[, vars]
names(.x) <- c('eff_precip', 'MAAT', 'aET', 'NPP', 'andic')


# marginal and pair-wise joint distributions of select soil properties
# isotic soils highlighted
cols <- scales::alpha(c(grey(0.6), 'firebrick'), alpha = 0.25)
splom(
  .x, 
  cex = 0.65, 
  pch = 16, 
  col = cols[as.numeric(x$isotic) + 1], 
  axis.text.cex = 0.5, 
  varname.cex = 0.8, 
  varname.font = 2
)


## logistic regression
dd <- datadist(x)
options(datadist = 'dd')

m.full <- lrm(
  isotic ~ effective_precipitation_800m + final_MAAT_800m + et_rast + annualNPP + 
           andisols.and.andic.subgroups, 
  data = x
) ; m.full

m.reduced <- lrm(
  isotic ~ effective_precipitation_800m + final_MAAT_800m + annualNPP + 
    andisols.and.andic.subgroups, 
  data = x
) ; m.reduced

m.reduced2 <- lrm(
  isotic ~ effective_precipitation_800m + final_MAAT_800m + andisols.and.andic.subgroups, 
  data = x
) ; m.reduced2

m.reduced3 <- lrm(
  isotic ~ effective_precipitation_800m + final_MAAT_800m + et_rast + 
    andisols.and.andic.subgroups, 
  data = x
) ; m.reduced3

m.reduced4 <- lrm(
  isotic ~ effective_precipitation_800m + final_MAAT_800m + 
    andisols.and.andic.subgroups, 
  data = x
) ; m.reduced4

## questions:
# 1. do all of these 5 measurements (wt. mean soil properties) contribute 
# logically and consistently to our use of the term "isotic" vs. "mixed"?
#
# - logically: are the curves / slopes in the right direction?
# - consistently: are the curves / slopes > 0, and with "small" SE (narrow conf. envelopes)?
#

# simple visualization of partial effects
p <- plot(Predict(m.full, fun = plogis))
p <- update(p, as.table = TRUE, scales = list(alternating = 1, tick.number = 8, cex = 1), ylab = list('Pr(isotic)', cex = 1.5))
p

p2 <- plot(Predict(m.reduced, fun = plogis))
p2 <- update(p2, as.table = TRUE, scales = list(alternating = 1, tick.number = 8, cex = 1), ylab = list('Pr(isotic)', cex = 1.5))
p2

p3 <- plot(Predict(m.reduced2, fun = plogis))
p3 <- update(p3, as.table = TRUE, scales = list(alternating = 1, tick.number = 8, cex = 1), ylab = list('Pr(isotic)', cex = 1.5))
p3

p3 <- plot(Predict(m.reduced3, fun = plogis))
p3 <- update(p3, as.table = TRUE, scales = list(alternating = 1, tick.number = 8, cex = 1), ylab = list('Pr(isotic)', cex = 1.5))
p3

p4 <- plot(Predict(m.reduced4, fun = plogis))
p4 <- update(p4, as.table = TRUE, scales = list(alternating = 1, tick.number = 8, cex = 1), ylab = list('Pr(isotic)', cex = 1.5))
p4

# reduced model looks pretty good save a figure
row.names(p4) <- c('Andic Soils (% area)', 'Effective Precip. (mm)', 'MAAT (Celcius)')
p4

ragg::agg_png(filename = 'figures/env-lrm-partial-effects-and-effmap-maat.png', width = 1200, height = 500, scaling = 1.5)
print(p3)
dev.off()


# variance inflation factor
VIF <- vif(m.reduced4)
vif_df <- data.frame(
  Variable = names(VIF),
  VIF = as.numeric(VIF)
)
knitr::kable(vif_df, col.names = c('var', 'VIF'), digits = 2)



# plotting checks
# cm <- cor(x[, .vars], method = 'spearman', use = 'complete.obs')
# cm.isotic <- cor(x[which(x$which == 'isotic'), .vars], method = 'spearman', use = 'complete.obs')
# cm.mixed <- cor(x[which(x$which == 'mixed'), .vars], method = 'spearman', use = 'complete.obs')
# 
# knitr::kable(cm.isotic, digits = 2)
# 
# corrplot(cm, method = 'number', col = hcl.colors(n = 20, 'zissou1'), type = 'lower', order = 'AOE', diag = FALSE, col.lim = c(0, 1))
# 
# corrplot(cm.isotic, method = 'number', col = hcl.colors(n = 20, 'zissou1'), type = 'lower', order = 'AOE', diag = FALSE, col.lim = c(0, 1))
# 
# corrplot(cm.mixed, method = 'number', col = hcl.colors(n = 20, 'zissou1'), type = 'lower', order = 'AOE', diag = FALSE, col.lim = c(-0.04, 1))


plot(summary(m.full))
plot(anova(m.full))

plot(summary(m.reduced))
plot(anova(m.reduced))

plot(summary(m.reduced2))
plot(anova(m.reduced2))

