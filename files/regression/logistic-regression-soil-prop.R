## Soil properties logistic regression analysis
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
library(dplyr)



# not filtered for NA or extreme values
# x <- readRDS('data/results.rds')

# how will the regression results look if we use this dataset?
x <- readRDS('data/results-without-sandy-and-skeletal.rds')


# switch levels of our factor for model
x$isotic <- x$which == 'isotic'

# isotic = 361, mixed = 644
table(x$isotic)

## multivariate context

# some variables of interest
vars <- c('ph_naf', 'water_15_bar_to_clay_ratio', 'new_zealand_phosphorus_retent', 'aluminum_ammonium_oxalate',  'silica_ammonium_oxalate', 'aluminum_plus_half_iron_oxalat', 'cec_nh4_ph_7')

# subset and shorten names
.x <- x[, vars]
names(.x) <- c('pH_NaF', 'ws15_clay', 'NZ_P', 'Al_o', 'Si_o', 'Al + 1/2 Fe (AO)', 'CEC7')


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

## Full
m.full <- lrm(
  isotic ~ ph_naf + log(water_15_bar_to_clay_ratio, base = 10) + new_zealand_phosphorus_retent +
    aluminum_ammonium_oxalate + silica_ammonium_oxalate + aluminum_plus_half_iron_oxalat + cec_nh4_ph_7, 
  data = x
) ; m.full

## Reduced
m.reduced <- lrm(
  isotic ~ log(water_15_bar_to_clay_ratio, base = 10) + new_zealand_phosphorus_retent + silica_ammonium_oxalate + cec_nh4_ph_7, 
  data = x
) ; m.reduced

# m.reduced.2 <- lrm(
#   isotic ~ log(water_15_bar_to_clay_ratio, base = 10) + new_zealand_phosphorus_retent +
#     aluminum_ammonium_oxalate + aluminum_plus_half_iron_oxalat + cec_nh4_ph_7, 
#   data = x
# ) ; m.reduced.2
# 
# m.reduced.3 <- lrm(
#   isotic ~ new_zealand_phosphorus_retent +
#     aluminum_ammonium_oxalate + aluminum_plus_half_iron_oxalat + cec_nh4_ph_7, 
#   data = x
# ) ; m.reduced.3
# 
# m.reduced.4 <- lrm(
#   isotic ~ new_zealand_phosphorus_retent +
#     aluminum_ammonium_oxalate +  cec_nh4_ph_7, 
#   data = x
# ) ; m.reduced.4


m.reduced.4 <- lrm(
  isotic ~ new_zealand_phosphorus_retent +
    silica_ammonium_oxalate + cec_nh4_ph_7, 
  data = x
) ; m.reduced.4

m.reduced.4.int <- lrm(
  isotic ~ new_zealand_phosphorus_retent +
    silica_ammonium_oxalate * cec_nh4_ph_7, 
  data = x
) ; m.reduced.4.int

# m.reduced.5 <- lrm(
#   isotic ~ new_zealand_phosphorus_retent +
#     silica_ammonium_oxalate + aluminum_plus_half_iron_oxalat, 
#   data = x
# ) ; m.reduced.5
# 
# m.reduced.6 <- lrm(
#   isotic ~ silica_ammonium_oxalate +
#     aluminum_plus_half_iron_oxalat + ph_naf, 
#   data = x
# ) ; m.reduced.6
# 
# m.reduced.7 <- lrm(
#   isotic ~ new_zealand_phosphorus_retent + cec_nh4_ph_7, 
#   data = x
# ) ; m.reduced.7

## questions:
# 1. do all of these 5 measurements (wt. mean soil properties) contribute 
# logically and consistently to our use of the term "isotic" vs. "mixed"?
#
# - logically: are the curves / slopes in the right direction?
# - consistently: are the curves / slopes > 0, and with "small" SE (narrow conf. envelopes)?
#


# simple visualization of partial effects
p1 <- plot(Predict(m.full, fun = plogis))
p1 <- update(p1, as.table = TRUE, scales = list(alternating = 1, tick.number = 8, cex = 1), ylab = 'Pr(isotic)')
p1

# p2 <- plot(Predict(m.full.2, fun = plogis))
# p2 <- update(p2, as.table = TRUE, scales = list(alternating = 1, tick.number = 8, cex = 1), ylab = 'Pr(isotic)')
# p2
# 
p3 <- plot(Predict(m.reduced, fun = plogis))
p3 <- update(p3, as.table = TRUE, scales = list(alternating = 1, tick.number = 8, cex = 1), ylab = list('Pr(isotic)', cex = 1.5))
p3

# p4 <- plot(Predict(m.reduced.2, fun = plogis))
# p4 <- update(p4, as.table = TRUE, scales = list(alternating = 1, tick.number = 8, cex = 1), ylab = list('Pr(isotic)', cex = 1.5))
# p4

p5 <- plot(Predict(m.reduced.4, fun = plogis))
p5 <- update(p5, as.table = TRUE, scales = list(alternating = 1, tick.number = 8, cex = 1), ylab = list('Pr(isotic)', cex = 1.5))
p5

p6 <- plot(Predict(m.reduced.4.int, fun = plogis, silica_ammonium_oxalate = NA, cec_nh4_ph_7 = c(10, 20, 40)))
p6 <- update(p6, as.table = TRUE, scales = list(alternating = 1, tick.number = 8, cex = 1.2), ylab = list('Pr(isotic)', cex = 1.5), 
             xlab = list('Si (AO; %)', cex = 1.5), sub = 'Adjusted to NZ-P Retention of 38%')
p6

# further update interaction plot for paper
# demonstrate interaction with CEC at select percentiles
# roughly CEC7 ~ 5, 15, 30 cmol [+] / kg soil
round(quantile(x$cec_nh4_ph_7, probs = c(0.1, 0.5, 0.9), na.rm = TRUE))

# specify CEC7 values at which we are slicing the model surface
# sometimes this is called a "counter-factual" plot
pp <- Predict(m.reduced.4.int, fun = plogis, silica_ammonium_oxalate = NA, cec_nh4_ph_7 = c(5, 15, 30))

# nrcs colors
.nrcs_blue <- '#19567B'
.nrcs_brown <- '#724324'
.nrcs_green <- '#005440'
.nrcs_orange <- '#FFCA63'
.nrcs_tan <- '#A09074'

# adjust confidence interval shadding and turn off labeling of curves
# force a single panel by setting `nlevels` = number of CEC7 slices
p6 <- plot(pp, col.fill = gray(0.85), label.curves = FALSE, nlevels = 3, 
           col = c(.nrcs_blue, .nrcs_tan, .nrcs_orange))

update(
  p6, 
  scales = list(alternating = 1, tick.number = 8 , cex = 1.2),
  ylim = c(-0.1, 1.1),
  ylab = list('Pr(isotic)', cex = 1.5),
  xlab = list('Si (AO; %)', cex = 1.5), 
  sub = 'Adjusted to NZ-P Retention of 38%',
  par.settings = tactile.theme(superpose.line = list(lwd = 2, col = c(.nrcs_blue, .nrcs_tan, .nrcs_orange))),
  auto.key = list(columns = 3, space = 'top', title = 'CEC7 (cmol[+]/kg)', cex.title = 1)
)






# p7 <- plot(Predict(m.reduced.5, fun = plogis))
# p7 <- update(p7, as.table = TRUE, scales = list(alternating = 1, tick.number = 8, cex = 1), ylab = list('Pr(isotic)', cex = 1.5))
# p7
# 
# p8 <- plot(Predict(m.reduced.6, fun = plogis))
# p8 <- update(p8, as.table = TRUE, scales = list(alternating = 1, tick.number = 8, cex = 1), ylab = list('Pr(isotic)', cex = 1.5))
# p8
# 
# p9 <- plot(Predict(m.reduced.7, fun = plogis))
# p9 <- update(p9, as.table = TRUE, scales = list(alternating = 1, tick.number = 8, cex = 1), ylab = list('Pr(isotic)', cex = 1.5))
# p9

# reduced model looks pretty good save a figure
row.names(p5) <- c('CEC7 (cmolc/kg)', 'NZ-P Retention (%)', 'Si (AO; %)')
p5

ragg::agg_png(filename = 'figures/soilprop-lrm-partial-effects-3props.png', width = 1200, height = 500, scaling = 1.5)
print(p5)
dev.off()


# variance inflation factor
VIF <- vif(m.full)
vif_df <- data.frame(
  Variable = names(VIF),
  VIF = as.numeric(VIF)
)
knitr::kable(vif_df, col.names = c('var', 'VIF'), digits = 2)

VIF <- vif(m.full.2)
vif_df <- data.frame(
  Variable = names(VIF),
  VIF = as.numeric(VIF)
)
knitr::kable(vif_df, col.names = c('var', 'VIF'), digits = 2)

VIF <- vif(m.reduced)
vif_df <- data.frame(
  Variable = names(VIF),
  VIF = as.numeric(VIF)
)
knitr::kable(vif_df, col.names = c('var', 'VIF'), digits = 2)


VIF <- vif(m.reduced.2)
vif_df <- data.frame(
  Variable = names(VIF),
  VIF = as.numeric(VIF)
)
knitr::kable(vif_df, col.names = c('var', 'VIF'), digits = 2)

VIF <- vif(m.reduced.3)
vif_df <- data.frame(
  Variable = names(VIF),
  VIF = as.numeric(VIF)
)
knitr::kable(vif_df, col.names = c('var', 'VIF'), digits = 2)

VIF <- vif(m.reduced.4)
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

plot(summary(m.reduced.3))
plot(anova(m.reduced.3))

plot(summary(m.reduced.4))
plot(anova(m.reduced.4))
