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
library(glmulti)


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
vars <- c('effective_precipitation_800m', 'final_MAAT_800m', 'et_rast', 'annualNPP', 
          'andisols.and.andic.subgroups')
response <- "isotic"


# apply backwards elimination on model
library(MASS)

full_model <- glm(isotic ~ effective_precipitation_800m + final_MAAT_800m + et_rast + annualNPP + 
                    andisols.and.andic.subgroups, 
                  data = x, family = binomial(link = "logit"))

# Run glmulti to find the best models
# Use method="h" for exhaustive screening (suitable for smaller numbers of predictors)
# also, method = "l" suitable for a fast branch-and-bound algorithm for linear
# models with only covariates and no interactions.
# Use crit="aicc" for the small-sample corrected AIC
# we don't want pairwise interactions, set level to 1
# Set marginality to TRUE to enforce the marginality rule
model_selection <- glmulti(
  y = full_model,
  # xr = vars,
  # data = x,
  level = 1,
  # marginality = TRUE,
  method = "h",
  crit = "aicc",
  confsetsize = 100 # Number of models in the confidence set to return
)

# Print a summary of the results
print(model_selection)

# Plot the information criterion profile
plot(model_selection)

# Plot the relative importance of terms
par(mar = c(4, 20, 3, 1))
plot(model_selection, type = "s", las = 1)


#
model_averaged_results <- coef(model_selection)
model_averaged_results


# Extract the weightable (table of models and their weights)
weightable(model_selection)



backward_model <- stepAIC(full_model, direction = "backward")
