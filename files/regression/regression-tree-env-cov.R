## Applying trained regression tree to environmental properties
## R. Hodges and D. Beaudette
## Oct 25, 2024
## 

## cleanup
rm(list = ls())
gc(reset = TRUE)


x <- readRDS('data/isotic-mixed-nosand-90-dataset-forenvtree.rds')




########################################################################
########## RTest ##################
library(dplyr)
library(rpart)
library(rpart.plot)
library(clhs)

############## data prep: creating random training and test datasets ########
## using clhs to create 80% representative subset for the training model

## train with CLHS
# v <- c('effective_precipitation_800m', 'final_MAAT_800m', 'et_rast', 'annualNPP', 
#        'andisols.and.andic.subgroups')

v <- c('effective_precipitation_800m', 'final_MAAT_800m', 
         'andisols.and.andic.subgroups')

set.seed(1986)
train_sample <- clhs(x[, v], size = floor(nrow(x) * 0.8))

nrow(x)
nrow(x[train_sample,  ])


# subset training and test datasets from main (x)
train <- x[train_sample, ]
test <- x[-train_sample, ]
# check randomization.
prop.table(table(x$which))

prop.table(table(train$which))
prop.table(table(test$which))

# modeling environmental covariates (excluding MAP due to colinearity w/eff precip)
env.fit <- rpart(which ~ effective_precipitation_800m + final_MAAT_800m + 
                   andisols.and.andic.subgroups, 
                 data = train, method = "class")
env.fit
summary(env.fit)

## visualize
rpart.plot(env.fit)
env.fit$variable.importance


## predict using the regression tree and evaluate
pred.tree <- predict(env.fit, test)

# calculate mean square error - Al(AO)
# al.ao.mse <- mean((pred.tree - min_test$aluminum_ammonium_oxalate)^2)
# al.ao.mse

# ## prune regression tree
printcp(env.fit)
bestcp <- env.fit$cptable[which.min(env.fit$cptable[,"xerror"]),"CP"]
bestcp

## prune tree with best cp value (0.01595745)
pruned.tree <- prune(env.fit, cp = bestcp)


## visualize the pruned tree
prp(pruned.tree, extra = 6, box.palette = "Blues", faclen = 0)
pruned.tree
summary(pruned.tree)

# save model
saveRDS(pruned.tree, file = "models/pruned-env-tree.rds")


# order of importance - variables
pruned.tree$variable.importance

## Evaluate performance of pruned regression tree
# Use the test data to evaluate performance of pruned regression tree
pred.prune.test = predict(pruned.tree, test, type = "class")

pred.prune.train = predict(pruned.tree, train, type = "class")

# generate the confusion matrix
library(caret)

confusionMatrix(pred.prune.train, train$which)

confusionMatrix(pred.prune.test, test$which)


# print
write.csv(train, file = "data/env-cov-train.csv")
write.csv(test, file = "data/env-cov-test.csv")

saveRDS(train, file = "data/env-cov-train.rds")
saveRDS(test, file = "data/env-cov-test.rds")















