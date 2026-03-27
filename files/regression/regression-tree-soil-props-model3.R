## version 3 of the tree: using NMDS data where clusters = 4, euclidean distance 
## for pairwise, and single 90% ellipse for drawing data.
## Applying trained regression tree to soil properties
## R. Hodges and D. Beaudette
## May 22, 2024
## 

## cleanup
rm(list = ls())
gc(reset = TRUE)


x <- readRDS('data/isotic-mixed-nosand-90-dataset.rds')




########################################################################
########## RTest ##################
library(dplyr)
library(rpart)
library(rpart.plot)
library(clhs)

############## data prep: creating random training and test datasets ########
## using clhs to create 80% representative subset for the training model


## train with CLHS
v <- c('new_zealand_phosphorus_retent', 'silica_ammonium_oxalate', 'cec_nh4_ph_7')

set.seed(2025)
train_sample <- clhs(x[, v], size = floor(nrow(x) * 0.8))
# subset.20 <- clhs(x[, v], size = floor(nrow(x) * 0.2))

nrow(x)
nrow(x[train_sample,  ])


# subset training and test datasets from main (x)
train <- x[train_sample, ]
test <- x[-train_sample, ]
# test.20 <- x[subset.20, ]

# check randomization. Should be about 30%. looks right
prop.table(table(x$which))

prop.table(table(train$which))
prop.table(table(test$which))

# prop.table(table(test.20$which))


# check fraction of overlap between train and test (84%)
# length(intersect(subset.20, train_sample))/200



# based on results from Kendall Tau correlation
# modeling soil properties
soilprop.fit <- rpart(which ~ silica_ammonium_oxalate + cec_nh4_ph_7, 
                      data = train, method = "class")

soilprop.fit
summary(soilprop.fit)

## visualize
rpart.plot(soilprop.fit)
soilprop.fit$variable.importance


# ## prune regression tree
printcp(soilprop.fit)
bestcp <- soilprop.fit$cptable[which.min(soilprop.fit$cptable[,"xerror"]),"CP"]
bestcp

## prune tree with best cp value (0.01)
pruned.tree <- prune(soilprop.fit, cp = bestcp)

## visualize the pruned tree
prp(pruned.tree, extra = 6, box.palette = "Blues", faclen = 0)
pruned.tree
summary(pruned.tree)

# order of importance - variables
pruned.tree$variable.importance




## Evaluate performance of pruned regression tree
# Use the test data to evaluate performance of pruned regression tree on test data
pred.prune.test = predict(pruned.tree, test, type = "class")

pred.prune.train = predict(pruned.tree, train, type = "class")

# generate the confusion matrix
library(caret)

confusionMatrix(pred.prune.train, train$which)

confusionMatrix(pred.prune.test, test$which)

# print
write.csv(train, file = "data/soil-prop-train-model3.csv")
write.csv(test, file = "data/soil-prop-test-model3.csv")

saveRDS(train, file = "data/soil-prop-train-model3.rds")
saveRDS(test, file = "data/soil-prop-test-model3.rds")






























