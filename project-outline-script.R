## Isotic research step-wise script.
## R. Hodges and D. Beaudette
## July 28, 2025


# set wd
setwd(".../isotic/clean-project-files-extendedwork")

############################################################################  
## Section A. Data extraction.

## A.1. prepare local SC database copy
source('files/data-extraction/prepare-SC-DB-copy.R')

  ## produces:
  readRDS('data/SC.rds')
  # csv: 'data/SC.csv'

## A.2. assign effervescence class via OSDs
# this contains an conditional reBuild flag, since it relies on local files
# in the future, use the OSD database directly
source('files/data-extraction/assign-effervescence-class.R')
  
  ## produces:
  readRDS('data/osd-eff-class-interpretation.rds')

## A.3. identify isotic soil series and get lab data
source('files/data-extraction/isotic_pedons.R')
source('files/data-extraction/isotic_pedons_withrf_and_sandfrac.R')

  ## produces:
  readRDS('data/isotic_pedons_properties.rds')
  readRDS('data/isotic_pedons_properties_rf_sand.rds')

## 4. identify mixed soil series and get lab data
source('files/data-extraction/mixed_pedons.R')
source('files/data-extraction/mixed_pedons_with_rf_and_sandfrac.R')

  ## produces:
  readRDS('data/mixed_pedons_properties.rds')
  readRDS('data/mixed_pedons_properties_with_rf_sandfrac.rds')

## 5. combine mixed + isotic, then subset lab data based on completeness
source('files/data-extraction/lab-data-filtering.R')
source('files/data-extraction/lab-data-filtering-w-rf-and-sandfrac.R')

  ## produces:
  readRDS('data/combined-lab-wt-mean.rds')
  readRDS('data/combined-lab-wt-mean-with-rf-and-sandfrac.rds')

## 6. overlay with spatial data
source('files/data-extraction/spatial-overlay.R')
source('files/data-extraction/spatial-overlay-with-rf-and-sandfrac.R')

  ## produces:
  readRDS('data/results.rds')
  readRDS('data/final_pedon_climate_data.rds')
  readRDS('data/results-with-rf-and-sandfrac.rds')
  readRDS('data/final_pedon_climate_data-with-rf-and-sandfrac.rds')

  
############################################################################  
## Section B. Defining sandy and sandy-skeletal pedons

## B.1. Define sandy subclasses using results-with-rf-and-sandfrac.rds
source('files/defining-sandy/sandy-subclasses.R')

  ## produces:
  readRDS('data/pedons-with-s-ls-sl-subclasses.rds')

## B.2. Define particle size class for pedons
source('files/defining-sandy/particle-size-class-sandy-and-sandyskeletal.R')

  ## produces:
  readRDS('data/pedons-with-sandy-and-skeletal-psc.rds')

  
############################################################################
## Section C. Creating general figures
## C.1. interpret using either 'results.rds' OR 'pedons-with-sandy-and-skeletal-psc.rds'
## Here is where sandy and sandy-skeletal pedons are removed from the dataset...
source('files/gen-figs/general-figures.R')
source('files/gen-figs/general-figures-with-sandy-data.R')

  ## produces:
  readRDS('data/results-without-sandy-and-skeletal.rds')

  
############################################################################  
## Section D. Correlation assessments and plots

## D.1. soil property and environmental correlations
## use 'results-without-sandy-and-skeletal.rds'
source('files/property.correlation.R')
  

############################################################################  
## Section E. Logistic Regression Analyses

## E.1. Soil Properties  
source('files/regression/logistic-regression-soil-prop.R')
  
## E.2. Environmental Covariates
source('files/regression/log-regression-env-cov.R')
  

############################################################################  
## Section F. NMDS development and pedon extraction for dataset without 
## sandy/sandy-skeletal pedons. Use 'results-without-sandy-and-skeletal.rds'

## F.1. Soil properties NMDS
source('files/nmds/soilprop-NMDS-without-sandy.R')
  
  ## produces:
  readRDS('data/isotic-mixed-nosand-90-dataset.rds')
  
## F.2. Environmental covariates NMDS
source('files/nmds/env-cov-NMDS-without-sandy.R')
  
  ## produces:
  readRDS('data/isotic-mixed-nosand-90-dataset-forenvtree.rds')
  
############################################################################  
## Section G. Regression Tree Statistics

## G.1. Soil properties decision tree
source('files/regression/regression-tree-soil-props.R')
  
  ## produces:
    # "data/soil-prop-train.csv"
    # "data/soil-prop-test.csv"
    # "data/soil-prop-train.rds"
    # "data/soil-prop-test.rds"
  
## G.2. Environmental covariates decision tree
source('files/regression/regression-tree-env-cov.R')
  
  ## produces:
    # "data/env-cov-train.csv"
    # "data/env-cov-test.csv"
    # "data/env-cov-train.rds"
    # "data/env-cov-test.rds"
  
############################################################################  
## Section H. Theoretical Reclassification of dataset pedons based on produced
## decision trees.

# H.1. Soil property decision tree reclassification
source('files/theoretical-reclass/theoretical-isotic-from-pruned-soilprop-tree-USmapdataset.R')

# H.2. Environmental covariates decision tree reclassification
source('files/theoretical-reclass/theoretical-isotic-from-pruned-envcov-tree-USmapdataset.R')


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  