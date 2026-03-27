## property/covariate correlation statistics
## R. Hodges and D. Beaudette
## Oct 25, 2024
## 

## cleanup
rm(list = ls())
gc(reset = TRUE)

# load dataset
x <- readRDS('data/results-without-sandy-and-skeletal.rds')

# load libraries
library(dplyr)


# create dataset of JUST soil properties from "x"
x_sel <- subset(x, select = c(ph_naf, water_15_bar_to_clay_ratio, new_zealand_phosphorus_retent,
                              aluminum_ammonium_oxalate, silica_ammonium_oxalate,
                              aluminum_plus_half_iron_oxalat, cec_nh4_ph_7))

## correlation of soil properties
library(corrplot)
crm <- cor(x_sel, use="everything", method="kendall")

colnames(crm) <- c("NaF pH", "15 bar/clay", "NZ-P", "Al (AO)", "Si (AO)", "Al + 1/2Fe (AO)", "CEC7")

rownames(crm) <- c("NaF pH", "15 bar/clay", "NZ-P", "Al (AO)", "Si (AO)", "Al + 1/2Fe (AO)", "CEC7")

cor.sp <- corrplot(crm, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")



############################################################################
## now do the same for the environmental covariates
env_sel <- subset(x, select = c(effective_precipitation_800m, final_MAP_mm_800m,
                  annualNPP, final_MAAT_800m, et_rast, andisols.and.andic.subgroups))

## correlation of environmental properties
crm.e <- cor(env_sel, use = "pairwise.complete.obs", method = "kendall")

colnames(crm.e) <- c("Eff. Precip.", "MAP", "Annual NPP", "MAAT", "aET", "Andisols/Andic")

rownames(crm.e) <- c("Eff. Precip.", "MAP", "Annual NPP", "MAAT", "aET", "Andisols/Andic")

cor.ep <- corrplot(crm.e, method = "circle", type = "upper", 
                  tl.col = "black", tl.srt = 45, addCoef.col = "black")

# based on results, effective precip and map correlated at 0.88. removing MAP from model
env_sel.2 <- subset(x, select = c(effective_precipitation_800m,
                                annualNPP, final_MAAT_800m, et_rast, andisols.and.andic.subgroups))

## correlation of environmental properties
crm.e.2 <- cor(env_sel.2, use = "pairwise.complete.obs", method = "kendall")

colnames(crm.e.2) <- c("Eff. Precip.", "Annual NPP", "MAAT", "aET", "Andisols/Andic")

rownames(crm.e.2) <- c("Eff. Precip.", "Annual NPP", "MAAT", "aET", "Andisols/Andic")

cor.ep.2 <- corrplot(crm.e.2, method = "circle", type = "upper", 
                  tl.col = "black", tl.srt = 45, addCoef.col = "black")









