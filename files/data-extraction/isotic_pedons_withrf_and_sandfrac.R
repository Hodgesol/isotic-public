## Get "isotic" pedon/lab data, adapted from R. Hodges original
## D.E. Beaudette and R. Hodges
## 2024-04-19

library(soilDB)
library(aqp)

## effervescence class for all series, via OSDs
eff_class <- readRDS('data/osd-eff-class-interpretation.rds')
# eff_class <- `osd-eff-class-interpretation`

# 23152 series --> 23346
nrow(eff_class)

## cached SC database
s <- readRDS('data/SC.rds')

# remove inactive series
s <- subset(s, soilseriesstatus != 'inactive')

# combine with eff class
s <- merge(s, eff_class, by.x = 'soilseriesname', by.y = 'series', all.x = TRUE, sort = FALSE)

sort(table(s$taxminalogy))

# find possible isotic series
idx <- grep('isotic', s$taxclname, ignore.case = TRUE)
s <- s[idx, ]

# 1575 --> 1612
nrow(s)

sort(table(s$taxorder))

# note that there are some unknown eff class series
table(s$eff, useNA = 'always')

# keep only those series with eff == FALSE
s <- subset(s, !eff)

# 1403 series --> 1407
nrow(s)

## get pedon data from LDM snapshot

sql_isotic <- sprintf("
SELECT 
pedon_key, pedlabsampnum, pedoniid, upedonid, longitude_decimal_degrees, latitude_decimal_degrees, samp_name, corr_name, corr_taxorder, corr_taxsuborder, corr_taxgrtgroup, corr_taxsubgrp
FROM
lab_combine_nasis_ncss
WHERE CASE WHEN corr_name IS NOT NULL THEN LOWER(corr_name) ELSE LOWER(samp_name) END IN %s"
                      , format_SQL_in_statement(tolower(s$soilseriesname)))

# run query
sda_isotic <- SDA_query(sql_isotic)


## keep only those with coordinates
isoticpedons.sub <- subset(sda_isotic, subset = !is.na(longitude_decimal_degrees) & !is.na(latitude_decimal_degrees))

# 1641 --> 1642
nrow(isoticpedons.sub)

# make a quoted vector of pedon keys, for use in the next query
isoticpedons_sub.keys <- format_SQL_in_statement(isoticpedons.sub$pedon_key)

sql_isotic <- sprintf("
SELECT
l.pedon_key, l.labsampnum, hzn_top, hzn_bot, hzn_desgn,
sand_total, silt_total, clay_total, particle_size_method,
texture_description, texture_lab,
ph_naf,
bulk_density_third_bar, bulk_density_third_bar_method,
water_15_bar_to_clay_ratio,
new_zealand_phosphorus_retent, 
aluminum_ammonium_oxalate, fe_ammoniumoxalate_extractable,
aluminum_plus_half_iron_oxalat,
silica_ammonium_oxalate,
sand_coarse, sand_very_coarse, sand_fine, sand_medium, sand_very_fine,
wf_2075, wf_25, wf_520,
cec_nh4_ph_7, water_retention_15_bar, water_retention_third_bar, 
clay_mineral_interpretation
FROM
lab_layer AS l
JOIN lab_physical_properties AS p ON l.labsampnum = p.labsampnum
JOIN lab_chemical_properties AS c ON l.labsampnum = c.labsampnum
JOIN lab_calculations_including_estimates_and_default_values AS ce ON l.labsampnum = ce.labsampnum
LEFT JOIN lab_xray_and_thermal AS x ON l.labsampnum = x.labsampnum
WHERE l.pedon_key IN %s
ORDER BY l.pedon_key, hzn_top
;", isoticpedons_sub.keys)


# get
hz_isotic <- SDA_query(sql_isotic)

# 7408 horizons (without sand fractions and rock fragments); 6401 --> 7061 with
nrow(hz_isotic)

# curious how many pedons hz has (1,411 pedons without; 1313 --> 1314 with)
length(unique(hz_isotic$pedon_key))

colSums(!is.na(hz_isotic))

# merge with coordinates
hz_isotic <- merge(hz_isotic, isoticpedons.sub, by = 'pedon_key', all.x = TRUE, sort = FALSE)

# save pedon + horizon level properties data to folder
saveRDS(hz_isotic, file = "data/isotic_pedons_properties_rf_sand.rds")

## cleanup
rm(list = ls())
gc(reset = TRUE)










