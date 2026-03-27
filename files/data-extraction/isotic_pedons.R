## Get "isotic" pedon/lab data, adapted from R. Hodges original
## D.E. Beaudette and R. Hodges
## 2024-01-05

library(soilDB)
library(aqp)

## effervescence class for all series, via OSDs
eff_class <- readRDS('data/osd-eff-class-interpretation.rds')

# 23152 series
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

# 1563
nrow(s)

sort(table(s$taxorder))

# note that there are some unknown eff class series
table(s$eff, useNA = 'always')

# keep only those series with eff == FALSE
s <- subset(s, !eff)

# 1403 series
nrow(s)

## get pedon data from LDM snapshot

sql_isotic <- sprintf("
SELECT 
pedon_key, pedlabsampnum, pedoniid, upedonid, longitude_decimal_degrees, latitude_decimal_degrees, samp_name, corr_name,
corr_taxpartsize, taxonomic_order
FROM
lab_combine_nasis_ncss
WHERE CASE WHEN corr_name IS NOT NULL THEN LOWER(corr_name) ELSE LOWER(samp_name) END IN %s"
, format_SQL_in_statement(tolower(s$soilseriesname)))

# run query
sda_isotic <- SDA_query(sql_isotic)


## keep only those with coordinates
isoticpedons.sub <- subset(sda_isotic, subset = !is.na(longitude_decimal_degrees) & !is.na(latitude_decimal_degrees))

# 1629
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
aluminum_ammonium_oxalate, aluminum_na_pyro_phosphate,
aluminum_plus_half_iron_oxalat,
silica_ammonium_oxalate
FROM
lab_layer AS l
JOIN lab_physical_properties AS p ON l.labsampnum = p.labsampnum
JOIN lab_chemical_properties AS c ON l.labsampnum = c.labsampnum
WHERE l.pedon_key IN %s
ORDER BY l.pedon_key, hzn_top
;", isoticpedons_sub.keys)


# get
hz_isotic <- SDA_query(sql_isotic)

# 7408 horizons (without sand fractions and rock fragments); 6401 with
nrow(hz_isotic)

# curious how many pedons hz has (1,411 pedons without; 1313 with)
length(unique(hz_isotic$pedon_key))

colSums(!is.na(hz_isotic))

# merge with coordinates
hz_isotic <- merge(hz_isotic, isoticpedons.sub, by = 'pedon_key', all.x = TRUE, sort = FALSE)

# save pedon + horizon level properties data to folder
saveRDS(hz_isotic, file = "data/isotic_pedons_properties.rds")

## cleanup
rm(list = ls())
gc(reset = TRUE)










