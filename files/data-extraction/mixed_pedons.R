## Get "mixed" pedon/lab data, adapted from R. Hodges original
## D.E. Beaudette and R. Hodges
## 2024-01-05

library(soilDB)
library(aqp)
library(purrr)

## cached SC database
s <- readRDS('data/SC.rds')

# remove inactive series
s <- subset(s, soilseriesstatus != 'inactive')

# mixed mineralogy class
idx_m <- grep('mixed', s$taxclname, ignore.case = TRUE)
s_m <- s[idx_m, ]

# 16745
nrow(s_m)

sort(table(s_m$taxorder))


## only interested in mixed soils from mineralogy sections (D) and (E)
# remove (A) oxisols
s_m_a <- s_m[!s_m$taxorder == "oxisols", ]

# remove kandi and kanhapl
idx <- grep('kandi|kanhapl', s_m_a$taxgrtgroup, invert = TRUE)
s_m_a2 <- s_m_a[idx, ]

sort(table(s_m_a2$taxminalogy))


# remove (B) soils that have a substitute class that replaces the particle 
# size class, other than fragmental
idx <- grep('ashy|medial|hydrous|diatomaceous|gypseous', s_m_a2$taxpartsize, invert = TRUE)
s_m_b <- s_m_a2[idx, ]

sort(table(s_m_b$taxminalogy))


# remove (C) other mineral soils and soils in the Terric subgroups of Histosols and Histels
# this step is unnecessary as there is no mixed class in group (C)

# no additional filter and removal is required for particle size class!

## save intermediate results for later
saveRDS(s_m_b, file = "data/mixed-series.rds")


## get lab pedons matching these series

# 16410 series
smb2series <- tolower(trimws(unique(s_m_b$soilseriesname)))
chunks <- makeChunks(smb2series, 1000)

# split series name vector into pieces for chunk-wise fetching of data
.seriesList <- split(smb2series, chunks)

# get a chunk-worth of lab pedon data
getLabPedons <- function(i) {
  .sql <- sprintf("
SELECT
pedon_key, pedlabsampnum, pedoniid, upedonid, longitude_decimal_degrees, latitude_decimal_degrees, samp_name, corr_name,
corr_taxpartsize
FROM
lab_combine_nasis_ncss
WHERE CASE WHEN corr_name IS NOT NULL THEN LOWER(corr_name) ELSE LOWER(samp_name) END IN %s;"
, format_SQL_in_statement(gsub("'", "", i)))
  
  .res <- SDA_query(.sql)
  return(.res)
}

# retrieve pedon lab data by chunk, result is a data.frame
# ~ 2 minutes
mixed_pedons <- map_df(.seriesList, .f = getLabPedons, .progress = TRUE)

# 32,972 pedons
nrow(mixed_pedons)

# keep only those with coordinates
mixed_pedons.sub <- subset(
  mixed_pedons, 
  subset = !is.na(longitude_decimal_degrees) & !is.na(latitude_decimal_degrees)
)

# 28,632 pedons
nrow(mixed_pedons.sub)

# iterate over chunks of pedon_key
chunks <- makeChunks(mixed_pedons$pedon_key, 1000)

# split series name vector into pieces for chunk-wise fetching of data
# 33 chunks
.pkList <- split(mixed_pedons$pedon_key, chunks)

getLabHorizons <- function(i) {
  .pk <- format_SQL_in_statement(i)
  
  .sql <- sprintf("
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
;", .pk)
  
  .res <- SDA_query(.sql)
  return(.res)
}

# iterate over chunks of lab horizon data
# ~ 1 minute
mixed_hz <- map_df(.pkList, .f = getLabHorizons, .progress = TRUE)

# 154886 horizons
nrow(mixed_hz)

# curious how many pedons hz has
# 25364 pedons
length(unique(mixed_hz$pedon_key))

colSums(!is.na(mixed_hz))

# merge with coordinates
mixed_hz <- merge(mixed_hz, mixed_pedons.sub, by = 'pedon_key', all.x = TRUE, sort = FALSE)

# save pedon + horizon level properties data to folder
saveRDS(mixed_hz, file = "data/mixed_pedons_properties.rds")


## cleanup
rm(list = ls())
gc(reset = TRUE)


