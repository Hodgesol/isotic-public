
library(soilDB)
library(aqp)

library(lattice)
library(latticeExtra)
library(tactile)

library(DBI)
library(RSQLite)

library(ggdist)
library(ggplot2)
library(patchwork)


# connect
db <- dbConnect(RSQLite::SQLite(), 'C:/NASIS_KSSL/ncss_labdatasqlite/ncss_labdata.sqlite')

dbListTables(db)


dbListFields(db, 'lab_layer')
dbListFields(db, 'lab_pedon')
dbListFields(db, 'lab_combine_nasis_ncss')
dbListFields(db, 'lab_chemical_properties')


qq <- "SELECT peiid, pedon_key, pedlabsampnum, samp_taxminalogy, corr_taxminalogy, SSL_taxminalogy FROM lab_combine_nasis_ncss;"
x <- dbGetQuery(db, qq)

str(x)
head(x, 50)

x$min <- x$corr_taxminalogy

# use sampled as when correlated as and SSL are missing
idx <- which(is.na(x$corr_taxminalogy) & is.na(x$SSL_taxminalogy))
x$min[idx] <- x$samp_taxminalogy[idx]

idx <- which(is.na(x$min) & !is.na(x$SSL_taxminalogy))
x$min[idx] <- x$SSL_taxminalogy[idx]
table(x$min, useNA = 'always')

x <- x[, c('pedon_key', 'peiid', 'pedlabsampnum', 'min')]
x <- na.omit(x)

str(x)
head(x)

sort(table(x$min))

.is <- format_SQL_in_statement(x$pedon_key)

qq <- sprintf("
SELECT
l.pedon_key, l.labsampnum AS labsampnum, hzn_top, hzn_bot, hzn_desgn,
sand_total AS sand, silt_total AS silt, clay_total AS clay, cole_whole_soil AS cole,
cec_nh4_ph_7, ecec_base_plus_aluminum, cec7_clay_ratio,
le_third_ovendry_lt_2_mm AS lep_thirdbar_to_od,
le_third_fifteen_lt2_mm AS lep_thirdbar_to_fifteenbar,
ca_to_mg_ratio, new_zealand_phosphorus_retent, fe_ammoniumoxalate_extractable, silica_ammonium_oxalate
FROM lab_layer AS l 
INNER JOIN lab_chemical_properties AS c ON l.labsampnum = c.labsampnum 
INNER JOIN lab_physical_properties AS p ON l.labsampnum = p.labsampnum
WHERE l.pedon_key IN %s
;
", .is)

# run query
y <- dbGetQuery(db, qq)

# close connection
dbDisconnect(db)


z <- merge(x, y, by = 'pedon_key', sort = FALSE)


z <- z[grep('B', z$hzn_desgn), ]

str(z)

tab <- sort(table(z$min), decreasing = TRUE)
names(tab)

ll <- names(tab)[1:20]

ll <- c('mixed', 'kaolinitic', 'amorphic', 'halloysitic', 'ferrihydritic', 'isotic', 'glassy')



zz <- subset(z, subset = min %in% ll)

zz$min <- factor(
  zz$min,
  levels = ll
)


tps <- tactile.theme()


bwplot(min ~ ecec_base_plus_aluminum, data = zz, par.settings = tactile.theme(), xlab = 'CEC at pH7 (cmol[+]/kg)')

bwplot(min ~ cole, data = zz, par.settings = tactile.theme(), xlab = 'COLE (whoil soil)', scales = list(x = list(log = 10)), xscale.components = xscale.components.log10ticks)

bwplot(min ~ fe_ammoniumoxalate_extractable, data = zz, par.settings = tactile.theme(), xlab = 'Fe_o', scales = list(x = list(log = 10)), xscale.components = xscale.components.log10ticks, subset = fe_ammoniumoxalate_extractable < 50)

bwplot(min ~ silica_ammonium_oxalate, data = zz, par.settings = tactile.theme(), xlab = 'Si_ao', scales = list(x = list(log = 10)), xscale.components = xscale.components.log10ticks, subset = silica_ammonium_oxalate < 50)

bwplot(min ~ ca_to_mg_ratio, data = zz, par.settings = tactile.theme(), xlab = 'Ca:Mg', subset = ca_to_mg_ratio < 50)


bwplot(min ~ lep_thirdbar_to_fifteenbar, data = zz, par.settings = tactile.theme(), xlab = 'LEP (1/3bar to 15bar)', scales = list(x = list(log = 10)), xscale.components = xscale.components.log10ticks)

bwplot(min ~ lep_thirdbar_to_od, data = zz, par.settings = tactile.theme(), xlab = 'LEP (1/3 bar to OD)', scales = list(x = list(log = 10)), xscale.components = xscale.components.log10ticks)


bwplot(min ~ cec_nh4_ph_7, data = zz, par.settings = tactile.theme(), xlab = 'CEC at pH7 (cmol[+]/kg)')

bwplot(min ~ cec_nh4_ph_7, data = zz, scales = list(x = list(log = 10)), xscale.components = xscale.components.log10ticks, par.settings = tactile.theme(), xlab = 'CEC at pH7 (cmol[+]/kg)')

bwplot(min ~ cec_nh4_ph_7 / clay, data = zz, scales = list(x = list(log = 10)), xscale.components = xscale.components.log10ticks, par.settings = tactile.theme(), xlab = 'CEC7:Clay Ratio, <2mm Fraction')






# manually filter 0
# <2mm Fraction, B horizons
p1 <- ggplot(zz, aes(x = cec_nh4_ph_7, y = min)) +
  stat_interval(inherit.aes = TRUE, orientation = 'horizontal') + 
  theme_minimal() +
  theme(legend.position = c(1, 1), legend.justification ='right', legend.direction	
        = 'horizontal', legend.background = element_rect(fill = 'white', color = NA), axis.text.y = element_text(face = 'bold'), plot.title = element_text(hjust = 0.5)) + 
  stat_summary(geom = 'point', fun = median, shape = 21, fill = 'black', col = 'white', cex = 2) +
  scale_color_brewer(palette = 'Greens') + 
  scale_x_continuous(n.breaks = 16) +
  xlab('CEC by Ammonium Acetate at pH 7 (cmol[+]/kg)') + ylab('') +
  labs(title = 'Pedon Mineralogy Class (Soil Taxonomy), KSSL Snapshot', color = 'Interval')


p2 <- ggplot(zz, aes(x = new_zealand_phosphorus_retent, y = min)) +
  stat_interval(inherit.aes = TRUE, orientation = 'horizontal') + 
  theme_minimal() +
  theme(legend.position = c(1, 1), legend.justification ='right', legend.direction	
        = 'horizontal', legend.background = element_rect(fill = 'white', color = NA), axis.text.y = element_text(face = 'bold')) + 
  stat_summary(geom = 'point', fun = median, shape = 21, fill = 'black', col = 'white', cex = 2) +
  scale_color_brewer() + 
  scale_x_continuous(n.breaks = 16) +
  xlab('NZ Phosphorus Retention (%)') + ylab('') +
  labs(color = 'Interval')


p3 <- ggplot(zz, aes(x = silica_ammonium_oxalate, y = min)) +
  stat_interval(inherit.aes = TRUE, orientation = 'horizontal') + 
  theme_minimal() +
  theme(legend.position = c(1, 1), legend.justification ='right', legend.direction	
        = 'horizontal', legend.background = element_rect(fill = 'white', color = NA), axis.text.y = element_text(face = 'bold')) + 
  stat_summary(geom = 'point', fun = median, shape = 21, fill = 'black', col = 'white', cex = 2) +
  scale_color_brewer(palette = "Reds") + 
  scale_x_continuous(n.breaks = 16) +
  xlab('Si (AO; %)') + ylab('') +
  labs(color = 'Interval')

p1 / p2 / p3







