library(soilDB)

## latest SC database
sc <- get_soilseries_from_NASIS()

## save
saveRDS(sc, file = 'data/SC.rds')

write.csv(sc, file = 'data/SC.csv', row.names = FALSE)

## cleanup
rm(list = ls())
gc(reset = TRUE)
