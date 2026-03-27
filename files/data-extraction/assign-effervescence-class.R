library(aqp)
library(soilDB)
library(purrr)
library(furrr)


# rebuild flag
reBuild <- FALSE

if(reBuild) {
  
  # load cached SC database
  sc <- readRDS('data/SC.rds')
  
  # all established soil series names
  s <- sc$soilseriesname[which(sc$soilseriesstatus == 'established')]
  
  # test effervescence class
  # singe parsed OSD at a time
  testEffClass <- function(i) {
    
    # there may be no data for a given series
    # return NULL
    # these series are ignored / no result
    if(length(i$HORIZONS) < 1) {
      return(NULL)
    }
    
    # extract eff class data for all horizons
    .e <- i$HORIZONS[[1]]$eff_class
    
    # remove NA, may result in length 0
    .e <- na.omit(.e)
    
    # no non-NA matches, likely no carbonates
    if(length(.e) < 1) {
      # all NA
      .res <- FALSE
    } else {
      
      # possibly more criteria here, such as effervescence class > threshold
      # or in B horizons
      
      # simplest possible test: 
      # exclude when all, non-NA, horizons are noneffervescent
      if(all(.e == 'noneffervescent')) {
        .res <- FALSE
      } else {
        # some effervescence in profile
        .res <- TRUE
      }
      
    }
    
    # TODO: maybe list reason for TRUE/FALSE
    
    # pack into a data.frame for simple use later
    .d <- data.frame(series = i$SERIES[1], eff = .res)
    return(.d)
  }
  
  
  
  # setup parallel processing
  plan(multisession, workers = 8)
  
  
  # load selected OSDs from local JSON
  # ~ 1 minutes for all established soil series
  # adjust local URL to SKB working copy
  system.time(
    osds <- future_map(
      s,
      .progress = TRUE,
      .f = get_OSD, 
      base_url = 'e:/working_copies/SoilKnowledgeBase/inst/extdata/OSD'
    )
  )
  
  
  ## tests
  # all noneffervescent
  testEffClass(osds[[which(s == 'TOADTOWN')]])
  testEffClass(osds[[which(s == 'FRAZWELL')]])
  
  # no eff class included in typical pedon narrative
  # all NA
  testEffClass(osds[[which(s == 'AMADOR')]])
  testEffClass(osds[[which(s == 'CECIL')]])
  
  # some horizons with eff class > noneffervescent
  testEffClass(osds[[which(s == 'PIERRE')]])
  
  
  ## shutdown parallel back-ends
  plan(sequential)
  
  # apply test / criteria to all parsed soil series
  # single-threaded
  # result is a data.frame
  x <- map_df(osds, .f = testEffClass, .progress = TRUE)
  
  # check: ok
  head(x)
  table(x$eff)
  
  ## interpretation notes:
  # 1. NA -> FALSE could mean 
  #  - "unable to parse due to poorly formatted text"
  #  - "eff class not given"
  # 
  # 2. number / thickness of effervescent horizons not part of criteria
  # 
  
  ## save
  # RDS
  saveRDS(x, file = 'data/osd-eff-class-interpretation.rds')
  
  # write to CSv
  # write.csv(x, file = 'data/eff_series.csv')
  
}

# cleanup
rm(list = ls())
gc(reset = TRUE)



