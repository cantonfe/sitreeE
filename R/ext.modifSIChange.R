 ## needed to run the models
ff <- function(
               mod2, 
               j2,
               alpha,
               waterbal){
  my.waterbal <- ifelse(waterbal >= j2, waterbal - j2, 0)
  mia <- my.waterbal/alpha
  
  return(
    mod2/alpha *
      mia ^ (mod2 - 1) *
      exp(-mia ^ mod2)
  )
}

my.f.si <- function(mod2,j2,alpha,waterbal){
  my.max <-
    optimize(
      f        = ff,
      mod2     = mod2,
      j2       = j2, 
      alpha    = alpha,
      interval = c(min(waterbal), max(waterbal)),
      maximum  = TRUE)
  fnl <- (
    1/my.max$objective * ff(mod2,
                            j2,
                            alpha,
                            waterbal)
  )
  return(fnl)
}

ext.modif.fun <- function(
                          tr,
                          fl,
                          this.period,
                          i.period,
                          ...){
  
  others <- list(...) ## clim.scenario, year.start.simulation
  clim.scenario         <- others$clim.scenario
  year.start.simulation <- others$year.start.simulation
  
 

  if (this.period == 't0') return (NULL)
  
  if (clim.scenario == 'nochange') {
    return(NULL)
  } else {
    if (clim.scenario %in% c('rcp26', 'rcp45')){
    
      ## we need historical climate and current climate to calculate the
      ## difference in SI

      ## SOIL CLASSIFICATION IN SITE QUALITIES
      ## what to do when missing vegetation type
      if (sum(is.na(fl$veg.type)) > 0) stop('missing vegetation type')
      veg.type.A <- veg.type.A.calc(fl$veg.type)
      
      ## missing soil.depth.1234 --- we will assume 2
      fl$soil.depth.1234[is.na(fl$soil.depth.1234)] <- 2
      ## missing veg.type.A we will assume med med
      veg.type.A[is.na(veg.type.A)] <- 'MediumMedium'
      soilquality <- soilquality.calc(veg.type.A, fl$soil.depth.1234)
      soilquality[soilquality == 'Best' & fl$SI.spp %in% c(2,3)] <- 'AlmostBest'

      klim.h.mid <- klim.h.mid[klim.h.mid$flateid %in% substr(fl$ustandID,1,6),]
      

      ## CALCULATE CLIMATIC VARIABLES NEEDED
      ## Historical
      ## lat.deg same length as the rest

      lat.deg <- fl$lat.deg[match(klim.h.mid$flateid, substr(fl$ustandID,1,6))]
      si.clim.chang.vars <- with(
        klim.h.mid,
        si.change.clim.var(flateid, mid.temp.C, mid.pp.mm, mo, lat.deg))
      ## to make it match values in fl
      si.clim.chang.vars <- si.clim.chang.vars[match(substr(fl$ustandID,1,6),
                                                     si.clim.chang.vars$flateid),]
      si.clim.chang.vars$ustandID <- fl$ustandID
      
      ## Calculate historical SI

      ##
      i.spruce <- fl$SI.spp == 1
      i.pine   <- fl$SI.spp == 2
      i.lauv   <- fl$SI.spp == 3
      SI.hist <- rep(NA, length(soilquality))

      soilquality [soilquality == "Poorest" & i.spruce] <- "Average" ## spruce
      soilquality [soilquality == "Best"    & i.pine] <- "AlmostBest"
      soilquality [soilquality == "Poorest" & i.lauv] <- "Average"

     
      if (sum(i.spruce) > 0){
        SI.hist[i.spruce] <-
          predict(mod.spruce,
                  newdata = data.frame(
                    soilquality = factor(soilquality[i.spruce]),
                    t.early.summer = si.clim.chang.vars$t.early.summer[i.spruce]
                  )
                )
      }
      if (sum(i.pine) > 0){
      SI.hist[i.pine] <-
        predict(mod.pine,
                newdata = data.frame(
                  soilquality = factor(soilquality[i.pine]),
                  t.early.summer =
                    si.clim.chang.vars$t.early.summer[i.pine],
                  waterbal =
                    si.clim.chang.vars$waterbal[i.pine]
                 )
                )}
      if (sum(i.lauv) > 0){
        SI.hist[i.lauv] <-
          predict(mod.birch,
                  newdata = data.frame(
                    soilquality = factor(soilquality[i.lauv]),
                    t.early.summer = si.clim.chang.vars$t.early.summer[i.lauv],
                    waterbal = si.clim.chang.vars$waterbal[i.lauv]
                  )
                  )
      }
      
######################
      ## Calculate future SI
######################
      klima.prognoser <- klima.prognoser[flateid %in% substr(fl$ustandID,1,6),]
      ## which years should we use?
      ## my.env <- new.env()
      ## load(file.import, envir = my.env)
      ## sesons.to.use <- my.env$fl[, c('FLATEID', 'SESONG')]
      ## names(sesons.to.use) <- tolower(names(sesons.to.use))

      r.season <- (year.start.simulation: (year.start.simulation+ 4)) + ##unique(sesons.to.use$sesong) +
        i.period*5

      ## if r.season > 2100 just use the last one, we assume it remains the same
      if (min(r.season) > 2106) r.season <- 2111:(2111 + 4)
      
      klima.prognoser.now <- klima.prognoser[sesong %in% r.season]
      lat.deg <- fl$lat.deg[match(klima.prognoser.now$flateid,
                                  substr(fl$ustandID,1,6))]
      si.clim.chang.vars <- with(
        klima.prognoser.now,
        si.change.clim.var(flateid, temp.C, pp.mm, mo, lat.deg))
      ## to make it match values in fl
      si.clim.chang.vars <- si.clim.chang.vars[match(substr(fl$ustandID, 1, 6),
                                                     si.clim.chang.vars$flateid),]
      si.clim.chang.vars$ustandID <- fl$ustandID
      
      
      SI.prog <- rep(NA, length(soilquality))
      if (sum(i.spruce) > 0){  
        SI.prog[i.spruce] <-
          predict(mod.spruce,
                  newdata = data.frame(
                    soilquality = factor(soilquality[i.spruce]),
                    t.early.summer = si.clim.chang.vars$t.early.summer[i.spruce])
                  )
      }
      if (sum(i.pine) > 0){     
        SI.prog[i.pine] <-
          predict(mod.pine,
                  newdata = data.frame(
                    soilquality = factor(soilquality[i.pine]),
                    t.early.summer = si.clim.chang.vars$t.early.summer[i.pine],
                    waterbal       = si.clim.chang.vars$waterbal[i.pine]
                  )
                  )
      }
      if (sum(i.lauv) > 0){
        SI.prog[i.lauv] <-
          predict(mod.birch,
                  newdata = data.frame(
                    soilquality = factor(soilquality[i.lauv]),
                    t.early.summer = si.clim.chang.vars$t.early.summer[i.lauv],
                    waterbal       = si.clim.chang.vars$waterbal[i.lauv]
                  )
                  )
      }
      
      SI.prognosis <- fl$SI.m + (SI.prog - SI.hist)

      ## And now we need to classify it
      SI.prognosis[SI.prognosis < 6] <- 6
      SI.prognosis[SI.prognosis > 30] <- 30
      
      SI.cuts <- c(5, seq(6.5, 26, by=3), 31)
      my.si <- unique(fl$SI.m)
      my.si <- my.si[order(my.si)]
      sa <- as.character(cut(SI.prognosis, breaks = SI.cuts, labels = my.si))

      
      return(list(SI.m = as.numeric(sa)))
    } else stop ('clim.scenario no valid')
  }
}
## reassignInPackage("ext.modif.fun", "sitreeE", ext.modif.fun)
