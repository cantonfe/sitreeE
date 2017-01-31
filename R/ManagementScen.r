## Management following the scenarios defined by Birger and Hanne

management.scen <- function(
                            tr = tr,
                            fl = fl,
                            common.vars = common.vars,
                            this.period = this.period,
                            next.period = next.period,
                            i.period = i.period,
                            mng.options    = mng.options,
                            ...){
  ## we treat anything with SI > 23 as 23
  fl$SI.m[fl$SI.m > 23] <- 23
  others <- list(...)
  
  mic.specification <- mng.options$current.mics


  ## calculate the development class
  devel.class <-  data.frame(
    SI.m = rep(c(26,23,20,17,14,11,8,6),2),
    spp     = rep(c("conif","birch"),each=8),
    class3 = c(
      20, 20, 20, 25, 30, 35, 45, 55,
      15,15,15,20,25,25,25,30),
    class5 = c(
      60, 60, 70, 80, 90,100,110,120,
      40,40,50,60,70,70,70,80)
  )

  devel.class$ID <- with(devel.class, paste0(spp, "-", SI.m))
  my.spp <- as.character(fl$SI.spp)
  my.spp[my.spp %in% c(1, 2)] <- "conif"
  my.spp[my.spp %in% c(3)] <- "birch"
  ID <- paste0(my.spp, "-", fl$SI.m)
  sa <- devel.class[match(ID, devel.class$ID), c("class3", "class5")]
  AgeTo5 <- unlist(fl$stand.age.years[,this.period] - sa$class5)
  AgeTo3 <- unlist(fl$stand.age.years[,this.period] - sa$class3)


  ## calculate the time to reach dbh
  breast.height.age <-  data.frame(
    SI.m = rep(c(23,20,17,14,11,8,6),3),
    spp     = rep(c(1,2,3),each=7),
    breast.height.age = c(
      9,10,11,13,15,17,20,
      7,8,9,10,12,15,18,
      4,5,5,6,7,9,12),
    age.dbh.5cm = c(
      15,16,18,23,25,31,34,
      12,13,14,19,22,28,34,
      9,10,10,15,17,22,28)
  )
  
  breast.height.age$ID <- with(breast.height.age, paste0(spp, "-", SI.m))
  SPID <- paste0(fl$SI.spp, "-", fl$SI.m)
  time.to.dbh <- breast.height.age[match(SPID, breast.height.age$ID), "breast.height.age"]
  time.to.dbh.5cm <- breast.height.age[match(SPID, breast.height.age$ID), "age.dbh.5cm"]
  
  ## a False-vector for all plots
  plots.false <- rep(FALSE, length(fl$ustandID))
  
  ## group the plots by dominant species and SI
  ## pine
  plots.pine.high.si <- fl$mic.stratum %in% "ph"
  plots.pine.med.si <- fl$mic.stratum %in% "pm"
  plots.pine.low.si <- fl$mic.stratum %in% "pl"
  ## spruce
  plots.spruce.high.si <- fl$mic.stratum %in% "sh"
  plots.spruce.med.si <- fl$mic.stratum %in% "sm"
  plots.spruce.low.si <- fl$mic.stratum %in% "sl"
  ## other species
  plots.other.all.si <- fl$mic.stratum %in% "o"
  
##############
  ## thinning ##
##############
  
  ## thinning-vectors for species and SI
  ## initialised with FALSE
  thin.plots.pine.high.si <- thin.plots.pine.med.si <- thin.plots.spruce.high.si <-
    thin.plots.spruce.med.si <- plots.false
  
  ##------##
  ## pine ##
  ##------##
  
  ##>>>>>>>>>>>>>>>>>##
  ## high site-index ##
  ##>>>>>>>>>>>>>>>>>##
  
  if(mic.specification[["thinning.pine.high.si"]]){
    
    si.pine.high.si <- which(plots.pine.high.si)
    th.pine.high.si <-
      common.vars$i.stand[(common.vars$topheight.dm >=
                           mic.specification[["thinning.height.dm.pine.high.si"]])]
    thin.plots.pine.high.si[intersect(si.pine.high.si,th.pine.high.si)] <- T
    
  }

  ##>>>>>>>>>>>>>>>>>>>##
  ## medium site-index ##
  ##>>>>>>>>>>>>>>>>>>>##
  if(mic.specification[["thinning.pine.medium.si"]]){
    
    si.pine.med.si <- which(plots.pine.med.si)
    th.pine.med.si <-
      common.vars$i.stand[(common.vars$topheight.dm >=
                           mic.specification[["thinning.height.dm.pine.medium.si"]])]
    thin.plots.pine.med.si[intersect(si.pine.med.si,th.pine.med.si)] <- T
    
  }

  ##--------##
  ## spruce ##
  ##--------##
  
  ##>>>>>>>>>>>>>>>>>##
  ## high site-index ##
  ##>>>>>>>>>>>>>>>>>##
  if(mic.specification[["thinning.spruce.high.si"]]){
    
    si.spruce.high.si <- which(plots.spruce.high.si)
    th.spruce.high.si <-
      common.vars$i.stand[(common.vars$topheight.dm >=
                           mic.specification[["thinning.height.dm.spruce.high.si"]])]
    thin.plots.spruce.high.si[intersect(si.spruce.high.si,th.spruce.high.si)] <- T
    
  }
  
  ##>>>>>>>>>>>>>>>>>>>##
  ## medium site-index ##
  ##>>>>>>>>>>>>>>>>>>>##
  if(mic.specification[["thinning.spruce.medium.si"]]){
    
    si.spruce.med.si <- which(plots.spruce.med.si)
    th.spruce.med.si <-
      common.vars$i.stand[(common.vars$topheight.dm >=
                           mic.specification[["thinning.height.dm.spruce.medium.si"]])]
    thin.plots.spruce.med.si[intersect(si.spruce.med.si,th.spruce.med.si)] <- T
    
  }

  ## a combined vector
  thinning <- (thin.plots.pine.high.si | thin.plots.pine.med.si |
               thin.plots.spruce.high.si | thin.plots.spruce.med.si)
  
  ## no thinning if the stands are too young
  thinning[is.finite(AgeTo3) & AgeTo3 < -6] <- FALSE
  
  ## save that certain stands have been thinned in the management table
  if(sum(thinning) > 0) {
    substr(fl$management[thinning, next.period], 2, 2) <- "1"
  } else {
    print('No thinning on this period')    
  }
  
###################
  ## final felling ##
###################

  ## harvesting-vectors for species and SI
  ## initialised with FALSE
  harv.plots.pine.high.si <- harv.plots.pine.med.si <- harv.plots.pine.low.si <-
    harv.plots.spruce.high.si <- harv.plots.spruce.med.si <-
      harv.plots.spruce.low.si <- plots.false
  
  ##------##
  ## pine ##
  ##------##
  
  ##>>>>>>>>>>>>>>>>>##
  ## high site-index ##
  ##>>>>>>>>>>>>>>>>>##
  if(mic.specification[["final.felling.pine.high.si"]] %in% c("clear.cut","seed.tree")){
    harv.plots.pine.high.si[
      plots.pine.high.si & AgeTo5 >= (0 + mng.options$add.time.pine.high.si)] <- TRUE
    ## save the type of final harvest
    if(mic.specification[["final.felling.pine.high.si"]] %in% c("clear.cut")){
      substr(fl$management[harv.plots.pine.high.si,next.period], 1, 1) <- "1"
    } else {
      substr(fl$management[harv.plots.pine.high.si,next.period], 1, 1) <- "2"
    }
  }
  

  ##>>>>>>>>>>>>>>>>>>>##
  ## medium site-index ##
  ##>>>>>>>>>>>>>>>>>>>##
  if(mic.specification[["final.felling.pine.medium.si"]] %in%
     c("clear.cut","seed.tree")){
    harv.plots.pine.med.si[
      plots.pine.med.si & AgeTo5 >= (0 + mng.options$add.time.pine.high.si)] <- TRUE
    ## save the type of final harvest
    if(mic.specification[["final.felling.pine.medium.si"]] %in% c("clear.cut")){
      substr(fl$management[harv.plots.pine.med.si,next.period],1,1) <- "1"
    } else {
      substr(fl$management[harv.plots.pine.med.si,next.period],1,1) <- "2"
    }
    
  }
  
  
  ##>>>>>>>>>>>>>>>>>##
  ## low site-index ##
  ##>>>>>>>>>>>>>>>>>##
  if(mic.specification[["final.felling.pine.low.si"]] %in% c("clear.cut","seed.tree")){
    
    harv.plots.pine.low.si[plots.pine.low.si & AgeTo5 >=
                           (0 + mng.options$add.time.pine.low.si)]      <- TRUE
    ## save the type of final harvest
    if(mic.specification[["final.felling.pine.low.si"]] %in% c("clear.cut")){
      substr(fl$management[harv.plots.pine.low.si,next.period],1,1) <- "1"
    }else{substr(fl$management[harv.plots.pine.low.si,next.period],1,1) <- "2"}
    
  }
  
  ##--------##
  ## spruce ##
  ##--------##
  
  ##>>>>>>>>>>>>>>>>>##
  ## high site-index ##
  ##>>>>>>>>>>>>>>>>>##  
  if(mic.specification[["final.felling.spruce.high.si"]] %in% c("clear.cut","seed.tree")){
    
    harv.plots.spruce.high.si[plots.spruce.high.si & AgeTo5 >=
                              (0 + mng.options$add.time.pine.high.si)]      <- TRUE
    ## save the type of final harvest
    if(mic.specification[["final.felling.spruce.high.si"]] %in% c("clear.cut")){
      substr(fl$management[harv.plots.spruce.high.si,next.period],1,1) <- "1"
    }else{
      substr(fl$management[harv.plots.spruce.high.si,next.period],1,1) <- "2"
    }
    
  }
  
  ##>>>>>>>>>>>>>>>>>>>##
  ## medium site-index ##
  ##>>>>>>>>>>>>>>>>>>>##
  if(mic.specification[["final.felling.spruce.medium.si"]] %in%
     c("clear.cut","seed.tree")){
    
    harv.plots.spruce.med.si[plots.spruce.med.si & AgeTo5 >=
                             (0 + mng.options$add.time.pine.high.si)]      <- TRUE
    ## save the type of final harvest
    if(mic.specification[["final.felling.spruce.medium.si"]] %in% c("clear.cut")){
      substr(fl$management[harv.plots.spruce.med.si,next.period],1,1) <- "1"
    } else {
      substr(fl$management[harv.plots.spruce.med.si,next.period],1,1) <- "2"
    }
  }  
  
  ##>>>>>>>>>>>>>>>>##
  ## low site-index ##
  ##>>>>>>>>>>>>>>>>##  
  if(mic.specification[["final.felling.spruce.low.si"]] %in% c("clear.cut","seed.tree")){
    
    harv.plots.spruce.low.si[
      plots.spruce.low.si & AgeTo5 >= (0 + mng.options$add.time.pine.low.si)]  <- TRUE
    ## save the type of final harvest
    if(mic.specification[["final.felling.spruce.low.si"]] %in% c("clear.cut")){
      substr(fl$management[harv.plots.spruce.low.si,next.period],1,1) <- "1"
    } else {
      substr(fl$management[harv.plots.spruce.low.si,next.period],1,1) <- "2"
    }
  }
  
  
  ## a combined vector
  final.felling <- (
    harv.plots.pine.high.si  | harv.plots.pine.med.si    |
    harv.plots.pine.low.si   | harv.plots.spruce.high.si |
    harv.plots.spruce.med.si | harv.plots.spruce.low.si
  )
  
  
##############
  ## ingrowth ##
##############

  ## planted trees can not appear in the first period
  if(i.period > 0){
    ##if (i.period > 9) browser()
    ## planting-vectors for species and SI
    ## initialised with FALSE
    pwi.pine.high.si <- pwi.pine.medium.si <- pwi.pine.low.si <-
      pwi.spruce.high.si <- pwi.spruce.medium.si <- pwi.spruce.low.si <- plots.false
    ## the stands without trees
    stands.without.trees <- fl$ustandID[!fl$ustandID %in% tr$data$ustandID]

    ##------##
    ## pine ##
    ##------##
    
    ##>>>>>>>>>>>>>>>>>##
    ## high site-index ##
    ##>>>>>>>>>>>>>>>>>## 
    if(mic.specification[["reg.type.pine.high.si"]]%in%c("planting","natural")){
      
      ## plots, which should have ingrowth
      ## time since harvest is larger than the time to reach the dbh
      itr.pine.high.si <- 
        fl$ustandID[plots.pine.high.si &
                    fl$time.since.final.felling >=
                    (time.to.dbh.5cm + mic.specification[["waiting.time.pine.high.si"]]) &
                    !is.na(fl$time.since.final.felling)]
      
      ## select the corresponding plots -- nts.pine.high.si)
      pwi.pine.high.si <- fl$ustandID %in% intersect(itr.pine.high.si, stands.without.trees)
      
    }
    ##>>>>>>>>>>>>>>>>>>>##
    ## medium site-index ##
    ##>>>>>>>>>>>>>>>>>>>##
    if(mic.specification[["reg.type.pine.medium.si"]]%in%c("planting","natural")){
      
      ## plots, which should have ingrowth
      ## time since harvest is larger than the time to reach the dbh
      itr.pine.medium.si <- 
        fl$ustandID[plots.pine.med.si &
                    fl$time.since.final.felling >=
                    (time.to.dbh.5cm +
                     mic.specification[["waiting.time.pine.medium.si"]]
                    ) &
                    !is.na(fl$time.since.final.felling)]
      
      ## select the corresponding plots
      pwi.pine.medium.si <- fl$ustandID %in%
        intersect(itr.pine.medium.si,stands.without.trees)# nts.pine.medium.si)
      
    }
    
    
    ##>>>>>>>>>>>>>>>>##
    ## low site-index ##
    ##>>>>>>>>>>>>>>>>## 
    if(mic.specification[["reg.type.pine.low.si"]]%in%c("planting","natural")){
      
      ## plots, which should have ingrowth
      ## time since harvest is larger than the time to reach the dbh
      itr.pine.low.si <-
        fl$ustandID[plots.pine.low.si &
                    fl$time.since.final.felling >=
                    (time.to.dbh.5cm + mic.specification[["waiting.time.pine.low.si"]]) &
                    !is.na(fl$time.since.final.felling)]
      
      ## select the corresponding plots# nts.pine.low.si)
      pwi.pine.low.si <- fl$ustandID %in% intersect(itr.pine.low.si, stands.without.trees)
      
    }
    
    ##--------##
    ## spruce ##
    ##--------##
    
    ##>>>>>>>>>>>>>>>>>##
    ## high site-index ##
    ##>>>>>>>>>>>>>>>>>## 
    if(mic.specification[["reg.type.spruce.high.si"]]%in%c("planting","natural")){
      
      ## plots, which should have ingrowth
      ## time since harvest is larger than the time to reach the dbh
      itr.spruce.high.si <-
        fl$ustandID[
          plots.spruce.high.si &
          fl$time.since.final.felling >=
          (
            time.to.dbh.5cm + mic.specification[["waiting.time.spruce.high.si"]]
          ) &
          !is.na(fl$time.since.final.felling)]
      ## select the corresponding plots # nts.spruce.high.si)
      pwi.spruce.high.si <- fl$ustandID %in% intersect(itr.spruce.high.si,
                                                       stands.without.trees)
    }
    
    ##>>>>>>>>>>>>>>>>>>>##
    ## medium site-index ##
    ##>>>>>>>>>>>>>>>>>>>##
    if(mic.specification[["reg.type.spruce.medium.si"]]%in%c("planting","natural")){
      
      ## plots, which should have ingrowth
      ## time since harvest is larger than the time to reach the dbh
      itr.spruce.medium.si <-
        fl$ustandID[plots.spruce.med.si &
                    fl$time.since.final.felling >=
                    (time.to.dbh.5cm +
                     mic.specification[["waiting.time.spruce.medium.si"]]
                    ) & !is.na(fl$time.since.final.felling)]

      ## select the corresponding plots# nts.spruce.medium.si)
      pwi.spruce.medium.si <- fl$ustandID %in%
        intersect(itr.spruce.medium.si, stands.without.trees)
      
    }
    ##>>>>>>>>>>>>>>>>##
    ## low site-index ##
    ##>>>>>>>>>>>>>>>>## 
    if(mic.specification[["reg.type.spruce.low.si"]]%in%c("planting","natural")){
      
      ## plots, which should have ingrowth
      ## time since harvest is larger than the time to reach the dbh
      itr.spruce.low.si <- 
        fl$ustandID[plots.spruce.low.si &
                    fl$time.since.final.felling >=
                    (
                      time.to.dbh.5cm + mic.specification[["waiting.time.spruce.low.si"]]
                    ) & 
                    !is.na(fl$time.since.final.felling)]
      
      ## select the corresponding plots# nts.spruce.low.si)
      pwi.spruce.low.si <- fl$ustandID %in%
        intersect(itr.spruce.low.si, stands.without.trees)
      
    }
    
    ## all the plots that shall be planted
    ingrowth <- (pwi.pine.high.si   | pwi.pine.medium.si   | pwi.pine.low.si |
                 pwi.spruce.high.si | pwi.spruce.medium.si | pwi.spruce.low.si)
    
  } else { ## end if (i.period > 0)
    ingrowth <- plots.false
  }

######################################
  ## waiting for managed regeneration ##
######################################
  ## plots which have been harvested recently and which are waiting for managed regeneration
  waiting.pine.high.si <- plots.pine.high.si &
    fl$time.since.final.felling <
    (time.to.dbh.5cm + mic.specification[["waiting.time.pine.high.si"]]) &
    !is.na(fl$time.since.final.felling)

  waiting.pine.med.si <- plots.pine.med.si &
    fl$time.since.final.felling <
    (time.to.dbh.5cm + mic.specification[["waiting.time.pine.medium.si"]]) &
    !is.na(fl$time.since.final.felling)

  waiting.pine.low.si <- plots.pine.low.si &
    fl$time.since.final.felling <
    (time.to.dbh.5cm + mic.specification[["waiting.time.pine.low.si"]]) &
    !is.na(fl$time.since.final.felling)

  waiting.spruce.high.si <- plots.spruce.high.si &
    fl$time.since.final.felling <
    (time.to.dbh.5cm + mic.specification[["waiting.time.spruce.high.si"]]) &
    !is.na(fl$time.since.final.felling)

  waiting.spruce.med.si <- plots.spruce.med.si &
    fl$time.since.final.felling <
    (time.to.dbh.5cm + mic.specification[["waiting.time.spruce.medium.si"]]) &
    !is.na(fl$time.since.final.felling)

  waiting.spruce.low.si <- plots.spruce.low.si &
    fl$time.since.final.felling <
    (time.to.dbh.5cm + mic.specification[["waiting.time.spruce.low.si"]]) &
    !is.na(fl$time.since.final.felling)

  ## a combined vector for all species and SI
  waiting <- (waiting.pine.high.si   | waiting.pine.med.si   | waiting.pine.low.si |
              waiting.spruce.high.si | waiting.spruce.med.si | waiting.spruce.low.si)

  ## plots that are finally felt are not thinned
  thinning[final.felling] <- FALSE

  ## SEED TREES REMOVAL
  ## We will add a code for seed trees removal because it is a request of Hanne
  ## we don't use it for anything else in the code
  if (i.period != 0)  {
    substr(fl$management[substr(fl$management[, this.period], 1, 1) == "2",
                         next.period],
           1, 1) <- '3'
  

    ## PLANTING
    ## only spruce is planted
    i.planting.now <- substr(fl$management[, next.period], 1, 1) != "0" & ( 
    plots.spruce.low.si      | plots.spruce.med.si      |  plots.spruce.high.si 
    )
    substr(fl$management[i.planting.now,  next.period], 3, 3) <- '1'
  }
  ## save all management options in a list
  management <- list(final.felling = final.felling,
                     thinning      = thinning,
                     ingrowth      = ingrowth,
                     waiting       = waiting,
                     management    = fl$management[,next.period]
                     )  

  return(management)

}



## reassignInPackage("management.scen", "skogsimExtra", management.scen)
