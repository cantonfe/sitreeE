#########################
## tree.ingrowth.mic.r ##
#########################
## tree ingrowth according to MIC


tree.ingrowth.mic <- function(mic.specification,
                              tr = tr,
                              fl = fl,
                              common.vars = common.vars,
                              management  = management,
                              this.period = this.period,
                              next.period = next.period,
                              i.period    = i.period,
                              period.length = period.length,
                              mng.options = mng.options,
                              ...){

  print('---------------------- New trees planted')
  
   pwi.pine.high.si <- n.trees.pine.high.si <-
    dbh.pine.high.si <- standID.pine.high.si <- spec.pine.high.si <- NULL
  
  pwi.spruce.high.si <- n.trees.spruce.high.si <- dbh.spruce.high.si <-
    standID.spruce.high.si <- spec.spruce.high.si <- NULL
  pwi.pine.medium.si <- n.trees.pine.medium.si <- dbh.pine.medium.si <-
          standID.pine.medium.si <- spec.pine.medium.si <- NULL
  
  pwi.pine.low.si <- n.trees.pine.low.si <- dbh.pine.low.si <-
    standID.pine.low.si <- spec.pine.low.si <- NULL
  pwi.spruce.medium.si <- n.trees.spruce.medium.si <- dbh.spruce.medium.si <-
    standID.spruce.medium.si <- spec.spruce.medium.si <- NULL
  pwi.spruce.low.si <- n.trees.spruce.low.si <- dbh.spruce.low.si <-
          standID.spruce.low.si <- spec.spruce.low.si <- NULL
  
 

  ## the plots with ingrowth
  pwi.pine.high.si   <- fl$ustandID[management$ingrowth   &
                                    fl$SI.spp %in% 2 &
                                    fl$SI.m >= 15.5]
  pwi.pine.medium.si <- fl$ustandID[management$ingrowth &
                                    fl$SI.spp %in% 2 &
                                    fl$SI.m < 15.5 &
                                    fl$SI.m >= 10.5]
  pwi.pine.low.si    <- fl$ustandID[management$ingrowth &
                                    fl$SI.spp %in% 2 &
                                    fl$SI.m < 10.5]
  pwi.spruce.high.si <- fl$ustandID[management$ingrowth &
                                    fl$SI.spp %in% 1 &
                                    fl$SI.m >= 15.5]
  pwi.spruce.medium.si <- fl$ustandID[management$ingrowth &
                                      fl$SI.spp %in% 1 &
                                      fl$SI.m < 15.5 &
                                      fl$SI.m >= 10.5]
  pwi.spruce.low.si <- fl$ustandID[management$ingrowth &
                                   fl$SI.spp %in% 1 &
                                   fl$SI.m < 10.5]

  
 
    
    
  ##------##
  ## pine ##
  ##------##
  
  ##>>>>>>>>>>>>>>>>>##
  ## high site-index ##
  ##>>>>>>>>>>>>>>>>>##  
  
  ##<<<<<<<<<<##
  ## planting ##
  ##<<<<<<<<<<##
  if( mic.specification[["reg.type.pine.high.si"]] %in% c("planting","natural") &
      length(pwi.pine.high.si) > 0){
    
    ## trees2ha for the corresponding stands
    trees2ha.pine.high.si <- (fl$tree2ha[fl$ustandID %in% pwi.pine.high.si])
    
    ## a list with vectors of the regeneration species
    reg.spec.pine.high.si <-
      lapply(seq_along(pwi.pine.high.si),
             function(x){
               rep(c(mic.specification[["reg.main.spec.pine.high.si"]],
                     mic.specification[["reg.add.spec.pine.high.si"]])
                 , times =
                     c(mic.specification[["reg.main.n.plants.pine.high.si"]],
                       mic.specification[["reg.add.n.plants.pine.high.si"]]) /
                     trees2ha.pine.high.si[x])
             }
             )
    
    
    ## number of lost trees
    loss.pine.high.si <-
      lapply(seq_along(pwi.pine.high.si),
             function(x){
               as.integer(
                 rnorm(n=1
                      , mean = mng.options$mean.loss.pine.high.si *
                          length(reg.spec.pine.high.si[[x]])
                     , sd = mng.options$sd.loss.pine.high.si *
                         mng.options$mean.loss.pine.high.si *
                         length(reg.spec.pine.high.si[[x]]))
                 +.5)                            
             }
             )
    
    ## the dbh of all trees
    dbh.all.pine.high.si <-
      lapply(seq_along(pwi.pine.high.si),
             function(x){rnorm(1:length(reg.spec.pine.high.si[[x]])
                              ,mean=50
                              ,sd=0.1*50)})
    
    ## trees that die before reaching dbh
    trees.out.pine.high.si <-
      lapply(seq_along(pwi.pine.high.si),
             function(x){
               sample(1:length(reg.spec.pine.high.si[[x]]),loss.pine.high.si[[x]])
             })
    ## the tree species
    spec.pine.high.si <-
      as.factor(unlist(lapply(seq_along(pwi.pine.high.si),
                              function(x){
                                reg.spec.pine.high.si[[x]][-trees.out.pine.high.si[[x]]]
                              })))
    ## the dbh of the living trees
    dbh.pine.high.si <-
      unlist(lapply(seq_along(pwi.pine.high.si),
                    function(x){
                      dbh.all.pine.high.si[[x]][-trees.out.pine.high.si[[x]]]
                    }))
    ## the corresponding stand ids
    standID.pine.high.si <-
      unlist(lapply(seq_along(pwi.pine.high.si),
                    function(x){
                      rep(pwi.pine.high.si[x],
                          times = length(reg.spec.pine.high.si[[x]][-trees.out.pine.high.si[[x]]]))
                    }))
    ## the number of trees per plot
    n.trees.pine.high.si <-
      unlist(lapply(seq_along(pwi.pine.high.si),
                    function(x){
                      length(reg.spec.pine.high.si[[x]][-trees.out.pine.high.si[[x]]])
                    }))
    
  }
  
  ##>>>>>>>>>>>>>>>>>>>##
  ## medium site-index ##
  ##>>>>>>>>>>>>>>>>>>>##  
  ##<<<<<<<<<<##
  ## planting ##
  ##<<<<<<<<<<##
  if(mic.specification[["reg.type.pine.medium.si"]]%in%c("planting","natural") &
     length(pwi.pine.medium.si)>0){
    
    
    ## trees2ha for the corresponding stands
    trees2ha.pine.medium.si <- (fl$tree2ha[fl$ustandID %in% pwi.pine.medium.si])
    
    ## a list with vectors of the regeneration species
    reg.spec.pine.medium.si <-
      lapply(seq_along(pwi.pine.medium.si),
             function(x){
               rep(c(mic.specification[["reg.main.spec.pine.medium.si"]],
                     mic.specification[["reg.add.spec.pine.medium.si"]])
                 , times = c(mic.specification[["reg.main.n.plants.pine.medium.si"]],
                             mic.specification[["reg.add.n.plants.pine.medium.si"]])/trees2ha.pine.medium.si[x])
             }
             )
    
    
    ## number of lost trees
    loss.pine.medium.si <- lapply(seq_along(pwi.pine.medium.si),
                                  function(x){
                                    as.integer(
                                      rnorm(n=1
                                           ,mean = mng.options$mean.loss.pine.medium.si*length(reg.spec.pine.medium.si[[x]])
                                           ,sd = mng.options$sd.loss.pine.medium.si*mng.options$mean.loss.pine.medium.si*length(reg.spec.pine.medium.si[[x]]))
                                      +.5)                            
                                  }
                                  )
    
    ## the dbh of all trees
    dbh.all.pine.medium.si <- lapply(seq_along(pwi.pine.medium.si),function(x){rnorm(1:length(reg.spec.pine.medium.si[[x]])
                                                                                    ,mean=50
                                                                                    ,sd=0.1*50)})
    
    ## trees that die before reaching dbh
    trees.out.pine.medium.si <- lapply(seq_along(pwi.pine.medium.si),function(x){
      sample(1:length(reg.spec.pine.medium.si[[x]]),loss.pine.medium.si[[x]])})
    ## the tree species
    spec.pine.medium.si <- as.factor(unlist(lapply(seq_along(pwi.pine.medium.si),function(x){
      reg.spec.pine.medium.si[[x]][-trees.out.pine.medium.si[[x]]]})))
    ## the dbh of the living trees
    dbh.pine.medium.si <- unlist(lapply(seq_along(pwi.pine.medium.si),function(x){
      dbh.all.pine.medium.si[[x]][-trees.out.pine.medium.si[[x]]]}))
    ## the corresponding stand ids
    standID.pine.medium.si <- unlist(lapply(seq_along(pwi.pine.medium.si),function(x){
      rep(pwi.pine.medium.si[x],times=length(reg.spec.pine.medium.si[[x]][-trees.out.pine.medium.si[[x]]]))}))
    ## the number of trees per plot
    n.trees.pine.medium.si <- unlist(lapply(seq_along(pwi.pine.medium.si),function(x){
      length(reg.spec.pine.medium.si[[x]][-trees.out.pine.medium.si[[x]]])}))
    
  }
  
  ##>>>>>>>>>>>>>>>>##
  ## low site-index ##
  ##>>>>>>>>>>>>>>>>##  
  ##<<<<<<<<<<##
  ## planting ##
  ##<<<<<<<<<<##
  if(mic.specification[["reg.type.pine.low.si"]]%in%c("planting","natural") &
     length(pwi.pine.low.si)>0){
    
    
    ## trees2ha for the corresponding stands
    trees2ha.pine.low.si <- (fl$tree2ha[fl$ustandID %in% pwi.pine.low.si])
    
    ## a list with vectors of the regeneration species
    reg.spec.pine.low.si <-
      lapply(seq_along(pwi.pine.low.si),
             function(x){
               rep(c(mic.specification[["reg.main.spec.pine.low.si"]],
                     mic.specification[["reg.add.spec.pine.low.si"]])
                  ,times=c(mic.specification[["reg.main.n.plants.pine.low.si"]],
                           mic.specification[["reg.add.n.plants.pine.low.si"]])/trees2ha.pine.low.si[x])
             }
             )
    
    
    ## number of lost trees
    loss.pine.low.si <- lapply(seq_along(pwi.pine.low.si),
                               function(x){
                                 as.integer(
                                   rnorm(n=1
                                        ,mean = mng.options$mean.loss.pine.low.si*length(reg.spec.pine.low.si[[x]])
                                        ,sd = mng.options$sd.loss.pine.low.si*mng.options$mean.loss.pine.low.si*length(reg.spec.pine.low.si[[x]]))
                                   +.5)                            
                               }
                               )
    
    ## the dbh of all trees
    dbh.all.pine.low.si <- lapply(seq_along(pwi.pine.low.si),function(x){rnorm(1:length(reg.spec.pine.low.si[[x]])
                                                                              ,mean=50
                                                                              ,sd=0.1*50)})
    
    ## trees that die before reaching dbh
    trees.out.pine.low.si <- lapply(seq_along(pwi.pine.low.si),function(x){sample(1:length(reg.spec.pine.low.si[[x]]),loss.pine.low.si[[x]])})
    ## the tree species
    spec.pine.low.si <- as.factor(unlist(lapply(seq_along(pwi.pine.low.si),function(x){reg.spec.pine.low.si[[x]][-trees.out.pine.low.si[[x]]]})))
    ## the dbh of the living trees
    dbh.pine.low.si <- unlist(lapply(seq_along(pwi.pine.low.si),function(x){dbh.all.pine.low.si[[x]][-trees.out.pine.low.si[[x]]]}))
    ## the corresponding stand ids
    standID.pine.low.si <- unlist(lapply(seq_along(pwi.pine.low.si),function(x){rep(pwi.pine.low.si[x],times=length(reg.spec.pine.low.si[[x]][-trees.out.pine.low.si[[x]]]))}))
    ## the number of trees per plot
    n.trees.pine.low.si <- unlist(lapply(seq_along(pwi.pine.low.si),function(x){length(reg.spec.pine.low.si[[x]][-trees.out.pine.low.si[[x]]])}))
    
  }
  
  
  ##--------##
  ## spruce ##
  ##--------##
  
  ##>>>>>>>>>>>>>>>>>##
  ## high site-index ##
  ##>>>>>>>>>>>>>>>>>##  
  ##<<<<<<<<<<##
  ## planting ##
  ##<<<<<<<<<<##
  if (length(pwi.spruce.high.si) > 0 & 
    mic.specification[["reg.type.spruce.high.si"]] == "planting" 
    
  ){
    ## trees2ha for the corresponding stands
    trees2ha.spruce.high.si <- (fl$tree2ha[fl$ustandID %in% pwi.spruce.high.si])
    
    ## a list with vectors of the regeneration species
    reg.spec.spruce.high.si <-lapply(
      seq_along(pwi.spruce.high.si),
      function(x){
        rep(c(mic.specification[["reg.main.spec.spruce.high.si"]],
              mic.specification[["reg.add.spec.spruce.high.si"]]),
            times = c(mic.specification[["reg.main.n.plants.spruce.high.si"]],
                      mic.specification[["reg.add.n.plants.spruce.high.si"]])/
              trees2ha.spruce.high.si[x])
      }
    )
    
    
    ## number of lost trees
    loss.spruce.high.si <- lapply(
      seq_along(pwi.spruce.high.si),
      function(x){
        as.integer(
          rnorm(n=1,
                mean = mng.options$mean.loss.spruce.high.si *
                  length(reg.spec.spruce.high.si[[x]]),
                sd = mng.options$sd.loss.spruce.high.si *
                  mng.options$mean.loss.spruce.high.si *
                  length(reg.spec.spruce.high.si[[x]]))
          +.5)                             
      }
    )
    
    ## the dbh of all trees
    dbh.all.spruce.high.si <- lapply(
      seq_along(pwi.spruce.high.si),
      function(x){rnorm(1:length(reg.spec.spruce.high.si[[x]])
                       ,mean=50
                       ,sd=0.1*50)})
    
    ## trees that die before reaching dbh
    trees.out.spruce.high.si <- lapply(
      seq_along(pwi.spruce.high.si),
      function(x){sample(1:length(reg.spec.spruce.high.si[[x]]),
                         loss.spruce.high.si[[x]])})
    ## the tree species
    spec.spruce.high.si <- as.factor(
      unlist(lapply(seq_along(pwi.spruce.high.si),
                    function(x){
                      reg.spec.spruce.high.si[[x]][-trees.out.spruce.high.si[[x]]]})))
    ## the dbh of the living trees
    dbh.spruce.high.si <- unlist(
      lapply(seq_along(pwi.spruce.high.si),
             function(x){dbh.all.spruce.high.si[[x]][-trees.out.spruce.high.si[[x]]]}))
    ## the corresponding stand ids
    standID.spruce.high.si <- unlist(
      lapply(seq_along(pwi.spruce.high.si),
             function(x){rep(pwi.spruce.high.si[x],
                             times = length(reg.spec.spruce.high.si[[x]]
                                            [-trees.out.spruce.high.si[[x]]]))}))
    ## the number of trees per plot
    n.trees.spruce.high.si <- unlist(
      lapply(seq_along(pwi.spruce.high.si),
             function(x){
               length(reg.spec.spruce.high.si[[x]]
                                [-trees.out.spruce.high.si[[x]]])})
    )
    
  }
  
  ##>>>>>>>>>>>>>>>>>>>##
  ## medium site-index ##
  ##>>>>>>>>>>>>>>>>>>>##  
  ##<<<<<<<<<<##
  ## planting ##
  ##<<<<<<<<<<##
  if(mic.specification[["reg.type.spruce.medium.si"]] %in% c("planting") &
     length(pwi.spruce.medium.si)>0){
    
    ## trees2ha for the corresponding stands
    trees2ha.spruce.medium.si <- (fl$tree2ha[fl$ustandID %in% pwi.spruce.medium.si])
    
    ## a list with vectors of the regeneration species
    reg.spec.spruce.medium.si <-lapply(
      seq_along(pwi.spruce.medium.si),
      function(x){rep(c(mic.specification[["reg.main.spec.spruce.medium.si"]],
                        mic.specification[["reg.add.spec.spruce.medium.si"]])
                     ,times=c(mic.specification[["reg.main.n.plants.spruce.medium.si"]],
                              mic.specification[["reg.add.n.plants.spruce.medium.si"]])/
                        trees2ha.spruce.medium.si[x])
      }
    )
    
    
    ## number of lost trees
    loss.spruce.medium.si <- lapply(
      seq_along(pwi.spruce.medium.si),
      function(x){
        as.integer(
          rnorm(n=1
               ,mean = mng.options$mean.loss.spruce.medium.si *
                  length(reg.spec.spruce.medium.si[[x]])
               ,sd = mng.options$sd.loss.spruce.medium.si *
                  mng.options$mean.loss.spruce.medium.si *
                  length(reg.spec.spruce.medium.si[[x]]))
          +.5)                            
      }
    )
    
    ## the dbh of all trees
    dbh.all.spruce.medium.si <-
      lapply(seq_along(pwi.spruce.medium.si),
             function(x){
               rnorm(1:length(reg.spec.spruce.medium.si[[x]])
                    ,mean=50
                    ,sd=0.1*50)
             })
    
    ## trees that die before reaching dbh
    trees.out.spruce.medium.si <-
      lapply(seq_along(pwi.spruce.medium.si),
             function(x){
               sample(1:length(reg.spec.spruce.medium.si[[x]]),
                      loss.spruce.medium.si[[x]])
             })
    ## the tree species
    spec.spruce.medium.si <-
      as.factor(unlist(
        lapply(seq_along(pwi.spruce.medium.si),
               function(x){
                 reg.spec.spruce.medium.si[[x]][-trees.out.spruce.medium.si[[x]]]
               }
               )))
    ## the dbh of the living trees
    dbh.spruce.medium.si <-
      unlist(lapply(seq_along(pwi.spruce.medium.si),
                    function(x){
                      dbh.all.spruce.medium.si[[x]][-trees.out.spruce.medium.si[[x]]]
                    }
                    ))
    ## the corresponding stand ids
    standID.spruce.medium.si <-
      unlist(
        lapply(seq_along(pwi.spruce.medium.si),
               function(x){
                 rep(pwi.spruce.medium.si[x],
                     times = length(reg.spec.spruce.medium.si[[x]][-trees.out.spruce.medium.si[[x]]]))
               }))
    ## the number of trees per plot
    n.trees.spruce.medium.si <-
      unlist(
        lapply(seq_along(pwi.spruce.medium.si),
               function(x){
                 length(reg.spec.spruce.medium.si[[x]][-trees.out.spruce.medium.si[[x]]])
               }))
    
  }
  
  ##>>>>>>>>>>>>>>>>##
  ## low site-index ##
  ##>>>>>>>>>>>>>>>>##  
  ##<<<<<<<<<<##
  ## planting ##
  ##<<<<<<<<<<##
  if(mic.specification[["reg.type.spruce.low.si"]] %in% c("planting") &
     length(pwi.spruce.low.si)>0){
    
    
    ## trees2ha for the corresponding stands
    trees2ha.spruce.low.si <- (fl$tree2ha[fl$ustandID %in% pwi.spruce.low.si])
    
    ## a list with vectors of the regeneration species
    reg.spec.spruce.low.si <-
      lapply(seq_along(pwi.spruce.low.si),
             function(x){
               rep(c(mic.specification[["reg.main.spec.spruce.low.si"]],
                     mic.specification[["reg.add.spec.spruce.low.si"]])
                  , times = c(mic.specification[["reg.main.n.plants.spruce.low.si"]],
                              mic.specification[["reg.add.n.plants.spruce.low.si"]])/
                      trees2ha.spruce.low.si[x])
             }
             )
    
    
    ## number of lost trees
    loss.spruce.low.si <-
      lapply(seq_along(pwi.spruce.low.si),
             function(x){
               as.integer(
                 rnorm(n=1
                      ,mean = mng.options$mean.loss.spruce.low.si *
                         length(reg.spec.spruce.low.si[[x]])
                      ,sd = mng.options$sd.loss.spruce.low.si *
                         mng.options$mean.loss.spruce.low.si  *
                         length(reg.spec.spruce.low.si[[x]]))
                 +.5)                            
             }
             )
    
    ## the dbh of all trees
    dbh.all.spruce.low.si <-
      lapply(seq_along(pwi.spruce.low.si),
             function(x){
               rnorm(1:length(reg.spec.spruce.low.si[[x]])
                    ,mean=50
                    ,sd=0.1*50)
             })
    
    ## trees that die before reaching dbh
    trees.out.spruce.low.si <- lapply(
      seq_along(pwi.spruce.low.si),
      function(x){
        sample(1:length(reg.spec.spruce.low.si[[x]]),loss.spruce.low.si[[x]])
      })
    ## the tree species
    spec.spruce.low.si <- as.factor(
      unlist(lapply(seq_along(pwi.spruce.low.si),
                    function(x){
                      reg.spec.spruce.low.si[[x]][-trees.out.spruce.low.si[[x]]]
                    })))
    ## the dbh of the living trees
    dbh.spruce.low.si <- unlist(
      lapply(seq_along(pwi.spruce.low.si),
             function(x){
               dbh.all.spruce.low.si[[x]] [-trees.out.spruce.low.si[[x]]]}))
    ## the corresponding stand ids
    standID.spruce.low.si <- unlist(
      lapply(seq_along(pwi.spruce.low.si),
             function(x){
               rep(pwi.spruce.low.si[x],
                   times = length(reg.spec.spruce.low.si[[x]][-trees.out.spruce.low.si[[x]]]))}))
    ## the number of trees per plot
    n.trees.spruce.low.si <- unlist(
      lapply(seq_along(pwi.spruce.low.si),
             function(x){length(reg.spec.spruce.low.si[[x]] [-trees.out.spruce.low.si[[x]]])}))
    
  }

  ## a list for all ingrowth trees
  ingrowth <- list()
   ## combine the values of the different strata
  ## ids
  all.pwi <- levels(fl$ustandID)[c(pwi.pine.high.si,   pwi.pine.medium.si,
                                   pwi.pine.low.si,    pwi.spruce.high.si,
                                   pwi.spruce.medium.si, pwi.spruce.low.si)
                                 ]
  all.trees <- c(n.trees.pine.high.si,
                 n.trees.pine.medium.si,
                 n.trees.pine.low.si,
                 n.trees.spruce.high.si,
                 n.trees.spruce.medium.si,
                 n.trees.spruce.low.si)
  ingrowth$ustandID <- factor(
    rep(all.pwi, times = all.trees),
    levels=levels(tr$data$ustandID)
  )
  ## dbh
  dbh.mm <- c(dbh.pine.high.si,   dbh.pine.medium.si,   dbh.pine.low.si,
              dbh.spruce.high.si, dbh.spruce.medium.si, dbh.spruce.low.si)
  ## internal data.frame
  dat.intern <- as.data.frame(matrix(0, nrow=length(dbh.mm), ncol=ncol(tr$data$dbh.mm)))
  names(dat.intern) <- names(tr$data$dbh.mm)
  ## new tree ids
  ingrowth$treeid <- if(length(dbh.mm)>0){
                       max(tr$data[["treeid"]]) + (1:length(dbh.mm))
                     } else{
                       as.integer()
                     }
  ingrowth$height.dm <- ingrowth$dbh.mm <- dat.intern
  ## add dbh and height data
  if(!is.null(dbh.mm)){ingrowth$dbh.mm[,next.period] <- dbh.mm}

  ## all heights are set to 32 this probably needs to be recalculated
  ingrowth$height.dm[,next.period] <- rep(32, length.out = length(dbh.mm))
  ## the years in simulation
  ingrowth$yrs.sim <- as.numeric(rep(period.length/2,length.out = length(dbh.mm)))
  ## the tree species
  
  ingrowth$tree.sp <- factor(
    c(
      spec.pine.high.si,   spec.pine.medium.si,   spec.pine.low.si,
      spec.spruce.high.si, spec.spruce.medium.si, spec.spruce.low.si
    ),
    levels(tr$data$tree.sp))
  
  ## return the ingrowth data.frame
  return(ingrowth)
  
}

## reassignInPackage("tree.ingrowth.mic", "skogsimExtra", tree.ingrowth.mic)
