########################
## tree.removal.mic.r ##
########################
## tree removal according to MIC

tree.removal.mic <- function(
                             tr
                            ,fl
                            ,common.vars
                            ,management
                            ,this.period
                            ,i.period
                             , mng.options
                           , ...){
 
  
  mic.specification <- mng.options$current.mics
  
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
  thin.trees.pine.high.si <- thin.trees.pine.med.si <- thin.trees.spruce.high.si <-
    thin.trees.spruce.med.si <- rep(F,length(tr$data$ustandID))
  
  ##------##
  ## pine ##
  ##------##
  
  ##>>>>>>>>>>>>>>>>>##    
  ## high site-index ##
  ##>>>>>>>>>>>>>>>>>##
  if(mic.specification[["thinning.pine.high.si"]]){
    if (is.factor(fl$ustandID)) {
      thin.plots.pine.high.si <- fl$ustandID[management$thinning &
                                             plots.pine.high.si]
    }
    thin.trees.pine.high.si <- tr$data$ustandID %in% thin.plots.pine.high.si &
      common.vars$PBAL.m2.ha  > 24
  }
  ##>>>>>>>>>>>>>>>>>>>##
  ## medium site-index ##
  ##>>>>>>>>>>>>>>>>>>>##
  if(mic.specification[["thinning.pine.medium.si"]]){
    
    thin.plots.pine.med.si <- fl$ustandID[management$thinning &
                                          plots.pine.med.si]
    thin.trees.pine.med.si <- tr$data$ustandID %in% thin.plots.pine.med.si &
      common.vars$PBAL.m2.ha  > sample(17:21,1)
    
  }
  
  ##--------##
  ## spruce ##
  ##--------##
  
  ##>>>>>>>>>>>>>>>>>##
  ## high site-index ##
  ##>>>>>>>>>>>>>>>>>##
  if(mic.specification[["thinning.spruce.high.si"]]){
    
    thin.plots.spruce.high.si <- fl$ustandID[management$thinning &
                                                        plots.spruce.high.si]
    thin.trees.spruce.high.si <- tr$data$ustandID %in% thin.plots.spruce.high.si &
      common.vars$PBAL.m2.ha  > 24
  }
  ##>>>>>>>>>>>>>>>>>>>##
  ## medium site-index ##
  ##>>>>>>>>>>>>>>>>>>>##
  if(mic.specification[["thinning.spruce.medium.si"]]){
    
    thin.plots.spruce.med.si <- fl$ustandID[management$thinning &
                                            plots.spruce.med.si]
    thin.trees.spruce.med.si <- tr$data$ustandID %in%
      thin.plots.spruce.med.si&common.vars$PBAL.m2.ha  > sample(20:22,1)
  }   
  
  thinned.trees <- (thin.trees.pine.high.si | thin.trees.pine.med.si |
                    thin.trees.spruce.high.si | thin.trees.spruce.med.si)
  
  
  ###################
  ## final felling ##
  ###################
  
  ##------##
  ## pine ##
  ##------##
  harv.trees.pine.high.si <- harv.trees.pine.med.si <- harv.trees.pine.low.si <-
    harv.trees.spruce.high.si <- harv.trees.spruce.med.si <-
      harv.trees.spruce.low.si <- rep(F,length(tr$data$ustandID))
  
  ##>>>>>>>>>>>>>>>>>##
  ## high site-index ##
  ##>>>>>>>>>>>>>>>>>##
  ##<<<<<<<<<<<##
  ## clear cut ##
  ##<<<<<<<<<<<##
  if(mic.specification[["final.felling.pine.high.si"]] %in% c("clear.cut")){
    
    ## the plots in which final felling might be executed in this stratum
    harv.plots.pine.high.si <- fl$ustandID[management$final.felling &
                                                      plots.pine.high.si]
    
    ## select trees from below
    ## up to a certain cumulative volume
    prob.harv.plots.pine.high.si <-
      as.logical(ave(common.vars$vol.prob.cc.left,
                     common.vars$i.stand,
                     FUN=function(x){
                       X1 <- rep(F,length(x))
                       X1[which(x <= x[which.min(abs(x-mic.specification[["prop.vol.left.cc.pine.high.si"]]))])] <- T
                       return(X1)
                     }))
    ## the selected trees in this stratum 
    harv.trees.pine.high.si <- tr$data$ustandID %in% harv.plots.pine.high.si &
      prob.harv.plots.pine.high.si
  }
  
  ##<<<<<<<<<<<<##
  ## seed trees ##
  ##<<<<<<<<<<<<##
  if(mic.specification[["final.felling.pine.high.si"]] %in% c("seed.tree")){
    
    ## the plots in which final felling might be executed in this stratum
    final.felling.pine.high.si <- management$final.felling & plots.pine.high.si
    ## a dataframe with the stands with possible final felling
    ## standID and the number of trees represented by a tree on a plot
    harv.plots.pine.high.si <- data.frame(
      ustandID = fl$ustandID[final.felling.pine.high.si]
     ,tree2ha  = fl$tree2ha[final.felling.pine.high.si]
    )
    ## the number of seed trees per ha
    ## random number per plot
    harv.plots.pine.high.si$seed.trees.per.ha <-
      runif(min=mic.specification[["seed.trees.n.min.pine.high.si"]]
           ,max=mic.specification[["seed.trees.n.max.pine.high.si"]]
           ,n=nrow(harv.plots.pine.high.si))
    ## the minimal number of seed trees per plot
    harv.plots.pine.high.si$min.seed.trees.per.plot <-
      as.integer(harv.plots.pine.high.si$seed.trees.per.ha /
                 harv.plots.pine.high.si$tree2ha)
    ## the probability of having an additional tree on the plot
    harv.plots.pine.high.si$prob.add.seed.tree.per.plot <-
      with(harv.plots.pine.high.si,(seed.trees.per.ha/tree2ha)-min.seed.trees.per.plot)
    ## the number of additional trees on the plots
    harv.plots.pine.high.si$add.seed.tree.per.plot <-
      ifelse(runif(n=nrow(harv.plots.pine.high.si)) >=
             harv.plots.pine.high.si$prob.add.seed.tree.per.plot,1,0)
    ## the number of seed trees per plot
    harv.plots.pine.high.si$seed.trees.per.plot <-
      with(harv.plots.pine.high.si,min.seed.trees.per.plot+add.seed.tree.per.plot)
    ## only plots with seed trees
    harv.plots.red.pine.high.si <-
      harv.plots.pine.high.si[harv.plots.pine.high.si$seed.trees.per.plot>0,]
    ## drop factor levels
    harv.plots.red.pine.high.si$ustandID <- harv.plots.red.pine.high.si$ustandID
    
    ## the trees on all the plots
    harv.trees.pine.high.si <- tr$data$ustandID %in%
      harv.plots.red.pine.high.si$ustandID
    
    ## a data.table of treeid, dbh and stand
    harv.data.1.pine.high.si <- data.frame(
      treeid=tr$data$treeid[harv.trees.pine.high.si]
      ,dbh.mm=tr$data$dbh.mm[harv.trees.pine.high.si,this.period]
      ,ustandID=tr$data$ustandID[harv.trees.pine.high.si]
    )
    
    ## attach the number of seed trees per plot
    harv.data.2.pine.high.si <-
      merge(harv.data.1.pine.high.si, 
            harv.plots.red.pine.high.si[,c("ustandID","seed.trees.per.plot")]
           ,by="ustandID")
    ## rank the trees according to their dbh
    harv.data.2.pine.high.si$dbh.rank <- ave(harv.data.2.pine.high.si$dbh.mm
                                             , harv.data.2.pine.high.si$ustandID
                                             , FUN = function(x){
                                               length(x) -
                                                 rank(x, ties.method="random")+1
                                             })
    
    ## the ids of the seed trees
    seed.trees.pine.high.si <-
      harv.data.2.pine.high.si$treeid[harv.data.2.pine.high.si$dbh.rank <=
                                      harv.data.2.pine.high.si$seed.trees.per.plot]
    
    ## exlude the big trees from the selection    
    harv.trees.pine.high.si[which(tr$data$treeid %in% seed.trees.pine.high.si)] <- F
    
    
    ## The seed trees of the last period have to be removed
    if(i.period>0){    
      sp.lp.pine.high.si <- fl$ustandID[
        substr(fl$management[,this.period], 1, 1)== "2" &
        plots.pine.high.si
      ]
      st.lp.pine.high.si <- tr$data$ustandID %in% sp.lp.pine.high.si
      ## TRUE for the seed trees of the last period
      harv.trees.pine.high.si[which(st.lp.pine.high.si)] <- T
      
      
    }
  }
  
  ##>>>>>>>>>>>>>>>>>>>##
  ## medium site-index ##
  ##>>>>>>>>>>>>>>>>>>>##
  ##<<<<<<<<<<<##
  ## clear cut ##
  ##<<<<<<<<<<<##
  if(mic.specification[["final.felling.pine.medium.si"]] %in% c("clear.cut")){
    
    ## the plots in which final felling might be executed in this stratum
    harv.plots.pine.med.si <-
      fl$ustandID[management$final.felling & plots.pine.med.si]
  
    ## select trees from below
    ## up to a certain cumulative volume
    prob.harv.plots.pine.med.si <-
      as.logical(ave(common.vars$vol.prob.cc.left,
                     common.vars$i.stand,
                     FUN = function(x){
                       X1 <- rep(F,length(x))
                       X1[which(x <= x[which.min(abs(x - mic.specification[["prop.vol.left.cc.pine.medium.si"]]))])] <- T
                       return(X1)
                     }))
    ## the selected trees in this stratum 
    harv.trees.pine.med.si <- tr$data$ustandID%in%harv.plots.pine.med.si &
      prob.harv.plots.pine.med.si
    
  }
  ##<<<<<<<<<<<<##
  ## seed trees ##
  ##<<<<<<<<<<<<##
  if(mic.specification[["final.felling.pine.medium.si"]] %in% c("seed.tree")){
    ## the plots in which final felling might be executed in this stratum
    final.felling.pine.med.si <- management$final.felling & plots.pine.med.si
    ## a dataframe with the stands with possible final felling
    ## standID and the number of trees represented by a tree on a plot
    harv.plots.pine.med.si <-
      data.frame(ustandID = fl$ustandID[final.felling.pine.med.si]
                ,tree2ha  = fl$tree2ha[final.felling.pine.med.si])
    ## the number of seed trees per ha
    ## random number per plot
    harv.plots.pine.med.si$seed.trees.per.ha <-
      runif(min=mic.specification[["seed.trees.n.min.pine.medium.si"]]
           ,max=mic.specification[["seed.trees.n.max.pine.medium.si"]]
           ,n=nrow(harv.plots.pine.med.si))
    ## the minimal number of seed trees per plot
    harv.plots.pine.med.si$min.seed.trees.per.plot <-
      as.integer(harv.plots.pine.med.si$seed.trees.per.ha /
                 harv.plots.pine.med.si$tree2ha)
    ## the probability of having an additional tree on the plot
    harv.plots.pine.med.si$prob.add.seed.tree.per.plot <-
      with(harv.plots.pine.med.si,(seed.trees.per.ha/tree2ha)-min.seed.trees.per.plot)
    ## the number of additional trees on the plots
    harv.plots.pine.med.si$add.seed.tree.per.plot <-
      ifelse(runif(n=nrow(harv.plots.pine.med.si)) >=
             harv.plots.pine.med.si$prob.add.seed.tree.per.plot,1,0)
    ## the number of seed trees per plot
    harv.plots.pine.med.si$seed.trees.per.plot <-
      with(harv.plots.pine.med.si,min.seed.trees.per.plot+add.seed.tree.per.plot)
    ## only plots with seed trees
    harv.plots.red.pine.med.si <-
      harv.plots.pine.med.si[harv.plots.pine.med.si$seed.trees.per.plot>0,]
    ## drop factor levels
    harv.plots.red.pine.med.si$ustandID <- harv.plots.red.pine.med.si$ustandID
    
    ## the trees on all the plots
    harv.trees.pine.med.si <- tr$data$ustandID %in% harv.plots.red.pine.med.si$ustandID
    
    ## a data.table of treeid, dbh and stand
    harv.data.1.pine.med.si <- data.frame(
      treeid=tr$data$treeid[harv.trees.pine.med.si]
      ,dbh.mm=tr$data$dbh.mm[harv.trees.pine.med.si,this.period]
      ,ustandID=tr$data$ustandID[harv.trees.pine.med.si]
    )
    
    ## attach the number of seed trees per plot
    harv.data.2.pine.med.si <-
      merge(harv.data.1.pine.med.si
           , harv.plots.red.pine.med.si[,c("ustandID","seed.trees.per.plot")]
           , by="ustandID")
    ## rank the trees according to their dbh
    harv.data.2.pine.med.si$dbh.rank <- ave(harv.data.2.pine.med.si$dbh.mm
                                            , harv.data.2.pine.med.si$ustandID
                                            , FUN=function(x){
                                              length(x)-rank(x,ties.method="random")+1
                                            })
    
    ## the ids of the seed trees
    seed.trees.pine.med.si <-
      harv.data.2.pine.med.si$treeid[harv.data.2.pine.med.si$dbh.rank <=
                                     harv.data.2.pine.med.si$seed.trees.per.plot]
    
    ## exlude the big trees from the selection    
    harv.trees.pine.med.si[which(tr$data$treeid %in% seed.trees.pine.med.si)] <- F
    
    
    ## The seed trees of the last period have to be removed
    if(i.period>0){    
      sp.lp.pine.med.si <-
        fl$ustandID[substr(fl$management[,this.period],1,1)== "2" &
                    plots.pine.med.si]
      st.lp.pine.med.si <- tr$data$ustandID %in% sp.lp.pine.med.si
      ## TRUE for the seed trees of the last period
      harv.trees.pine.med.si[which(st.lp.pine.med.si)] <- T
      
      
    }
  }
  
  ##>>>>>>>>>>>>>>>>##
  ## low site-index ##
  ##>>>>>>>>>>>>>>>>##
  ##<<<<<<<<<<<##
  ## clear cut ##
  ##<<<<<<<<<<<##
  if(mic.specification[["final.felling.pine.low.si"]] %in% c("clear.cut")){
    
    ## the plots in which final felling might be executed in this stratum
    harv.plots.pine.low.si <- fl$ustandID[management$final.felling & plots.pine.low.si]
    
    ## select trees from below
    ## up to a certain cumulative volume
    prob.harv.plots.pine.low.si <-
      as.logical(ave(common.vars$vol.prob.cc.left,
                     common.vars$i.stand,
                     FUN=function(x){
                       X1 <- rep(F,length(x))
                       X1[which(x <= x[which.min(abs(x-mic.specification[["prop.vol.left.cc.pine.low.si"]]))])] <- T
                       return(X1)
                     }))
    ## the selected trees in this stratum 
    harv.trees.pine.low.si <- tr$data$ustandID%in%harv.plots.pine.low.si &
      prob.harv.plots.pine.low.si
    
  }
  
  ##<<<<<<<<<<<<##
  ## seed trees ##
  ##<<<<<<<<<<<<##
  if(mic.specification[["final.felling.pine.low.si"]] %in% c("seed.tree")){
    ## the plots in which final felling might be executed in this stratum
    final.felling.pine.low.si <- management$final.felling & plots.pine.low.si
    ## a dataframe with the stands with possible final felling
    ## standID and the number of trees represented by a tree on a plot
    harv.plots.pine.low.si <-
      data.frame(ustandID = fl$ustandID[final.felling.pine.low.si]
                ,tree2ha  = fl$tree2ha[final.felling.pine.low.si])
    ## the number of seed trees per ha
    ## random number per plot
    harv.plots.pine.low.si$seed.trees.per.ha <-
      runif(min=mic.specification[["seed.trees.n.min.pine.low.si"]]
           ,max=mic.specification[["seed.trees.n.max.pine.low.si"]]
           ,n=nrow(harv.plots.pine.low.si))
    ## the minimal number of seed trees per plot
    harv.plots.pine.low.si$min.seed.trees.per.plot <-
      as.integer(harv.plots.pine.low.si$seed.trees.per.ha /
                 harv.plots.pine.low.si$tree2ha)
    ## the probability of having an additional tree on the plot
    harv.plots.pine.low.si$prob.add.seed.tree.per.plot <-
      with(harv.plots.pine.low.si,
      (seed.trees.per.ha/tree2ha) - min.seed.trees.per.plot)
    ## the number of additional trees on the plots
    harv.plots.pine.low.si$add.seed.tree.per.plot <-
      ifelse(runif(n=nrow(harv.plots.pine.low.si)) >=
             harv.plots.pine.low.si$prob.add.seed.tree.per.plot,1,0)
    ## the number of seed trees per plot
    harv.plots.pine.low.si$seed.trees.per.plot <-
      with(harv.plots.pine.low.si, min.seed.trees.per.plot + add.seed.tree.per.plot)
    ## only plots with seed trees
    harv.plots.red.pine.low.si <-
      harv.plots.pine.low.si[harv.plots.pine.low.si$seed.trees.per.plot > 0,]
    ## drop factor levels
    harv.plots.red.pine.low.si$ustandID <- harv.plots.red.pine.low.si$ustandID
    
    ## the trees on all the plots
    harv.trees.pine.low.si <- tr$data$ustandID %in% harv.plots.red.pine.low.si$ustandID
    
    ## a data.table of treeid, dbh and stand
    harv.data.1.pine.low.si <- data.frame(
      treeid=tr$data$treeid[harv.trees.pine.low.si]
      ,dbh.mm=tr$data$dbh.mm[harv.trees.pine.low.si,this.period]
      ,ustandID=tr$data$ustandID[harv.trees.pine.low.si]
    )
    
    ## attach the number of seed trees per plot
    harv.data.2.pine.low.si <-
      merge(harv.data.1.pine.low.si
           ,harv.plots.red.pine.low.si[,c("ustandID","seed.trees.per.plot")]
           ,by="ustandID")
    ## rank the trees according to their dbh
    harv.data.2.pine.low.si$dbh.rank <-
      ave(harv.data.2.pine.low.si$dbh.mm
         ,harv.data.2.pine.low.si$ustandID
         ,FUN=function(x){length(x)-rank(x,ties.method="random")+1})
    
    ## the ids of the seed trees
    seed.trees.pine.low.si <-
      harv.data.2.pine.low.si$treeid[harv.data.2.pine.low.si$dbh.rank <=
                                     harv.data.2.pine.low.si$seed.trees.per.plot]
    
    ## exlude the big trees from the selection    
    harv.trees.pine.low.si[which(tr$data$treeid %in% seed.trees.pine.low.si)] <- F
    
    
    ## The seed trees of the last period have to be removed
    if(i.period>0){    
      sp.lp.pine.low.si <-
        fl$ustandID[substr(fl$management[,this.period],1,1)== "2" &
                    plots.pine.low.si]
      st.lp.pine.low.si <- tr$data$ustandID %in% sp.lp.pine.low.si
      ## TRUE for the seed trees of the last period
      harv.trees.pine.low.si[which(st.lp.pine.low.si)] <- T
    }
  }
  
  ##--------##
  ## spruce ##
  ##--------##
  
  ##>>>>>>>>>>>>>>>>>##
  ## high site-index ##
  ##>>>>>>>>>>>>>>>>>##  
  ##<<<<<<<<<<<##
  ## clear cut ##
  ##<<<<<<<<<<<##
  if(mic.specification[["final.felling.spruce.high.si"]] %in% c("clear.cut")){
    
    ## the plots in which final felling might be executed in this stratum
    harv.plots.spruce.high.si <- fl$ustandID[management$final.felling &
                                             plots.spruce.high.si]
    
    ## select trees from below
    ## up to a certain cumulative volume
    prob.harv.plots.spruce.high.si <-
      as.logical(ave(common.vars$vol.prob.cc.left,
                     common.vars$i.stand,
                     FUN=function(x){
                       X1 <- rep(F,length(x))
                       X1[which(x <= x[which.min(abs(x-mic.specification[["prop.vol.left.cc.spruce.high.si"]]))])] <- T
                       return(X1)
                     }))
    ## the selected trees in this stratum 
    harv.trees.spruce.high.si <-
      tr$data$ustandID %in% harv.plots.spruce.high.si & prob.harv.plots.spruce.high.si
    
  }
  
  ##<<<<<<<<<<<<##
  ## seed trees ##
  ##<<<<<<<<<<<<##
  if(mic.specification[["final.felling.spruce.high.si"]] %in% c("seed.tree")){
    ## the plots in which final felling might be executed in this stratum
    final.felling.spruce.high.si <- management$final.felling & plots.spruce.high.si
    ## a dataframe with the stands with possible final felling
    ## standID and the number of trees represented by a tree on a plot
    harv.plots.spruce.high.si <-
      data.frame(ustandID=fl$ustandID[final.felling.spruce.high.si]
                ,tree2ha=fl$tree2ha[final.felling.spruce.high.si])
    ## the number of seed trees per ha
    ## random number per plot
    harv.plots.spruce.high.si$seed.trees.per.ha <-
      runif(min=mic.specification[["seed.trees.n.min.spruce.high.si"]]
           ,max=mic.specification[["seed.trees.n.max.spruce.high.si"]]
           ,n=nrow(harv.plots.spruce.high.si))
    ## the minimal number of seed trees per plot
    harv.plots.spruce.high.si$min.seed.trees.per.plot <-
      as.integer(harv.plots.spruce.high.si$seed.trees.per.ha /
                 harv.plots.spruce.high.si$tree2ha)
    ## the probability of having an additional tree on the plot
    harv.plots.spruce.high.si$prob.add.seed.tree.per.plot <-
      with(harv.plots.spruce.high.si,
      (seed.trees.per.ha/tree2ha) - min.seed.trees.per.plot)
    ## the number of additional trees on the plots
    harv.plots.spruce.high.si$add.seed.tree.per.plot <-
      ifelse(runif(n = nrow(harv.plots.spruce.high.si)) >=
             harv.plots.spruce.high.si$prob.add.seed.tree.per.plot,1,0)
    ## the number of seed trees per plot
    harv.plots.spruce.high.si$seed.trees.per.plot <-
      with(harv.plots.spruce.high.si, min.seed.trees.per.plot + add.seed.tree.per.plot)
    ## only plots with seed trees
    harv.plots.red.spruce.high.si <-
      harv.plots.spruce.high.si[harv.plots.spruce.high.si$seed.trees.per.plot > 0, ]
    ## drop factor levels
    harv.plots.red.spruce.high.si$ustandID <- harv.plots.red.spruce.high.si$ustandID
    
    ## the trees on all the plots
    harv.trees.spruce.high.si <-
      tr$data$ustandID %in% harv.plots.red.spruce.high.si$ustandID
    
    ## a data.table of treeid, dbh and stand
    harv.data.1.spruce.high.si <- data.frame(
      treeid=tr$data$treeid[harv.trees.spruce.high.si]
      ,dbh.mm=tr$data$dbh.mm[harv.trees.spruce.high.si,this.period]
      ,ustandID=tr$data$ustandID[harv.trees.spruce.high.si]
    )
    
    ## attach the number of seed trees per plot
    harv.data.2.spruce.high.si <-
      merge(harv.data.1.spruce.high.si
           ,harv.plots.red.spruce.high.si[,c("ustandID","seed.trees.per.plot")]
           ,by="ustandID")
    ## rank the trees according to their dbh
    harv.data.2.spruce.high.si$dbh.rank <-
      ave(harv.data.2.spruce.high.si$dbh.mm
         ,harv.data.2.spruce.high.si$ustandID
         ,FUN=function(x){length(x)-rank(x,ties.method="random")+1})
    
    ## the ids of the seed trees
    seed.trees.spruce.high.si <-
      harv.data.2.spruce.high.si$treeid[harv.data.2.spruce.high.si$dbh.rank <=
                                        harv.data.2.spruce.high.si$seed.trees.per.plot]
    
    ## exlude the big trees from the selection    
    harv.trees.spruce.high.si[which(tr$data$treeid %in% seed.trees.spruce.high.si)] <- F
    
    
    ## The seed trees of the last period have to be removed
    if(i.period>0){    
      sp.lp.spruce.high.si <-
        fl$ustandID[substr(fl$management[,this.period],1,1)== "2" &
                    plots.spruce.high.si]
      st.lp.spruce.high.si <- tr$data$ustandID %in% sp.lp.spruce.high.si
      ## TRUE for the seed trees of the last period
      harv.trees.spruce.high.si[which(st.lp.spruce.high.si)] <- T
      
    }
  }
  
  
  ##>>>>>>>>>>>>>>>>>>>##
  ## medium site-index ##
  ##>>>>>>>>>>>>>>>>>>>##
  ##<<<<<<<<<<<##
  ## clear cut ##
  ##<<<<<<<<<<<##
  if(mic.specification[["final.felling.spruce.medium.si"]] %in% c("clear.cut")){
    
    ## the plots in which final felling might be executed in this stratum
    harv.plots.spruce.med.si <-
      fl$ustandID[management$final.felling & plots.spruce.med.si]
    
    ## select trees from below
    ## up to a certain cumulative volume
    prob.harv.plots.spruce.med.si <-
      as.logical(ave(common.vars$vol.prob.cc.left,
                     common.vars$i.stand,
                     FUN=function(x){
                       X1 <- rep(F,length(x))
                       X1[which(x <= x[which.min(abs(x-mic.specification[["prop.vol.left.cc.spruce.medium.si"]]))])] <- T
                       return(X1)
                     }))
    ## the selected trees in this stratum 
    harv.trees.spruce.med.si <-
      tr$data$ustandID %in% harv.plots.spruce.med.si & prob.harv.plots.spruce.med.si
    
  }  
  
  ##<<<<<<<<<<<<##
  ## seed trees ##
  ##<<<<<<<<<<<<##
  if(mic.specification[["final.felling.spruce.medium.si"]] %in% c("seed.tree")){
    ## the plots in which final felling might be executed in this stratum
    final.felling.spruce.med.si <- management$final.felling & plots.spruce.med.si
    ## a dataframe with the stands with possible final felling
    ## standID and the number of trees represented by a tree on a plot
    harv.plots.spruce.med.si <-
      data.frame(ustandID=fl$ustandID[final.felling.spruce.med.si]
                ,tree2ha=fl$tree2ha[final.felling.spruce.med.si])
    ## the number of seed trees per ha
    ## random number per plot
    harv.plots.spruce.med.si$seed.trees.per.ha <-
      runif(min=mic.specification[["seed.trees.n.min.spruce.medium.si"]]
           ,max=mic.specification[["seed.trees.n.max.spruce.medium.si"]]
           ,n=nrow(harv.plots.spruce.med.si))
    ## the minimal number of seed trees per plot
    harv.plots.spruce.med.si$min.seed.trees.per.plot <-
      as.integer(harv.plots.spruce.med.si$seed.trees.per.ha /
                 harv.plots.spruce.med.si$tree2ha)
    ## the probability of having an additional tree on the plot
    harv.plots.spruce.med.si$prob.add.seed.tree.per.plot <-
      with(harv.plots.spruce.med.si,
      (seed.trees.per.ha/tree2ha) - min.seed.trees.per.plot)
    ## the number of additional trees on the plots
    harv.plots.spruce.med.si$add.seed.tree.per.plot <-
      ifelse(runif(n=nrow(harv.plots.spruce.med.si)) >=
             harv.plots.spruce.med.si$prob.add.seed.tree.per.plot,1,0)
    ## the number of seed trees per plot
    harv.plots.spruce.med.si$seed.trees.per.plot <-
      with(harv.plots.spruce.med.si,min.seed.trees.per.plot+add.seed.tree.per.plot)
    ## only plots with seed trees
    harv.plots.red.spruce.med.si <-
      harv.plots.spruce.med.si[harv.plots.spruce.med.si$seed.trees.per.plot>0,]
    ## drop factor levels
    harv.plots.red.spruce.med.si$ustandID <- harv.plots.red.spruce.med.si$ustandID
    
    ## the trees on all the plots
    harv.trees.spruce.med.si <-
      tr$data$ustandID %in% harv.plots.red.spruce.med.si$ustandID
    
    ## a data.table of treeid, dbh and stand
    harv.data.1.spruce.med.si <- data.frame(
      treeid=tr$data$treeid[harv.trees.spruce.med.si]
      ,dbh.mm=tr$data$dbh.mm[harv.trees.spruce.med.si,this.period]
      ,ustandID=tr$data$ustandID[harv.trees.spruce.med.si]
    )
    
    ## attach the number of seed trees per plot
    harv.data.2.spruce.med.si <-
      merge(harv.data.1.spruce.med.si
           ,harv.plots.red.spruce.med.si[,c("ustandID","seed.trees.per.plot")]
           ,by="ustandID")
    ## rank the trees according to their dbh
    harv.data.2.spruce.med.si$dbh.rank <-
      ave(harv.data.2.spruce.med.si$dbh.mm
         ,harv.data.2.spruce.med.si$ustandID
         ,FUN=function(x){length(x)-rank(x,ties.method="random")+1})
    
    ## the ids of the seed trees
    seed.trees.spruce.med.si <-
      harv.data.2.spruce.med.si$treeid[harv.data.2.spruce.med.si$dbh.rank <=
                                       harv.data.2.spruce.med.si$seed.trees.per.plot]
    
    ## exlude the big trees from the selection    
    harv.trees.spruce.med.si[which(tr$data$treeid %in% seed.trees.spruce.med.si)] <- F
    
    
    ## The seed trees of the last period have to be removed
    if(i.period>0){    
      sp.lp.spruce.med.si <-
        fl$ustandID[substr(fl$management[,this.period],1,1)== "2" & plots.spruce.med.si]
      st.lp.spruce.med.si <- tr$data$ustandID %in% sp.lp.spruce.med.si
      ## TRUE for the seed trees of the last period
      harv.trees.spruce.med.si[which(st.lp.spruce.med.si)] <- T
      
    }
  }
  
  ##>>>>>>>>>>>>>>>>##
  ## low site-index ##
  ##>>>>>>>>>>>>>>>>##  
  ##<<<<<<<<<<<##
  ## clear cut ##
  ##<<<<<<<<<<<##
  if(mic.specification[["final.felling.spruce.low.si"]] %in% c("clear.cut")){
    
    ## the plots in which final felling might be executed in this stratum
    harv.plots.spruce.low.si <- fl$ustandID[management$final.felling & plots.spruce.low.si]
    
    ## select trees from below
    ## up to a certain cumulative volume
    prob.harv.plots.spruce.low.si <-
      as.logical(ave(common.vars$vol.prob.cc.left,
                     common.vars$i.stand,
                     FUN=function(x){
                       X1 <- rep(F,length(x))
                       X1[which(x <= x[which.min(abs(x-mic.specification[["prop.vol.left.cc.spruce.low.si"]]))])] <- T
                       return(X1)
                     }))
    ## the selected trees in this stratum 
    harv.trees.spruce.low.si <- tr$data$ustandID%in%harv.plots.spruce.low.si &
      prob.harv.plots.spruce.low.si
  }
  
  ##<<<<<<<<<<<<##
  ## seed trees ##
  ##<<<<<<<<<<<<##
  if(mic.specification[["final.felling.spruce.low.si"]] %in% c("seed.tree")){
    ## the plots in which final felling might be executed in this stratum
    final.felling.spruce.low.si <- management$final.felling & plots.spruce.low.si
    ## a dataframe with the stands with possible final felling
    ## standID and the number of trees represented by a tree on a plot
    harv.plots.spruce.low.si <- data.frame(
      ustandID=fl$ustandID[final.felling.spruce.low.si]
     ,tree2ha=fl$tree2ha[final.felling.spruce.low.si])
    ## the number of seed trees per ha
    ## random number per plot
    harv.plots.spruce.low.si$seed.trees.per.ha <-
      runif(min=mic.specification[["seed.trees.n.min.spruce.low.si"]]
           ,max=mic.specification[["seed.trees.n.max.spruce.low.si"]]
           ,n=nrow(harv.plots.spruce.low.si))
    ## the minimal number of seed trees per plot
    harv.plots.spruce.low.si$min.seed.trees.per.plot <-
      as.integer(harv.plots.spruce.low.si$seed.trees.per.ha /
                 harv.plots.spruce.low.si$tree2ha)
    ## the probability of having an additional tree on the plot
    harv.plots.spruce.low.si$prob.add.seed.tree.per.plot <-
      with(harv.plots.spruce.low.si,(seed.trees.per.ha/tree2ha)-min.seed.trees.per.plot)
    ## the number of additional trees on the plots
    harv.plots.spruce.low.si$add.seed.tree.per.plot <-
      ifelse(runif(n=nrow(harv.plots.spruce.low.si)) >=
             harv.plots.spruce.low.si$prob.add.seed.tree.per.plot,1,0)
    ## the number of seed trees per plot
    harv.plots.spruce.low.si$seed.trees.per.plot <-
      with(harv.plots.spruce.low.si,min.seed.trees.per.plot+add.seed.tree.per.plot)
    ## only plots with seed trees
    harv.plots.red.spruce.low.si <-
      harv.plots.spruce.low.si[harv.plots.spruce.low.si$seed.trees.per.plot>0,]
    ## drop factor levels
    harv.plots.red.spruce.low.si$ustandID <- harv.plots.red.spruce.low.si$ustandID
    
    ## the trees on all the plots
    harv.trees.spruce.low.si <-
      tr$data$ustandID %in% harv.plots.red.spruce.low.si$ustandID
    
    ## a data.table of treeid, dbh and stand
    harv.data.1.spruce.low.si <- data.frame(
      treeid=tr$data$treeid[harv.trees.spruce.low.si]
      ,dbh.mm=tr$data$dbh.mm[harv.trees.spruce.low.si,this.period]
      ,ustandID=tr$data$ustandID[harv.trees.spruce.low.si]
    )
    
    ## attach the number of seed trees per plot
    harv.data.2.spruce.low.si <- m
    merge(harv.data.1.spruce.low.si
         ,harv.plots.red.spruce.low.si[,c("ustandID","seed.trees.per.plot")]
         ,by="ustandID")
    ## rank the trees according to their dbh
    harv.data.2.spruce.low.si$dbh.rank <-
      ave(harv.data.2.spruce.low.si$dbh.mm
         ,harv.data.2.spruce.low.si$ustandID
         ,FUN=function(x){length(x)-rank(x,ties.method="random")+1})
    
    ## the ids of the seed trees
    seed.trees.spruce.low.si <-
      harv.data.2.spruce.low.si$treeid[harv.data.2.spruce.low.si$dbh.rank <=
                                       harv.data.2.spruce.low.si$seed.trees.per.plot]
    
    ## exlude the big trees from the selection    
    harv.trees.spruce.low.si[which(tr$data$treeid %in% seed.trees.spruce.low.si)] <- F
    
    
    ## The seed trees of the last period have to be removed
    if (i.period > 0) {    
      sp.lp.spruce.low.si <-
        fl$ustandID[substr(fl$management[, this.period], 1, 1) == "2" &
                    plots.spruce.low.si]
      st.lp.spruce.low.si <- tr$data$ustandID %in% sp.lp.spruce.low.si
      ## TRUE for the seed trees of the last period
      harv.trees.spruce.low.si[which(st.lp.spruce.low.si)] <- T

      
    }
  }
  
  ## a combined vector
  final.felling <- (
    harv.trees.pine.high.si|harv.trees.pine.med.si|harv.trees.pine.low.si|
    harv.trees.spruce.high.si|harv.trees.spruce.med.si|harv.trees.spruce.low.si)
  
  return(thinned.trees|final.felling)
  
}
## reassignInPackage("tree.removal.mic", "skogsimExtra", tree.removal.mic)
