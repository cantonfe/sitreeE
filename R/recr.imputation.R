## returns a data.frame with the new trees
recr.imputation <- function (tr,
                             fl,
                             common.vars,
                             i.period,
                             period.length,
                             fn.recr.pars, ...) {

  fn <- fn.recr.pars$fn.impute
  t.SBA.m2.ha <- with(common.vars, SBA.m2.ha[i.tree])
  t.tph <- with(common.vars, tph[i.tree])
  i.no.trees <- !is.finite(t.SBA.m2.ha)
  t.SBA.m2.ha[i.no.trees] <- t.tph[i.no.trees] <- 0

  if (fn == "impute.plot"){

    t.SDI <- common.vars$SDI[common.vars$i.tree]
    t.QMD.cm <- common.vars$QMD.cm[common.vars$i.tree]
    t.pr.pine.ba <- common.vars$pr.spp.ba$pine[common.vars$i.tree]
    t.pr.spru.ba <- common.vars$pr.spp.ba$spru[common.vars$i.tree]
    t.SDI[i.no.trees] <-  t.QMD.cm[i.no.trees] <- 
      t.pr.pine.ba[i.no.trees] <- t.pr.spru.ba[i.no.trees] <- 0
    
    
    ## It returns a list with imputed new trees for each plot
    imp <- do.call(what = fn, 
                   args = list(
                     uID.stand  = as.character(fl$ustandID),
                     SDI        = t.SDI,  ## tree
                     QMD.cm     = t.QMD.cm, ##tree                      
                     SQ.spru    = common.vars$SQ.fl$SQ.spru, 
                     SQ.pine    = common.vars$SQ.fl$SQ.pine,
                     SQ.harw    = common.vars$SQ.fl$SQ.harw, 
                     pr.pine.ba = t.pr.pine.ba,# tree
                     pr.spru.ba = t.pr.spru.ba # tree
                   )
                   )

  }

  if (fn == "impute.plot.Bollandsas"){
    
    t.spru.ba <- common.vars$pr.spp.ba$spru[common.vars$i.tree]
    t.harw.ba <- common.vars$pr.spp.ba$harw[common.vars$i.tree]
    i.no.trees <- is.na(t.spru.ba)
    t.spru.ba[i.no.trees] <- t.harw.ba[i.no.trees] <- 0
    ## It returns a list with imputed new trees for each plot

    imp <- with(common.vars,
                do.call(what = fn, 
                        args = list(
                          uID.stand  = fl$ustandID,
                          lat.deg    = fl$lat.deg,
                          SI.m       = fl$SI.m,
                          SBA.m2.ha  = t.SBA.m2.ha,
                          tph        = t.tph,
                          pr.spru.ba = t.spru.ba,
                          pr.harw.ba = t.harw.ba
                        )
                        )
                )
  }
  
  ## make the ingrowth proportional to plot size in partial plots
  partialplot <- substr(names(imp), 8,9) != 0
  have.ingrowth <- lapply(imp, FUN = nrow) > 0
  my.stands <- match(names(imp)[partialplot & have.ingrowth], fl$ustandID)
  plots.prop <- fl$subplot.size.m2[my.stands] / fl$plot.size.m2[my.stands]

  imp[partialplot & have.ingrowth] <-
    lapply(names(imp)[partialplot & have.ingrowth], FUN = function(x){
      size <- plots.prop[which(names(imp)[partialplot & have.ingrowth] == x)]
      X <- imp[[which(names(imp) == x)]]
      return(X [runif(nrow(X), 0, 1) > size,])
    })

  ## which have large distances and/or high densities
  ## if distance large, use other approach.. mah.plot uses aboniet, SBA and lat.deg
  i.far <- attr(imp, 'distance') > quantile(c(mah.plot$neiDstRefs), 0.99)
  
  have.ingrowth <- lapply(imp, FUN = nrow) > 0
  
  if (fn == "impute.plot"){
    my.stands <- have.ingrowth & (
      (i.far & t.tph > 2000 ) |
      (t.SBA.m2.ha > 140)
    )
  }
  if (fn == "impute.plot.Bollandsas"){
    t.SDI <- common.vars$SDI[common.vars$i.tree]
    
    my.stands <- have.ingrowth & (
      (i.far  | t.SBA.m2.ha > 140 | t.tph > 2000)
    )
  }

  imp[my.stands] <- lapply(
    imp[my.stands],
    FUN = function(x) (x[is.na(x[1,]),]) ) ## remove rows
  ## remove empty data.frames
  imp <- Filter(function(x) dim(x)[1] > 0, imp)

  dbh.mm <- unlist(lapply(imp, "[", c("dbh.mm.tr1")))
  tree.sp <- unlist(lapply(imp, "[", c("treslag.tr1")))
  ustandID <- factor(substr(names(dbh.mm), 1, 8), levels = fl$ustandID)
  imp <- data.frame(ustandID = ustandID, dbh.mm = dbh.mm, tree.sp)
  rownames(imp) <- NULL
  
  ##names.not.null <- names(imp[!sapply(imp, is.null)])
  ##imp.1[, uID.stand := rep(names(imp), times = unlist(lapply(imp, nrow)))]
  ##imp <- imp.1
  
  ## I need to add some fields, for example treeid, yrs.sim
  ## we assume they got in (ingrowth) in the middle of the period
  imp$yrs.sim <- period.length/2 
  imp$treeid <- max(tr$data[["treeid"]]) + (1:nrow(imp))

  ## we need to calculate height for the trees, we could also impute it
  ## since it is used for nothing so far,
  ## we will assign it for now just 36dm (1% quantile)
  imp$height.dm <- 32
  ## it needs to be correctly ordered
  
  imp <- imp[, names(tr$data)]

  foo2 <- function(mvec) {
    mdt <- matrix(0, nrow = length(mvec), ncol = (tr$nperiods +1))
    mdt[, 1 + (i.period + 1)] <- mvec ## periods start at zero, columns at 1
    colnames(mdt) <- paste("t", 0:tr$nperiods, sep = "")
    return(as.data.frame(mdt))
  }
  trl.imp <- list(
    ustandID  = imp$ustandID,
    treeid    = imp$treeid,
    dbh.mm    = foo2(imp$dbh.mm),
    height.dm = foo2(imp$height.dm),
    yrs.sim   = imp$yrs.sim,
    tree.sp   = factor(imp$tree.sp, levels = levels(tr$data$tree.sp))
  )
  
  return(trl.imp)
}
## reassignInPackage("recr.imputation", "skogsimExtra", recr.imputation)
