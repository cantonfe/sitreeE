#########################
## tree.ingrowth.mic.r ##
#########################
## tree ingrowth according to MIC

recr.imputation.mic <- function (tr ,
                                 fl , 
                                 common.vars,
                                 i.period  ,
                                 this.period,
                                 next.period,
                                 management ,
                                 period.length ,
                                 vars.required,
                                 ...) {

  others <- list(...)

  fn.recr.pars  <- list(fn.natural = others$fn.natural,
                        fn.impute  = others$fn.impute,
                        fn.mic     = others$fn.mic
                        )
  
  current.mics <- mng.options$current.mics
  
  nat.ingrowth <- do.call(fn.recr.pars$fn.natural,
                          args = list(
                            tr = tr,
                            fl = fl, 
                            common.vars = common.vars,
                            i.period    = i.period,
                            period.length = period.length,
                            fn.recr.pars  = fn.recr.pars
                          ))
  
 
  print('Passed natural ingrowth')
  
  ## Calculate the ingrowth in the managed stand
  if (sum(management$ingrowth) > 0){
  art.ingrowth <- do.call(fn.recr.pars$fn.mic,
                          args = list(
                            mic.specification = current.mics,
                            management  = management,
                            tr          = tr,
                            fl          = fl, 
                            common.vars = common.vars,
                            i.period    = i.period,
                            this.period = this.period,
                            next.period = next.period,
                            period.length = period.length,
                            mng.options = mng.options
                            
                          ))
  } else art.ingrowth <- NULL
  
  print(paste0('Art. ingrowth ', length(art.ingrowth$treeid)))
  ## exclude the managed plots
  no.imp.recr <- fl$ustandID[management$waiting | ## is waiting for regeneration
                             management$final.felling | ## it has been recently felled
                             management$ingrowth #time since harvest is larger than the time to reach the dbh and there are no trees
                             #!is.na(fl$time.since.final.felling)
                             ]
  to.keep <- which(!(nat.ingrowth$ustandID %in% no.imp.recr))
  
  nat.ingrowth$ustandID  <- nat.ingrowth$ustandID[to.keep]
  nat.ingrowth$treeid    <- nat.ingrowth$treeid[to.keep]
  nat.ingrowth$dbh.mm    <- nat.ingrowth$dbh.mm[to.keep,]
  nat.ingrowth$height.dm <- nat.ingrowth$height.dm[to.keep,]
  nat.ingrowth$yrs.sim   <- nat.ingrowth$yrs.sim[to.keep]
  nat.ingrowth$tree.sp   <- nat.ingrowth$tree.sp[to.keep]
  
  ## combine the two ingrowth-lists
  ingrowth <- list()
if (sum(management$ingrowth) > 0){
  ## remove trees of artifitial ingrowth with dbh below 50 mm
  is.large.enough <- art.ingrowth$dbh.mm[, next.period] >= 50
  
  ingrowth$ustandID <- factor(c(as.character(art.ingrowth$ustandID[is.large.enough]),
                                as.character(nat.ingrowth$ustandID)),
                              levels = levels(fl$ustandID))
  ingrowth$treeid <- max(tr$data[["treeid"]]) + (1:length(ingrowth$ustandID))
  ingrowth$dbh.mm <- rbind(art.ingrowth$dbh.mm[is.large.enough,], nat.ingrowth$dbh.mm)
  ingrowth$height.dm <- rbind(art.ingrowth$height.dm[is.large.enough,],
                              nat.ingrowth$height.dm)
  ingrowth$yrs.sim <- c(art.ingrowth$yrs.sim[is.large.enough], nat.ingrowth$yrs.sim)
  ingrowth$tree.sp <- factor(levels(nat.ingrowth$tree.sp)[
    c(art.ingrowth$tree.sp[is.large.enough], nat.ingrowth$tree.sp)],
    levels=levels(nat.ingrowth$tree.sp))
} else {
  ingrowth$ustandID <- factor(
                                as.character(nat.ingrowth$ustandID),
                              levels = levels(fl$ustandID))
  ingrowth$treeid <- max(tr$data[["treeid"]]) + (1:length(ingrowth$ustandID))
  ingrowth$dbh.mm <- nat.ingrowth$dbh.mm
  ingrowth$height.dm <-     nat.ingrowth$height.dm
  ingrowth$yrs.sim <- nat.ingrowth$yrs.sim
  ingrowth$tree.sp <- factor(levels(nat.ingrowth$tree.sp)[ nat.ingrowth$tree.sp],
    levels=levels(nat.ingrowth$tree.sp))


}
  return(ingrowth)
}

## reassignInPackage("recr.imputation.mic", "skogsimExtra", recr.imputation.mic)
