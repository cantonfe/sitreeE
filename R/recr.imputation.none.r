#########################
## tree.ingrowth.mic.r ##
#########################
## tree ingrowth according to MIC

recr.imputation.none <- function (tr ,
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
                        fn.impute  = others$fn.impute
                        )
  
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
  art.ingrowth <- NULL
  
  
  ## combine the two ingrowth-lists
  ingrowth <- list()
  ingrowth$ustandID <- factor(
    as.character(nat.ingrowth$ustandID),
    levels = levels(fl$ustandID))
  ingrowth$treeid <- max(tr$data[["treeid"]]) + (1:length(ingrowth$ustandID))
  ingrowth$dbh.mm <- nat.ingrowth$dbh.mm
  ingrowth$height.dm <-     nat.ingrowth$height.dm
  ingrowth$yrs.sim <- nat.ingrowth$yrs.sim
  ingrowth$tree.sp <- factor(levels(nat.ingrowth$tree.sp)[ nat.ingrowth$tree.sp],
                             levels=levels(nat.ingrowth$tree.sp))



  return(ingrowth)
}

## reassignInPackage("recr.imputation.mic", "skogsimExtra", recr.imputation.mic)
