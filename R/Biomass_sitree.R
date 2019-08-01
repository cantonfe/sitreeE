

biomass.sitree <- function(tr, plot.data){
## just to make CRAN happy
period <- i.height.dm <- tree2ha <- i.tree2ha <- tree.sp <- i.tree.sp <- NULL


  if (class(tr) == 'trList') {
    dbh.mm <- data.table(tr$data$dbh.mm)
    height.dm <- data.table(tr$data$height.dm)
    dbh.mm <- data.table(dbh.mm,
                         treeid = tr$data$treeid,
                         plot.id = tr$data$plot.id)
    height.dm <- data.table(height.dm,
                            treeid = tr$data$treeid,
                            plot.id = tr$data$plot.id)
     ## long to wide
    dbh.mm <- melt(dbh.mm,  id.vars = c("treeid", "plot.id"),
                   variable.name = 'period',
                   value.name = 'dbh.mm')
    height.dm <- melt(height.dm,  id.vars = c("treeid", "plot.id"),
                      variable.name = 'period',
                      value.name = 'height.dm')
  
  
  } else {
    dbh.mm <- data.table(dbh.mm = tr$last.measurement$dbh.mm,
                         treeid = tr$last.measurement$treeid,
                         plot.id = tr$data$plot.id)
    height.dm <- data.table(height.dm = tr$last.measurement$height.dm,
                            treeid = tr$last.measurement$treeid,
                            plot.id = tr$data$plot.id)
    if ('found.removed' %in% names(tr$last.measurement)){
      dbh.mm[, period := tr$last.measurement$found.removed]
      height.dm[, period := tr$last.measurement$found.removed]
    } else {
      dbh.mm[, period := tr$last.measurement$found.dead]
      height.dm[, period := tr$last.measurement$found.dead]
    }
  }
  ## just in case, but they are in the same exact order
  dbh.mm[height.dm, height.dm := i.height.dm, on = c('treeid', 'period')]
  pd <- data.table(
                   plot.id = plot.data$plot.id,
                   tree2ha = plot.data$tree2ha)
  pd.tr <- data.table(treeid = tr$data$treeid,
                      tree.sp = tr$data$tree.sp)
  dbh.mm[pd, tree2ha := i.tree2ha, on = 'plot.id']
  dbh.mm[pd.tr, tree.sp := i.tree.sp, on = 'treeid']
  
  ## calculate volume
  dbh.mm[dbh.mm > 0, c('living.branches',
                       'dead.branches', 'stem.wood'  ,
                       'stump', 'bark', 'usoil',
                       'rot1', 'rot2', 'foliage',            
                       'biomass.aboveground.kg',
                       'biomass.belowground.kg',
                       'biomass.total.kg') :=
                       biomass.norway.sitree(dbh.mm, height.dm, tree.sp)]
  in.kg <- c('living.branches',
             'dead.branches', 'stem.wood'  ,
             'stump', 'bark', 'usoil',
             
             'rot1', 'rot2', 'foliage' )
  setnames(dbh.mm, in.kg, paste0(in.kg, '.kg'))

  biomass <- dbh.mm
  return(biomass)
}
