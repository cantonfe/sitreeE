



volume.sitree <- function(tr, plot.data){

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
    dbh.mm[, period := tr$last.measurement$found.removed]
    height.dm[, period := tr$last.measurement$found.removed]
  }
  ## just in case, but they are in the same exact order
  dbh.mm[height.dm, height.dm := i.height.dm, on = c('treeid', 'period')]
  pd <- data.table(kom = plot.data$kom,
                   plot.id = plot.data$plot.id,
                   tree2ha = plot.data$tree2ha)
  pd.tr <- data.table(treeid = tr$data$treeid,
                      tree.sp = tr$data$tree.sp)
  dbh.mm[pd, kom := i.kom, on = 'plot.id']
  dbh.mm[pd, tree2ha := i.tree2ha, on = 'plot.id']
  dbh.mm[pd.tr, tree.sp := i.tree.sp, on = 'treeid']
  
  ## calculate volume
  dbh.mm[dbh.mm > 0, c('vol.w.tr.m3', 'vol.wo.tr.m3') :=
                       volume.norway(dbh.mm, height.dm, tree.sp, kom)]

  volume <- dbh.mm
  return(volume)
}
