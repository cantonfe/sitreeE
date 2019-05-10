

PlotDataToLong <- function(pd){
  is.frame <- lapply(pd, function(x) is.data.frame(x))
  not.frame <- names(pd)[!unlist(is.frame)]
  pd.dt <- as.data.frame.list(pd[not.frame])
  pd.dt <- data.table(pd.dt)

  if ('stringsAsFactors' %in% names(pd[['stand.age.years']]))  {
    pd[['stand.age.years']] <-
      pd[['stand.age.years']][, -match('stringsAsFactors', names(pd[['stand.age.years']]))]
  }
  ## just to make cran happy
plot.id <- NULL
  my.plot.id <- pd.dt[, plot.id]
  stand.age.years <- data.table(pd[['stand.age.years']])
  stand.age.years[, plot.id := my.plot.id]
  management <- data.table(pd[['management']])
  management[, plot.id := my.plot.id]

  
  age <- melt(stand.age.years, 
              variable.name = 'period',
              value.name = 'stand.age.years',
              measure = names(pd[['stand.age.years']])
              
              )
  
  mgmt <- melt(management,  
               variable.name = 'period',
               value.name = 'management',
               measure = names(pd[['management']])
               )
  all.long <- merge(mgmt, age, on = c('plot.id', 'period'), all = TRUE)

  all.plot <- all.long[pd.dt, on = 'plot.id']

  return(all.plot)
  
}
