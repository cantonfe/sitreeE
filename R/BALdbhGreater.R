

PBAL.dbh.greater <-  function(dbh.mm, dbh.mm.diff){

  BA <- pi * (dbh.mm/2/10)^2 ## mm²  to cm^2
  ord.x <- order(BA)

  ## Create a matrix to see which trees should be sum up for each row
  which.is.greater <- apply(as.matrix(dbh.mm),  1, FUN = function(x, dbh.mm.diff, all.dbh) {  
    all.dbh >= x + dbh.mm.diff
  }, dbh.mm.diff = dbh.mm.diff, all.dbh = dbh.mm)

  ## matrix of BA
  if (length(dbh.mm) > 1) {
    BA.mat <- matrix(BA, nrow = nrow(which.is.greater),
                     ncol = ncol(which.is.greater), byrow = FALSE)
    BA.greater <- BA.mat * which.is.greater
    to.be.returned <- colSums(BA.greater)
  } else to.be.returned <- 0
  
  return(to.be.returned)
}
