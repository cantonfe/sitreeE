###########################
## taper.function.spruce ##
###########################
## Functions for calculating the stemvolume, the topvolume, and 
## the fraction of the topvolume

############
## Spruce ##
############
##----------------------------------------------------------------------------------------------------------
taper.function.spruce <- function(height.dm
                                , dbh.mm
                                , saw.d.mm
                                , pulp.d.mm
                                , min.l.dm){

  ## change units, for spruce height in dm
  ## height.dm <- height.dm
  ## dbh.mm <- dbh.mm
  saw.d.cm  <- saw.d.mm/10 ## min diam in cm
  pulp.d.cm <- pulp.d.mm/10
  
  min.l.intern <- min.l.dm/10  ## length in m
  
  is.height <- length(height.dm) > 1
  
  ## a matrix wih the heights
  height.matrix.m <-  height.dm/10 * ## this height is expected in m
    matrix( c(0, 0.05, seq(0.1, 1, 0.1) ),
           nrow = length(height.dm),
           ncol = 12, byrow =TRUE)
  
  ## an empty matrix for the corresponding diameters
  diam.matrix.cm <- matrix(NA,nrow=length(height.dm),ncol=12)
  
  ## Calculation of the diameters at given relative heights
  diam.matrix.cm[,3]  <- (5.737+(1.066*dbh.mm) -
                          (0.067*height.dm) -
                          (0.0234*dbh.mm**2/100) -
                          (0.008*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,4]  <- (22.732+(0.891*dbh.mm) -
                          (0.154*height.dm) -
                          (0.0584*dbh.mm**2/100) + (0.0868*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,1:2] <- diam.matrix.cm[,3] + diam.matrix.cm[,3] - diam.matrix.cm[,4]
  diam.matrix.cm[,5]  <- (21.577+(0.772*dbh.mm) -
                          (0.12*height.dm) -
                          (0.0714*dbh.mm**2/100)+(0.118*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,6]  <- (
    25.31 +
    (0.631  * dbh.mm) -
    (0.0911 * height.dm) -
    (0.0715 * dbh.mm**2/100) +
    (0.132  * height.dm * dbh.mm/100)
  )/10
  
  diam.matrix.cm[,7]  <- (
    25.735 +
    (0.492  * dbh.mm) -
    (0.0433 * height.dm) -
    (0.0682 * dbh.mm**2/100) +
    (0.136  * height.dm * dbh.mm/100)
  )/10
  diam.matrix.cm[,8]  <- (25.003+
                          (0.356*dbh.mm) +
                          (0.0022*height.dm) -
                          (0.0611*dbh.mm**2/100)+
                          (0.131*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,9]  <- (23.606+(0.213*dbh.mm) +
                          (0.051*height.dm) -
                          (0.0467*dbh.mm**2/100)+(0.113*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,10]  <- (16.721+(0.099*dbh.mm) +
                           (0.099*height.dm) -
                           (0.0301*dbh.mm**2/100) + (0.0802*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,11]  <- (8.975+(0.0237*dbh.mm) +
                           (0.0873*height.dm) -
                           (0.0148*dbh.mm**2/100)+(0.0441*height.dm*dbh.mm/100))/10
  ##rm(height.dm, dbh.mm)
  diam.matrix.cm[,12]  <- 0

  vol.ct <- function(r1, r2, h.cm) 1/3 * pi * (r1^2 + r1 * r2 + r2^2) * h.cm
  
  ## calculating the volumes of the stem-parts
  volumes <- vol.ct(r1 = diam.matrix.cm[,-12]/2,
                    r2 = diam.matrix.cm[,-1] /2,
                    ## height in m, radius in cm, dm to cm * 100
                    h.cm  = (height.matrix.m[,-1] - height.matrix.m[,-12]) * 100)
  
  
  ## calculating the volume corrections
  vol.ct.corrections <- function(r1, r2, min.pulp.r, h.m) {
    1/3 * pi * (r1^2 + (r1 * min.pulp.r) + (min.pulp.r^2)) *
      (h.m * 100 * (r1 - min.pulp.r)/(r1 - r2) )
  }

  
  ## volume corrections is the volume we need to add
  volume.stem.corrections <-  vol.ct.corrections(
    r1      = diam.matrix.cm[,-12]/2,
    r2      = diam.matrix.cm[, -1]/2,
    min.pulp.r = pulp.d.cm/2, 
    h.m     = height.matrix.m[,-1] - height.matrix.m[,-12]
  )
  ## Corrections are made only on the part of the stem that changes between smaller and larger
  ## than pulp min diameter
  volume.stem.corrections[!(diam.matrix.cm[,-1] < pulp.d.cm &
                            diam.matrix.cm[,-12] >= pulp.d.cm)] <- 0
  
  ## calculating the length corrections
  length.stemsaw.corrections <- height.matrix.m[,-12] +
    ( height.matrix.m[,-1]   -  height.matrix.m[,-12] ) * (
      ( diam.matrix.cm[,-12]/2 - saw.d.cm/2 )  / ( diam.matrix.cm[,-12]/2 - diam.matrix.cm[,-1]/2 )
    )
  ## The corrections for D005 is zero
  length.stemsaw.corrections[, 1] <- 0
  length.stemsaw.corrections[ !(diam.matrix.cm[,-1] < saw.d.cm &
                                diam.matrix.cm[,-12] >= saw.d.cm)] <- 0
  
  ## Calculate the Stemvolume -- includes pulp and saw wood
  stemvolume.matrix <- volumes
  stemvolume.matrix[diam.matrix.cm[,-1] < pulp.d.cm] <- 0
  stemvolume.matrix <- stemvolume.matrix + volume.stem.corrections

  if(is.height){
    stemvolume <- rowSums(stemvolume.matrix)
  } else {
    stemvolume <- sum(stemvolume.matrix)
  }

  ## Calculate sawvolume
  sawvolume.matrix <- volumes
  sawvolume.matrix[diam.matrix.cm[,-1] < saw.d.cm] <- 0
  volume.corrections.saw <-  vol.ct.corrections(
    r1      = diam.matrix.cm[,-12]/2,
    r2      = diam.matrix.cm[,-1]/2,
    min.pulp.r = saw.d.cm/2, ## this should be saw here
    h.m     = height.matrix.m[,-1] - height.matrix.m[,-12]
  )
  volume.corrections.saw[ !(diam.matrix.cm[,-1] < saw.d.cm &
                            diam.matrix.cm[,-12] >= saw.d.cm)] <- 0
  
  sawvolume.matrix <- sawvolume.matrix + volume.corrections.saw

  if(is.height){
    sawvolume <- rowSums(sawvolume.matrix)
  } else {
    sawvolume <- sum(sawvolume.matrix)
  }
  
  ## Now we have totalvolume, stemvolume, and sawvolume, we need to know the length of the stem
  ## Calculate the length of the stem
  stemsawlength.matrix <- height.matrix.m[, -1]
  stemsawlength.matrix[diam.matrix.cm[,-1] < saw.d.cm] <- 0
  stemsawlength.matrix  <- stemsawlength.matrix + length.stemsaw.corrections
  
  if(is.height){
    stemsawlength.matrix[, ncol(stemsawlength.matrix)] <- 0
    stemsawlength <- apply(stemsawlength.matrix, 1, max)
  } else {
    stemsawlength.matrix[length(stemsawlength.matrix)] <- 0
    stemsawlength <- max(stemsawlength.matrix)
  }
  ##rm(height.matrix.m)
  
  ## Calculatate the volume of the sawlog
  sawvolume <- ifelse(stemsawlength >= min.l.intern, sawvolume, 0)
  
  ## return a datafae
  ## we should only return the saw/pulp proportion,
  stem.vol.m3 <- stemvolume/1000000
  all.vol.m3 <- rowSums(volumes)/1000000
  saw.vol.m3  <- sawvolume/1000000
  per.saw.all.vol <- saw.vol.m3 / all.vol.m3
  per.pulp.all.vol <- (stem.vol.m3 - saw.vol.m3)/ all.vol.m3
  return(data.frame(per.saw.all.vol, per.pulp.all.vol, all.vol.m3)) ## to be applied to volume under bark
  
}
##----------------------------------------------------------------------------------------------------------
## reassignInPackage("taper.function.spruce", "skogsim", taper.function.spruce)
