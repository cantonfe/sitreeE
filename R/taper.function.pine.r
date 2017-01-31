#########################
## taper.function.pine ##
#########################
## Functions for calculating the stemvolume, the topvolume, and 
## the fraction of the topvolume

##########
## Pine ##
##########
##----------------------------------------------------------------------------------------------------------
taper.function.pine <- function(height.dm
                              , dbh.mm
                              , saw.d.mm
                              , pulp.d.mm
                              , min.l.dm){
  saw.d.cm<- saw.d.mm/10 ## min diam in cm
  pulp.d.cm <- pulp.d.mm/10
  
  min.l.intern <- min.l.dm/10  ## length in m
  
  ##bottom.intern <- rep(bottom.d.mm/10,length(height.dm))
  ##top.d.intern <- rep(top.d.mm/10,length(height.dm))
  
  is.height <- length(height.dm)>1
  ## a matrix wih the heights
  height.matrix.m <-  height.dm/10 * ## this height is expected in m
    matrix( c(0, 0.05, seq(0.1, 1, 0.1) ),
           nrow = length(height.dm),
           ncol = 12, byrow =TRUE)
  
  ## an empty matrix for the corresponding diameters
  diam.matrix.cm <- matrix(NA,nrow=length(height.dm),ncol=12)
  
  ## Calculation of the diameters at given relative heights
  diam.matrix.cm[,1:2] <-   (-4.326+(1.167*dbh.mm)+(0.004*dbh.mm**2/100)+
                             (0.026*height.dm)-(0.084*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,3]  <- (5.373+(1.049*dbh.mm)-(0.01*dbh.mm**2/100)-
                          (0.048*height.dm)-(0.023*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,4]  <- (3.856+(0.975*dbh.mm)-(0.016*dbh.mm**2/100)-
                          (0.029*height.dm)-(0.01*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,5]  <- (2.616+(0.92*dbh.mm)-(0.017*dbh.mm**2/100)-
                          (0.014*height.dm)-(0.01*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,6]  <- (2.194+(0.862*dbh.mm)-(0.021*dbh.mm**2/100)-
                          (0.019*height.dm))/10
  diam.matrix.cm[,7]  <- (2.349+(0.763*dbh.mm)-(0.023*dbh.mm**2/100)-
                          (0.011*height.dm)+(0.014*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,8]  <- (0.467+(0.649*dbh.mm)-(0.026*dbh.mm**2/100)+
                          (0.015*height.dm)+(0.028*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,9]  <- (-3.768+(0.512*dbh.mm)-(0.027*dbh.mm**2/100)+
                          (0.071*height.dm)+(0.03*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,10]  <- (-5.607+(0.328*dbh.mm)-(0.02*dbh.mm**2/100)+
                           (0.063*height.dm)+(0.029*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,11]  <- (0.242+(0.134*dbh.mm)-(0.015*dbh.mm**2/100)+
                           (0.063*height.dm)+(0.034*height.dm*dbh.mm/100))/10
  diam.matrix.cm[,12]  <- 0

  
  vol.ct <- function(r1, r2, h.cm) 1/3 * pi * (r1^2 + r1 * r2 + r2^2) * h.cm
  
  ## calculating the volumes of the stem-parts
  volumes <- vol.ct(r1 = diam.matrix.cm[,-12]/2,
                    r2 = diam.matrix.cm[,-1] /2,
                    ## height in m, radius in cm, dm to cm * 100
                    h.cm  = (height.matrix.m[,-1] - height.matrix.m[,-12]) * 100)
  
  
  ## calculating the volume corrections
  vol.ct.corrections <- function(r1, r2, min.pulp.r, h.m) {
    1/3 * pi * (r1^2 + r1 * min.pulp.r + min.pulp.r^2) *
      (h.m*100 * (r1 - min.pulp.r)/(r1 - r2) )
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
      ( diam.matrix.cm[,-12]/2 - saw.d.cm/2 )  /
      ( diam.matrix.cm[,-12]/2 - diam.matrix.cm[,-1] / 2 )
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
  volume.corrections.saw[!(diam.matrix.cm[,-1] < saw.d.cm &
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
## reassignInPackage("taper.function.pine", "skogsimExtra", taper.function.pine)
