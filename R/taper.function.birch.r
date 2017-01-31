##########################
## taper.function.birch ##
##########################
## Functions for calculating the stemvolume, the topvolume, and 
## the fraction of the topvolume

###########
## Birch ##
###########
##----------------------------------------------------------------------------------------------------------
taper.function.birch <- function(height.dm
                                  , dbh.mm
                                  , saw.d.mm
                                  , pulp.d.mm
                                  , min.l.dm){
  ## dbh in cm and height in m
  ## change units
  height.dm <- height.dm/10 ## in m
  dbh.mm <- dbh.mm/10 ## in cm
  
 is.height <- length(height.dm)>1
 saw.d.cm<- saw.d.mm/10 ## min diam in cm
    pulp.d.cm <- pulp.d.mm/10
    
    min.l.intern <- min.l.dm/10  ## length in m
    
    
  ## a matrix wih the heights
  height.matrix.m <-  height.dm *
                      matrix( c(0, 0.05, seq(0.1, 1, 0.1) ),
                             nrow = length(height.dm),
                             ncol = 12, byrow =TRUE)
  ## an empty matrix for the corresponding diameters
  diam.matrix.cm <- matrix(NA,nrow=length(height.dm),ncol=12)
  
  ## Calculation of the diameters at given relative heights
  diam.matrix.cm[,1:2] <- -0.7281+1.2378*dbh.mm-0.000713*dbh.mm**2+
      0.03389*height.dm-0.008624*dbh.mm*height.dm
  diam.matrix.cm[,3]  <- 0.7555+1.0576*dbh.mm-0.002393*dbh.mm**2-
      0.05046*height.dm-0.002195*dbh.mm*height.dm
  diam.matrix.cm[,4]  <- -0.0526+1.0029*dbh.mm-0.003578*dbh.mm**2-
      0.01996*height.dm-0.001757*dbh.mm*height.dm
  diam.matrix.cm[,5]  <- -0.1128+0.9309*dbh.mm-0.00415*dbh.mm**2-
      0.03471*height.dm+0.000104*dbh.mm*height.dm
  diam.matrix.cm[,6]  <- -0.1078+0.8386*dbh.mm-0.003765*dbh.mm**2-
      0.03555*height.dm+0.000783*dbh.mm*height.dm
  diam.matrix.cm[,7]  <- 0.433+0.6732*dbh.mm-(0.003217*dbh.mm**2)-
      0.04839*height.dm+0.003598*dbh.mm*height.dm
  diam.matrix.cm[,8]  <- 0.8532+0.5149*dbh.mm-0.004897*dbh.mm**2-
      0.073*height.dm+0.008327*dbh.mm*height.dm
  diam.matrix.cm[,9]  <- 0.9077+0.3411*dbh.mm-0.005083*dbh.mm**2-
      0.04507*height.dm+0.009218*dbh.mm*height.dm
  diam.matrix.cm[,10]  <- 0.3592+0.1687*dbh.mm-0.002882*dbh.mm**2+
      0.04374*height.dm+0.00497*dbh.mm*height.dm
  diam.matrix.cm[,11]  <- 0.0094+0.0638*dbh.mm-0.002122*dbh.mm**2+
      0.04122*height.dm+0.002883*dbh.mm*height.dm
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
    volume.corrections.saw[!(diam.matrix.cm[,-1] < saw.d.cm & diam.matrix.cm[,-12] >= saw.d.cm)] <- 0
    
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
