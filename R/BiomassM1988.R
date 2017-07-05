

biomass.M1988 <- function(tr, spp, this.period){

  i.spru <- spp == "spruce"
  i.pine <- spp == "pine"
  i.birc <- spp %in% c("birch", "other")

  ## some specificities for dead and removed trees
  if (class(tr) == "trListDead") {
    if ('found.dead' %in% names(tr$last.measurement)) {
      i.found.this.period <- tr$last.measurement$found.dead    == this.period
    } else {
      i.found.this.period <- tr$last.measurement$found.removed == this.period
    }
  } else {
    i.found.this.period <- rep(TRUE, length(tr$data$tree.sp))
  }

  
  if (sum (i.found.this.period ) != 0){
    biomass.kg <- data.frame(living.branches = rep(NA, length(i.found.this.period)),
                             dead.branches = NA,
                             stem.wood = NA,
                             stump = NA,
                             bark = NA,
                             stump.roots = NA,
                             roots1 = NA,
                             roots2 = NA,
                             foliage = NA)
    
    ## SPRUCE
    if (sum(i.spru)> 0){
      if (class(tr) == "trListDead") {
        d <- tr$last.measurement[["dbh.mm"]]    [i.found.this.period] [ i.spru]/10
        H <- tr$last.measurement[["height.dm"]] [i.found.this.period] [ i.spru]/10
      } else {
        d <- tr$data[["dbh.mm"]]   [ i.spru, this.period]/10 ## dbh should be in cm
        H <- tr$data[["height.dm"]][ i.spru, this.period]/10
      }
      ## Marklund 1988 G20 s. 50 
      dgr   <- exp(-4.6351 +  3.6518*(d/(d+18)) + 0.0493*H  + 1.0129*(log(H)))
      ## Marklund 1988 G12 s. 46  Inkluderer foliage
      crown  <- exp(-1.2063 + 10.9708*(d/(d+13)) - 0.0124*H - 0.4923*(log(H)))
      ## Marklund 1988 G16 s. 48 not used
      foliage   <- exp(-1.8551 +  9.7809*(d/(d+12))             - 0.4873*(log(H)))
      ## Marklund 1988 G8  s. 44 
      bark  <- exp(-3.4020 +  8.3089*(d/(d+15)) + 0.0147*H  + 0.2295*(log(H)))
      ## Marklund 1988 G5  s. 42  Trunc excluding bark, excluding stump 
      stem  <- exp(-2.3032 +  7.2309*(d/(d+14)) + 0.0355*H  + 0.7030*(log(H)))
      ## Marklund 1988 G26 s. 54 
      stub  <- exp(-3.3645 + 10.6686*(d/(d+17)))
      ## Marklund 1988 G31 s. 57 
      rot1  <- exp(-2.5706 +  7.6283*(d/(d+12)))       
      ## Marklund 1988 G28 s. 55 
      rot2  <- exp(-6.3851 + 13.3703*(d/(d+8)))

      ## Petterson og Sthål 2006 Root limit 2 mm 
      usoil  <- (exp(((0.32308)^2) / 2 + 4.58761 + (d*10) /
                       ((d*10) + 138) * 10.44035))/1000

      
      biomass.kg[i.found.this.period,][i.spru,] <- cbind(crown, dgr, stem, stub, bark, usoil,
                                                         rot1, rot2, foliage)
      ## since these equations produce results different from zero even
      ## when d and H are zero, we should correct it
      biomass.kg[i.found.this.period,][i.spru,][d == 0,] <- 0
    }

    
    ## PINE
    if (sum(i.pine) > 0){
      if (class(tr) == "trListDead") {
        d <- tr$last.measurement[["dbh.mm"]]   [i.found.this.period] [ i.pine]/10
        H <- tr$last.measurement[["height.dm"]][i.found.this.period] [ i.pine]/10
      } else {
        d <- tr$data[["dbh.mm"]]   [ i.pine, this.period]/10
        H <- tr$data[["height.dm"]][ i.pine, this.period]/10
      }
      
      dgr    <- exp(-5.8926 +  7.1270*(d/(d+10)) - 0.0465 * H + 1.1060*(log(H)))
      crown  <- exp(-2.5413 + 13.3955*(d/(d+10)) - 1.1955 *            (log(H)))
      foliage    <- exp(-3.4781 + 12.1095*(d/(d+ 7)) + 0.0413 * H - 1.5650*(log(H)))
      bark   <- exp(-3.2765 +  7.2482*(d/(d+16)) + 0.4487 *            (log(H)))
      stem   <- exp(-2.6864 +  7.6066*(d/(d+14)) + 0.0200 * H + 0.8658*(log(H)))
      stub   <- exp(-3.9657 + 11.0481*(d/(d+15)))
      rot1   <- exp(-3.8375 +  8.8795*(d/(d+10)))                
      rot2   <- exp(-6.3413 + 13.2902*(d/(d+ 9)))
      ## Petterson og Sthål 2006 Root limit 2 mm */
      usoil  <- (exp(((0.35449)^2)  / 2 + 3.44275 + (d*10) / ((d*10) + 113) * 11.06537))/1000
      
      biomass.kg[i.found.this.period,][i.pine,] <- cbind(crown, dgr, stem, stub, bark,
                                                         usoil, rot1, rot2, foliage)
      ## since these equations produce results different from zero even
      ## when d and H are zero, we should correct it
      biomass.kg[i.found.this.period,][i.pine,][d == 0,] <- 0
    }

    
    ## BIRCH
    if (sum(i.birc) > 0){
      if (class(tr) == "trListDead") {
        d <- tr$last.measurement[["dbh.mm"]]   [i.found.this.period] [ i.birc]/10
        H <- tr$last.measurement[["height.dm"]][i.found.this.period] [ i.birc]/10
      } else {
        d <- tr$data[["dbh.mm"]]   [ i.birc, this.period]/10
        H <- tr$data[["height.dm"]][ i.birc, this.period]/10
      }
      dgr  <- exp(-6.6237 + 11.2872*(d/(d+30))-0.3081*H+2.6821*(log(H)))
      crown  <- exp(-3.3633 + 10.2806*(d/(d+10)))
      ##BSTEM *0.11/0.52
      foliage  <- exp(-3.3045 + 8.1184*(d/(d+11)) + 0.9783*(log(H))) * 0.011/0.52 
      bark <- exp(-4.0778 + 8.3019*(d/(d+14)) + 0.7433*(log(H)))
      stem <- exp(-3.3045 + 8.1184*(d/(d+11)) + 0.9783*(log(H)))
      stub <- exp(-3.9657 + 11.0481*(d/(d+15))) ##Som pine
      ##BSTEM *0.042/0.52 
      rot1 <- exp(-3.3045 + 8.1184*(d/(d+11)) + 0.9783*(log(H))) *0.042/0.52
      ##BSTEM *0.042/0.52
      rot2 <- exp(-3.3045 + 8.1184*(d/(d+11)) + 0.9783*(log(H))) *0.042/0.52
      usoil <- (exp(((0.36266)^2) / 2 + 6.1708 + (d*10) / ((d*10) + 225) * 10.01111))/1000
      
      biomass.kg[i.found.this.period,][i.birc,] <-
        cbind(crown, dgr, stem, stub,  bark, usoil, rot1, rot2, foliage)
      
      ## since these equations produce results different from zero even
      ## when d and H are zero, we should correct it
      biomass.kg[i.found.this.period,][i.birc,][d == 0,] <- 0
    }
    
    names(biomass.kg) <- paste0(names(biomass.kg), ".kg")
  }
  return(biomass.kg)
}
