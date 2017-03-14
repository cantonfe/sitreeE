###########################
## biomass function      ## Marklund 1988
###########################
## For consistency with estimates in the Kyoto Protocol, the tree biomass is defined as the sum of above ground and below ground biomass.
## The above ground biomass of a tree is the sum of the estimates of the fractions
## stem, stump, bark, living branches, and dead branches.
## The below ground biomass is the estimate of the fraction
## stump and roots minus the estimate of the fraction stump

## I think the M1988 option uses the same functions as M1988.Rune
## but with potentially varying parameters and standard errors calculated?

biomass.M1988 <- function (fn = "M1988.Rune", tr, res, this.period) 
{
  i.spru <- res$spp == "spruce"
  i.pine <- res$spp == "pine"
  i.birc <- res$spp %in% c("birch", "other")
  
  ## dealing with potential dbh < 50 mm
  i.no.dbh <- tr$data[["dbh.mm"]][ , this.period] < 50
  
  i.spru[i.no.dbh] <- i.pine[i.no.dbh] <- i.birc[i.no.dbh] <- FALSE
  
  if (class(tr) == "trListDead") {
    if ('found.dead' %in% names(tr$last.measurement)) {
      i.found.this.period <- tr$last.measurement$found.dead == this.period
    } else {
      i.found.this.period <- tr$last.measurement$found.removed == this.period
    }
  } else {
    i.found.this.period <- rep(TRUE, length(tr$data$tree.sp))
  }
  i.spru <- tr$data$tree.sp[i.found.this.period] %in% c('1','2','3','21','29')
  i.pine   <- tr$data$tree.sp[i.found.this.period] %in% c('10','11','20')
  i.birc  <- !i.spru & ! i.pine
  
  if (sum (i.found.this.period ) != 0){
    ###########################
    ## M1988. Rune
    ###########################
    if (fn == 'M1988.Rune'){
      
      biomass.kg <- data.frame(living.branches = rep(NA,
                                                     length(i.found.this.period)),
                               dead.branches = NA,
                               stem.wood = NA,
                               stump = NA,
                               bark = NA,
                               stump.roots = NA,
                               roots1 = NA,
                               roots2 = NA,
                               bar = NA)
      
      ## SPRUCE
      if (sum(i.spru)> 0){
        if (class(tr) == "trListDead") {
          d <- tr$last.measurement[["dbh.mm"]] [i.found.this.period] [ i.spru]/10
          H <- tr$last.measurement[["height.dm"]][i.found.this.period] [ i.spru]/10
        } else {
          d <- tr$data[["dbh.mm"]]   [ i.spru, this.period]/10
          H <- tr$data[["height.dm"]][ i.spru, this.period]/10
        }
        dgr   <- exp(-4.6351 +  3.6518*(d/(d+18)) + 0.0493*H + 1.0129*(log(H)))  ## Marklund 1988 G20 s. 50 */
        kron  <- exp(-1.2063 + 10.9708*(d/(d+13)) - 0.0124*H - 0.4923*(log(H))) ## Marklund 1988 G12 s. 46  Inkluderer bar*/
        bar  <- exp(-1.8551 +  9.7809*(d/(d+12))-0.4873*(log(H)))        ## Marklund 1988 G16 s. 48 brukes ikke   */
        bark  <- exp(-3.4020 +  8.3089*(d/(d+15))+0.0147*H +0.2295*(log(H))) ## Marklund 1988 G8  s. 44 */
        stam  <- exp(-2.3032 +  7.2309*(d/(d+14))+0.0355*H +0.7030*(log(H))) ## Marklund 1988 G5  s. 42 */
        stub  <- exp(-3.3645 + 10.6686*(d/(d+17)))       ## Marklund 1988 G26 s. 54 */
        rot1  <- exp(-2.5706 +  7.6283*(d/(d+12)))       ## Marklund 1988 G31 s. 57 */
        rot2  <- exp(-6.3851 + 13.3703*(d/(d+8)))        ## Marklund 1988 G28 s. 55 */
        ujord  <- (exp(((0.32308)^2) / 2 + 4.58761 + (d*10) /
                       ((d*10) + 138) * 10.44035))/1000 ## Petterson og Sthål 2006 Root limit 2 mm */
        
        biomass.kg[i.found.this.period,][i.spru,] <-
          cbind(kron, dgr, stam, stub, bark, ujord, rot1, rot2, bar)
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
        
        dgr   <- exp(-5.8926 +  7.1270*(d/(d+10))-0.0465*H+1.1060*(log(H)))
        kron  <- exp(-2.5413 + 13.3955*(d/(d+10))-1.1955*(log(H)))
        bar   <- exp(-3.4781 + 12.1095*(d/(d+7)) +0.0413*H-1.5650*(log(H)))##Brukes ikke
        bark   <-  exp(-3.2765 + 7.2482*(d/(d+16)) + 0.4487*(log(H)))
        stam   <- exp(-2.6864 +  7.6066*(d/(d+14))+0.0200*H+0.8658*(log(H)))
        stub   <- exp(-3.9657 + 11.0481*(d/(d+15)))
        rot1   <- exp(-3.8375 + 8.8795*(d/(d+10)))                
        rot2   <- exp(-6.3413 + 13.2902*(d/(d+9)))
        ujord  <- (exp(((0.35449)^2)  / 2 + 3.44275 + (d*10) / ((d*10) + 113) * 11.06537))/1000## Petterson og Sthål 2006 Root limit 2 mm */
        biomass.kg[i.found.this.period,][i.pine,] <-
          cbind(kron, dgr, stam, stub, bark, ujord, rot1, rot2, bar)
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
        kron  <- exp(-3.3633 + 10.2806*(d/(d+10)))
        bar  <- exp(-3.3045 + 8.1184*(d/(d+11)) + 0.9783*(log(H))) * 0.011/0.52 ##BSTAM *0.11/0.52
        bark <- exp(-4.0778 + 8.3019*(d/(d+14)) + 0.7433*(log(H)))
        stam <- exp(-3.3045 + 8.1184*(d/(d+11)) + 0.9783*(log(H)))
        stub <- exp(-3.9657 + 11.0481*(d/(d+15))) ##Som furu
        rot1 <- exp(-3.3045 + 8.1184*(d/(d+11)) + 0.9783*(log(H))) *0.042/0.52 ##BSTAM *0.042/0.52                     
        rot2 <- exp(-3.3045 + 8.1184*(d/(d+11)) + 0.9783*(log(H))) *0.042/0.52 ##BSTAM *0.042/0.52
        ujord <- (exp(((0.36266)^2) / 2 + 6.1708 + (d*10) / ((d*10) + 225) * 10.01111))/1000
        
        biomass.kg[i.found.this.period,][i.birc,] <-
          cbind(kron, dgr, stam, stub, bark, ujord, rot1, rot2, bar)
        ## since these equations produce results different from zero even
        ## when d and H are zero, we should correct it
        biomass.kg[i.found.this.period,][i.birc,][d == 0,] <- 0
      }
      names(biomass.kg) <- paste0(names(biomass.kg), ".kg")
      
    } else {

      ##################
      ##M1988
      ##################
      if (fn == 'M1988'){
      biomass.kg <- data.frame(living.branches = rep(NA, length(res$spp)),
                               dead.branches = NA,
                               stem.wood = NA,
                               stump = NA,
                               bark = NA,
                               stump.roots = NA)
      ## SPRUCE
      biomass.kg[i.spru, ] <-
        with(pars.functions[["M1988"]],
             do.call(what = fn,
                     args = list(
                       dbh.cm    = tr$data[["dbh.mm"]]   [ i.spru, this.period]/10 
                     , height.m  = tr$data[["height.dm"]][ i.spru, this.period]/10
                       ## should the standard error be returned additionally
                     , standard.error = F
                       ## multiplication factor to increase or decrease se (affects
                       ## bias correction)
                     , se.factor = 1
                     , exp.transf = T ## if F, half the variance is subtracted
                       ## function parameters stem
                     , stem.wood.i   = spruce["stem.wood", "i"]
                     , stem.wood.d   = spruce["stem.wood", "d"]
                     , stem.wood.h   = spruce["stem.wood", "h"]
                     , stem.wood.lh  = spruce["stem.wood", "lh"]
                     , stem.wood.se  = spruce["stem.wood", "se"]
                     , stem.wood.add = spruce["stem.wood", "add"]
                       ## function parameters bark
                     , bark.i   = spruce["bark", "i"]
                     , bark.d   = spruce["bark", "d"]
                     , bark.h   = spruce["bark", "h"]
                     , bark.lh  = spruce["bark", "lh"]
                     , bark.se  = spruce["bark", "se"]
                     , bark.add = spruce["bark", "add"]
                       ## function parameters living branches
                     , living.branches.i   = spruce["living.branches", "i"]
                     , living.branches.d   = spruce["living.branches", "d"]
                     , living.branches.h   = spruce["living.branches", "h"]
                     , living.branches.lh  = spruce["living.branches", "lh"]
                     , living.branches.se  = spruce["living.branches", "se"]
                     , living.branches.add = spruce["living.branches", "add"]
                       ## function parameters dead branches
                     , dead.branches.i   = spruce["dead.branches", "i"]
                     , dead.branches.d   = spruce["dead.branches", "d"]
                     , dead.branches.h   = spruce["dead.branches", "h"]
                     , dead.branches.lh  = spruce["dead.branches", "lh"]
                     , dead.branches.se  = spruce["dead.branches", "se"]
                     , dead.branches.add = spruce["dead.branches", "add"]
                       ## function parameters stump
                     , stump.i   = spruce["stump", "i"]
                     , stump.d   = spruce["stump", "d"]
                     , stump.h   = spruce["stump", "h"]
                     , stump.lh  = spruce["stump", "lh"]
                     , stump.se  = spruce["stump", "se"]
                     , stump.add = spruce["stump", "add"]
                       ## function parameters stump and roots 
                     , stump.roots.i    = spruce["stump.roots", "i"]
                     , stump.roots.d    = spruce["stump.roots", "d"]
                     , stump.roots.h    = spruce["stump.roots", "h"]
                     , stump.roots.lh   = spruce["stump.roots", "lh"]
                     , stump.roots.bias = spruce["stump.roots", "bias"]
                     , stump.roots.se   = spruce["stump.roots", "se"]
                     , stump.roots.add  = spruce["stump.roots", "add"]
                       
                     )
                     )
             )
      
      ## PINE
      biomass.kg[i.pine, ] <-
        with(pars.functions[["M1988"]],
             do.call(what = fn,
                     args = list(
                       dbh.cm    = tr$data[["dbh.mm"]][ i.pine, this.period]/10 
                     , height.m  = tr$data[["height.dm"]][ i.pine, this.period]/10
                       ## should the standard error be returned additionally
                     , standard.error = F
                       ## multiplication factor to increase or decrease se (affects
                       ## bias correction)
                     , se.factor = 1
                     , exp.transf = T ## if F, half the variance is subtracted
                       ## function parameters stem
                     , stem.wood.i   = pine["stem.wood", "i"]
                     , stem.wood.d   = pine["stem.wood", "d"]
                     , stem.wood.h   = pine["stem.wood", "h"]
                     , stem.wood.lh  = pine["stem.wood", "lh"]
                     , stem.wood.se  = pine["stem.wood", "se"]
                     , stem.wood.add = pine["stem.wood", "add"]
                       ## function parameters bark
                     , bark.i   = pine["bark", "i"]
                     , bark.d   = pine["bark", "d"]
                     , bark.h   = pine["bark", "h"]
                     , bark.lh  = pine["bark", "lh"]
                     , bark.se  = pine["bark", "se"]
                     , bark.add = pine["bark", "add"]
                       ## function parameters living branches
                     , living.branches.i   = pine["living.branches", "i"]
                     , living.branches.d   = pine["living.branches", "d"]
                     , living.branches.h   = pine["living.branches", "h"]
                     , living.branches.lh  = pine["living.branches", "lh"]
                     , living.branches.se  = pine["living.branches", "se"]
                     , living.branches.add = pine["living.branches", "add"]
                       ## function parameters dead branches
                     , dead.branches.i   = pine["dead.branches", "i"]
                     , dead.branches.d   = pine["dead.branches", "d"]
                     , dead.branches.h   = pine["dead.branches", "h"]
                     , dead.branches.lh  = pine["dead.branches", "lh"]
                     , dead.branches.se  = pine["dead.branches", "se"]
                     , dead.branches.add = pine["dead.branches", "add"]
                       ## function parameters stump
                     , stump.i   = pine["stump", "i"]
                     , stump.d   = pine["stump", "d"]
                     , stump.h   = pine["stump", "h"]
                     , stump.lh  = pine["stump", "lh"]
                     , stump.se  = pine["stump", "se"]
                     , stump.add = pine["stump", "add"]
                       ## function parameters stump and roots 
                     , stump.roots.i    = pine["stump.roots", "i"]
                     , stump.roots.d    = pine["stump.roots", "d"]
                     , stump.roots.h    = pine["stump.roots", "h"]
                     , stump.roots.lh   = pine["stump.roots", "lh"]
                     , stump.roots.bias = pine["stump.roots", "bias"]
                     , stump.roots.se   = pine["stump.roots", "se"]
                     , stump.roots.add  = pine["stump.roots", "add"]
                       
                     )
                     )
             )
      ## BIRCH
      biomass.kg[i.birc, ] <-
        with(pars.functions[["M1988"]],
             do.call(what = fn,
                     args = list(
                       dbh.cm    = tr$data[["dbh.mm"]]   [ i.birc, this.period]/10 
                     , height.m  = tr$data[["height.dm"]][ i.birc, this.period]/10
                       ## should the standard error be returned additionally
                     , standard.error = F
                       ## multiplication factor to increase or decrease se (affects
                       ## bias correction)
                     , se.factor = 1
                     , exp.transf = T ## if F, half the variance is subtracted
                       ## function parameters stem
                     , stem.wood.i   = birch["stem.wood", "i"]
                     , stem.wood.d   = birch["stem.wood", "d"]
                     , stem.wood.h   = birch["stem.wood", "h"]
                     , stem.wood.lh  = birch["stem.wood", "lh"]
                     , stem.wood.se  = birch["stem.wood", "se"]
                     , stem.wood.add = birch["stem.wood", "add"]
                       ## function parameters bark
                     , bark.i   = birch["bark", "i"]
                     , bark.d   = birch["bark", "d"]
                     , bark.h   = birch["bark", "h"]
                     , bark.lh  = birch["bark", "lh"]
                     , bark.se  = birch["bark", "se"]
                     , bark.add = birch["bark", "add"]
                       ## function parameters living branches
                     , living.branches.i   = birch["living.branches", "i"]
                     , living.branches.d   = birch["living.branches", "d"]
                     , living.branches.h   = birch["living.branches", "h"]
                     , living.branches.lh  = birch["living.branches", "lh"]
                     , living.branches.se  = birch["living.branches", "se"]
                     , living.branches.add = birch["living.branches", "add"]
                       ## function parameters dead branches
                     , dead.branches.i   = birch["dead.branches", "i"]
                     , dead.branches.d   = birch["dead.branches", "d"]
                     , dead.branches.h   = birch["dead.branches", "h"]
                     , dead.branches.lh  = birch["dead.branches", "lh"]
                     , dead.branches.se  = birch["dead.branches", "se"]
                     , dead.branches.add = birch["dead.branches", "add"]
                       ## function parameters stump
                     , stump.i   = birch["stump", "i"]
                     , stump.d   = birch["stump", "d"]
                     , stump.h   = birch["stump", "h"]
                     , stump.lh  = birch["stump", "lh"]
                     , stump.se  = birch["stump", "se"]
                     , stump.add = birch["stump", "add"]
                       ## function parameters stump and roots 
                     , stump.roots.i    = birch["stump.roots", "i"]
                     , stump.roots.d    = birch["stump.roots", "d"]
                     , stump.roots.h    = birch["stump.roots", "h"]
                     , stump.roots.lh   = birch["stump.roots", "lh"]
                     , stump.roots.bias = birch["stump.roots", "bias"]
                     , stump.roots.se   = birch["stump.roots", "se"]
                     , stump.roots.add  = birch["stump.roots", "add"]
                       
                     )
                     )
             )
      names(biomass.kg) <- paste0(names(biomass.kg), ".kg")
      }
    }
  } else {## if no tree to calcualte biomass to
    biomass.kg <- rep(0, length(i.found.this.period))
  }
  
  return(biomass.kg)
}
## reassignInPackage("biomass.M1988", "sitreeE", biomass.M1988)
