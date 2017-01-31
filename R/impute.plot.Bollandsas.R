impute.plot.Bollandsas <-
  function (uID.stand,
            lat.deg,
            SI.m,
            SBA.m2.ha,
            tph,
            pr.spru.ba,
            pr.harw.ba
            ) 
{
  dt2 <- data.frame(uID.stand = as.character(uID.stand),
                    lat.deg          = lat.deg,
                    abonitet.tr0     = SI.m,
                    SBA.m2.ha.tr0    = SBA.m2.ha,
                    tph.tr0          = tph,
                    pr.gran.ba.tr0  = pr.spru.ba    ,
                    pr.lauv.ba.tr0  = pr.harw.ba,
                    stringsAsFactors = FALSE,
                    row.names        = paste0("new.", 1:length(uID.stand))
                    )

  ##print(table(dt2$tph.tr0, useNA = 'always'))
  i <- is.na(dt2$tph.tr0) ## if nnhthere is no trees it should
  ## be zero
  dt2$tph.tr0[ i] <- 0
  dt2$pr.gran.ba.tr0[i ] <- 0
  dt2$pr.lauv.ba.tr0[i] <- 0
  refGroups <- as.character(mah.plot$yRefs$y)
  
  ## neccessary in case there are more (or different) plots in the new data 
  my.levels <- unique(c(refGroups, dt2$uID.stand))
  dt2$uID.stand <- factor(dt2$uID.stand, levels = my.levels)
  refGroups     <- factor(refGroups    , levels = my.levels)


  dt2[, c("new.uID.stand")] <- NA
  m50 <- newtargets(mah.plot, newdata = dt2, k = 3)

  jj <- applyMask(m50,
                  refGroups = refGroups,
                  trgGroups = dt2$uID.stand, 
                  method = "removeWhenCommon",
                  k = 1)  ## number of nearest neighbors to keep


  
  if (any(is.na(jj$neiIdsTrgs))) 
    print("K is not large enough")
  

  dt2[   , c("new.uID.stand")] <- jj$yRefs[match(jj$neiIdsTrgs, rownames(jj$yRefs)), ]

  

  dt2$ neiDstTrgs <- jj$neiDstTrgs
  sa <- dt.ingrowth[match(dt2[, c("new.uID.stand")], names(dt.ingrowth))]
  
  names(sa) <- dt2[, c("uID.stand")]
  
  attr(sa, "distances") <- dt2$neiDstTrgs

 
  
    return(sa)
}

## reassignInPackage("impute.plot.Bollandsas", "skogsimExtra", impute.plot.Bollandsas)
