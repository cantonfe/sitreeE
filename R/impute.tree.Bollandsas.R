impute.tree.Bollandsas <- function (flateid,
                                    dbh.mm,
                                    lat.deg,
                                    PBAL.m2.ha,
                                    SI.m,
                                    SBA.m2.ha,
                                    spp) 
{
    ## Remove any tree in mah.spruce, mah.pine and mah.harw that have "removed" trees
    for (my.model.name in c("mah.spruce", "mah.pine", "mah.harw")){
        my.model <- get(my.model.name)
        i.removed <- with(my.model, yRefs$life.fin == "removed")
        my.model$yRefs <- my.model$yRefs[!i.removed, ]
        my.model$xRefs <- my.model$xRefs[!i.removed, ]
        my.model$xall  <- my.model$xall [!i.removed, ]
        my.model$neiDstRefs <- my.model$neiDstRefs[!i.removed ]
        my.model$neiIdsRefs <- my.model$neiIdsRefs[!i.removed ]
        assign(my.model.name, my.model)
    }

    dt2 <- data.frame(flateid          = as.character(flateid),
                      dbh.mm.tr0       = dbh.mm,
                      lat.deg          = lat.deg,
                      PBAL.m2.ha.tr0   = PBAL.m2.ha,
                      abonitet.tr0     = SI.m,
                      SBA.m2.ha.tr0    = SBA.m2.ha,
                      species          = spp,
                      stringsAsFactors = FALSE,
                      row.names        = paste0("new.", 1:length(flateid))
                      )
    dt2[, c("life.fin", "dbh.inc.mm", "distance")] <- NA
    dt2[, c("life.fin")] <- factor(NA, levels = levels(mah.spruce$yRefs$life.fin))
    j.na <- rep(TRUE, length(flateid))
    i.species <- dt2$species == "spruce"
    k.k <- 0
    
    refGroups <-          as.character(mah.spruce$yRefs$flateid)
    my.levels <- unique(c(as.character(mah.spruce$yRefs$flateid),
                          as.character(mah.pine$yRefs$flateid),
                          as.character(mah.harw$yRefs$flateid),
                          dt2$flateid))
    dt2$flateid <- factor(dt2$flateid, levels = my.levels)
    
    refGroups   <- factor(as.character(mah.spruce$yRefs$flateid), levels = my.levels)
    dt2$flateid <- factor(dt2$flateid, levels = my.levels)
   
    while (sum(j.na[i.species]) != 0) {
        k.k <- k.k + 10
        m50 <- newtargets(mah.spruce,
                          newdata = dt2[i.species & j.na, ], k = k.k)
        jj <- applyMask(m50,
                        refGroups = refGroups,
                        trgGroups = dt2$flateid[i.species & j.na],
                        method    = "removeWhenCommon",
                        k         = 1)
        ##if (any(is.na(jj$neiIdsTrgs))) print("K is not large enough")
        dt2[i.species & j.na, c("life.fin", "dbh.inc.mm")] <-
            jj$yRefs[match(jj$neiIdsTrgs, 
                           rownames(jj$yRefs)), c("life.fin", "dbh.inc.mm")]
        dt2[i.species & j.na, "distance"] <- jj$neiDstTrgs
        j.na[i.species & j.na][!is.na(jj$neiIdsTrgs)] <- FALSE
    }
    i.species <- dt2$species == "pine"
    k.k <- 0
    refGroups <- factor(as.character(mah.pine$yRefs$flateid), levels = my.levels)
    while (sum(j.na[i.species]) != 0) {
        k.k <- k.k + 10
        m50 <- newtargets(mah.pine, newdata = dt2[i.species & 
            j.na, ], k = k.k)
        jj <- applyMask(m50, refGroups = refGroups, trgGroups = dt2$flateid[i.species & 
            j.na], method = "removeWhenCommon", k = 1)
        ##if (any(is.na(jj$neiIdsTrgs))) print("K is not large enough")
        dt2[i.species & j.na, c("life.fin", "dbh.inc.mm")] <- jj$yRefs[match(jj$neiIdsTrgs, 
            rownames(jj$yRefs)), c("life.fin", "dbh.inc.mm")]
        dt2[i.species & j.na, "distance"] <- jj$neiDstTrgs
        j.na[i.species & j.na][!is.na(jj$neiIdsTrgs)] <- FALSE
    }
    i.species <- dt2$species %in% c( "birch", "other")
    k.k <- 0
    refGroups <- factor(as.character(mah.harw$yRefs$flateid), levels = my.levels)
    while (sum(j.na[i.species]) != 0) {
        k.k <- k.k + 10
        m50 <- newtargets(mah.harw, newdata = dt2[i.species & 
            j.na, ], k = k.k)
        jj <- applyMask(m50, refGroups = refGroups, trgGroups = dt2$flateid[i.species & 
            j.na], method = "removeWhenCommon", k = 1)
        ##if (any(is.na(jj$neiIdsTrgs))) print("K is not large enough")
        dt2[i.species & j.na, c("life.fin", "dbh.inc.mm")] <- jj$yRefs[match(jj$neiIdsTrgs, 
            rownames(jj$yRefs)), c("life.fin", "dbh.inc.mm")]
        dt2[i.species & j.na, "distance"] <- jj$neiDstTrgs
        j.na[i.species & j.na][!is.na(jj$neiIdsTrgs)] <- FALSE
    }
    return(dt2[, c("life.fin", "dbh.inc.mm", "distance")])
}

