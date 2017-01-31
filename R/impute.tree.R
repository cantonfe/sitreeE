
impute.tree <- function (flateid,
                         tree.BA.m2,
                         SDI,
                         PBAL.m2.ha,
                         QMD.cm,
                         SQ.spru, 
                         SQ.pine,
                         SQ.harw,
                         pr.pine.ba,
                         pr.spru.ba,
                         spp, ...) 
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

    dt2 <- data.frame(flateid        = as.character(flateid),
                      BA.cm2.tr0     = tree.BA.m2*100*100,
                      SDI.tr0        = SDI,
                      PBAL.m2.ha.tr0 = PBAL.m2.ha,
                      QMD.cm.tr0     = QMD.cm, 
                      SQ.spruce      = SQ.spru,
                      SQ.pine        = SQ.pine,
                      SQ.harw        = SQ.harw,
                      pr.furu.ba.tr0 = pr.pine.ba,
                      pr.gran.ba.tr0 = pr.spru.ba, 
                      species        = spp,
                      stringsAsFactors = FALSE,
                      row.names      = paste0("new.", 1:length(flateid))
                      )
    dt2[, c("life.fin", "BA.cm2.inc", "distance")] <- NA
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
        dt2[i.species & j.na, c("life.fin", "BA.cm2.inc")] <-
            jj$yRefs[match(jj$neiIdsTrgs, 
                           rownames(jj$yRefs)), c("life.fin", "BA.cm2.inc")]
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
        dt2[i.species & j.na, c("life.fin", "BA.cm2.inc")] <- jj$yRefs[match(jj$neiIdsTrgs, 
            rownames(jj$yRefs)), c("life.fin", "BA.cm2.inc")]
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
        dt2[i.species & j.na, c("life.fin", "BA.cm2.inc")] <- jj$yRefs[match(jj$neiIdsTrgs, 
            rownames(jj$yRefs)), c("life.fin", "BA.cm2.inc")]
        dt2[i.species & j.na, "distance"] <- jj$neiDstTrgs
        j.na[i.species & j.na][!is.na(jj$neiIdsTrgs)] <- FALSE
    }
    return(dt2[, c("life.fin", "BA.cm2.inc", "distance")])
}

