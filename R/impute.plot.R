impute.plot <-
function (uID.stand,
          SDI,
          QMD.cm,
          SQ.spru,
          SQ.pine,
          SQ.harw, 
          pr.pine.ba,
          pr.spru.ba,...) 
{
    
    dt2 <- data.frame(uID.stand = as.character(uID.stand),
                      SDI.tr0 = SDI,
                      QMD.cm.tr0 = QMD.cm,
                      SQ.spruce = SQ.spru, 
                      SQ.pine,
                      SQ.harw,
                      pr.furu.ba.tr0 = pr.pine.ba,
                      pr.gran.ba.tr0 = pr.spru.ba, 
                      row.names = paste0("new.", 1:length(uID.stand)),
                      stringsAsFactors = FALSE
                      )
    refGroups <- as.character(mah.plot$yRefs$y)
    ## neccessary in case there are more (or different) plots in the new data 
    my.levels <- unique(c(refGroups, dt2$uID.stand))
    dt2$uID.stand <- factor(dt2$uID.stand, levels = my.levels)
    refGroups     <- factor(refGroups    , levels = my.levels)
    dt2[, c("new.uID.stand")] <- NA
    m50 <- newtargets(mah.plot, newdata = dt2, k = 3)
    jj <- applyMask(m50, refGroups = refGroups, trgGroups = dt2$uID.stand, 
        method = "removeWhenCommon", k = 1)
    if (any(is.na(jj$neiIdsTrgs))) 
        print("K is not large enough")
    dt2[   , c("new.uID.stand")] <- jj$yRefs[match(jj$neiIdsTrgs, rownames(jj$yRefs)), ]
    dt2$ neiDstTrgs <- jj$neiDstTrgs
    sa <- dt.ingrowth[match(dt2[, c("new.uID.stand")], names(dt.ingrowth))]
    names(sa) <- dt2[, c("uID.stand")]
    attr(sa, "distances") <- dt2$neiDstTrgs
    return(sa)
}
## reassignInPackage("impute.plot", "skogsim", impute.plot)
