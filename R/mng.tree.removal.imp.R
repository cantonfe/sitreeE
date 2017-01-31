

## ustandID     = tr$data$ustandID
## tree.BA.m2  = common.vars$tree.BA.m2
## SDI         = common.vars$SDI
## PBAL.m2.ha  = common.vars$PBAL.m2.ha
## QMD.cm      = common.vars$QMD.cm
## SQ.spru     = common.vars$SQ.fl$SQ.spru[common.vars$i.stand] 
## SQ.pine     = common.vars$SQ.fl$SQ.pine[common.vars$i.stand]
## SQ.harw     = common.vars$SQ.fl$SQ.harw[common.vars$i.stand] 
## pr.pine.ba  = common.vars$pr.spp.ba$pine[common.vars$i.stand]
## pr.spru.ba  = common.vars$pr.spp.ba$spru[common.vars$i.stand] 
## spp         = common.vars$spp
## ustandID.harv = fl$ustandID[management$final.felling]
## ustandID.thin = fl$ustandID[management$thinning]

## Impute using SQ
tree.removal.imp <- function(
    ustandID,
    tree.BA.m2,
    SDI,
    PBAL.m2.ha,
    QMD.cm,
    SQ.spru, 
    SQ.pine, SQ.harw,
    pr.pine.ba,
    pr.spru.ba,
    spp,
    ustandID.harv,
    ustandID.thin
    ){
    ## management list  final.felling thinning
    ## (load("/media/N/30-I/35/346030-ClimPol/WP1/Imputation/Data.R/Imputation/ImputationModelsManagement.RData"))
    trees.in.harvested.stands <- ustandID %in% ustandID.harv
    trees.in.thinned.stands   <- ustandID %in% ustandID.thin

    dt.harv <- data.frame(ustandID   = ustandID,
                          BA.cm2.tr0 = tree.BA.m2/100/100,
                          SDI.tr0    = SDI,
                          PBAL.m2.ha.tr0 = PBAL.m2.ha,
                          QMD.cm.tr0 = QMD.cm, 
                          SQ.spruce  = SQ.spru,
                          SQ.pine    = SQ.pine,
                          SQ.harw    = SQ.harw,
                          pr.furu.ba.tr0 = pr.pine.ba,
                          pr.gran.ba.tr0 = pr.spru.ba, 
                          species    = spp,
                          stringsAsFactors = FALSE,
                          row.names  = paste0("new.", 1:length(ustandID))
                          )
    dt.harv$species [dt.harv$species %in% c("other", "birch")] <- "birch"
    dt.harv[, c("life.fin")] <- factor(NA, levels = levels(mah.ff.harw$yRefs$life.fin))
    
    ## In this case it is not neccessary to use a mask
    
    ####################
    ## FINAL FELLING
    ####################
    ## SPRUCE

    for (my.species in c("spruce", "pine", "birch")){
        
        i.species <- dt.harv$species == my.species
        i.sp.harv <- i.species & trees.in.harvested.stands
        i.sp.thin <- i.species & trees.in.thinned.stands
        
        if (my.species == "birch") my.species <- "harw"

        if (sum(i.sp.harv) > 0){
            m50 <- newtargets(get(paste0( "mah.ff.", my.species)),
                              newdata = dt.harv[i.sp.harv, ], k = 1
                              )
            dt.harv$life.fin[i.sp.harv] <- impute(m50, vars= "life.fin")$life.fin
        }

        if (sum(i.sp.thin) > 0){
            m50 <- newtargets(get(paste0("mah.th.", my.species)),
                              newdata = dt.harv[i.sp.thin, ], k = 1
                              )
            dt.harv$life.fin[i.sp.thin] <-  impute(m50, vars= "life.fin")$life.fin
        }
    }

    ## table(dt.harv$life.fin    [trees.in.harvested.stands], useNA = "always")
    ## table(dt.harv$life.fin    [trees.in.thinned.stands], useNA = "always")
    return(dt.harv$life.fin)
}
