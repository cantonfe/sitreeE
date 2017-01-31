

## ustandID     = tr$data$ustandID
## tree.BA.m2  = tree.BA.m2
## SDI         = SDI
## PBAL.m2.ha  = PBAL.m2.ha
## QMD.cm      = QMD.cm
## SQ.spru     = SQ.fl$SQ.spru[i.stand] 
## SQ.pine     = SQ.fl$SQ.pine[i.stand]
## SQ.harw     = SQ.fl$SQ.harw[i.stand] 
## pr.pine.ba  = pr.spp.ba$pine[i.stand]
## pr.spru.ba  = pr.spp.ba$spru[i.stand] 
## spp         = spp
## ustandID.harv = fl$ustandID[management$final.felling]
## ustandID.thin = fl$ustandID[management$thinning]

## Impute using Bollandsas variables -SI instead of SQ

tree.removal.imp.Bollandsas <- function(
    ustandID,
    dbh.mm,
    lat.deg,
    PBAL.m2.ha,
    abonitet.tre,
    SBA.m2.ha,
    spp,
    ustandID.harv,
    ustandID.thin
    ){
    ## management list  final.felling thinning
    ## (load("/media/B/30-I/35/346030-ClimPol/WP1/Imputation/Data.R/Imputation/ImputationModelsManagement.RData"))
    trees.in.harvested.stands <- ustandID %in% ustandID.harv
    trees.in.thinned.stands   <- ustandID %in% ustandID.thin

    dt.harv <- data.frame(ustandID   = ustandID,
                          dbh.mm.tr0 = dbh.mm,
                          lat.deg    = lat.deg,
                          PBAL.m2.ha.tr0 = PBAL.m2.ha,
                          abonitet.tr0 = abonitet.tre,
                          SBA.m2.ha.tr0 = SBA.m2.ha,
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
