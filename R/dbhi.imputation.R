dbhi.imputation <- function (
    common.vars           = common.vars,
    this.period           = this.period,
    tr                    = tr,
    fl                    = fl,
    fn.growth.pars        = fn.growth.pars,
    ...) 
{

    ##require(yaImpute)
    fn.impute                  <- fn.growth.pars$fn.impute
    fn.impute.alternative      <- fn.growth.pars$fn.impute.alternative
    fn.impute.alternative.mort <- fn.growth.pars$fn.impute.alternative.mort
    
    if (fn.impute == "impute.tree"){
        imp <- with(common.vars,
                    impute.tree(
                        flateid     = uplotID.tr,
                        tree.BA.m2  = tree.BA.m2,
                        SDI         = SDI,
                        PBAL.m2.ha  = PBAL.m2.ha,
                        QMD.cm      = QMD.cm,
                        SQ.spru     = SQ.fl$SQ.spru[i.stand], 
                        SQ.pine     = SQ.fl$SQ.pine[i.stand],
                        SQ.harw     = SQ.fl$SQ.harw[i.stand], 
                        pr.pine.ba  = pr.spp.ba$pine[i.stand],
                        pr.spru.ba  = pr.spp.ba$spru[i.stand], 
                        spp         = spp, ...
                        )
                    )
    }

    if (fn.impute == "impute.tree.Bollandsas"){
        imp <- with(common.vars,
                    impute.tree.Bollandsas(
                        flateid     = uplotID.tr,
                        dbh.mm      = tr$data$dbh.mm[,this.period],
                        lat.deg     = fl$lat.deg[i.stand],
                        PBAL.m2.ha  = PBAL.m2.ha,
                        SI.m        = fl$SI.m[i.stand],
                        SBA.m2.ha   = SBA.m2.ha,
                        spp         = spp
                        )
                    )
    }
    
    ## if distance large, use other approach
    i.far <- imp$distance > quantile(c(mah.spruce$neiDstRefs,
                                       mah.pine$neiDstRefs,
                                       mah.harw$neiDstRefs), 0.99)
    print(paste0("i.far ",sum(i.far)))
    ## 5-years dbh increment BN2009 (spp, dbh.mm, DQ, SI.m, SBA.m2.ha, lat.deg)
    if (fn.impute.alternative == "BN2009"){
        dbh.inc.mm.alt <- with(common.vars, BN2009(
            spp    = spp[i.far],
            dbh.mm = tr$data$dbh.mm[,this.period][i.far],
            DQ     = (tr$data$dbh.mm[,this.period] /
                      (QMD.cm*10))[i.far],
            SI.m   = fl$SI.m[common.vars$i.stand][i.far],
            SBA.m2.ha = SBA.m2.ha[i.far],
            lat.deg = fl$lat.deg[i.stand][i.far]           
            )
                               )
    }
    
    ## Use tables calculated from data
    if (fn.impute.alternative == "dbh.inc.mort.fun"){
        ## SI 26 should be interpreted as 23
        SI.m <- fl$SI.m[common.vars$i.stand][i.far]
        SI.m[SI.m > 23] <- 23
        table.res <- with(common.vars,
                          dbh.inc.mort.fun(
                              dbh.mm = tr$data$dbh.mm[,this.period][i.far],
                              SI.m   = SI.m,
                              spp    = spp[i.far],
                              PBAL.m2.ha = PBAL.m2.ha[i.far],...
                              )
                          )
        dbh.inc.mm.alt <- unlist(table.res$dbh.inc.mm)
        mort.alt <- table.res$mort
        
    }
    
    ## 5-years dbh increment BN2009 (spp, dbh.mm, DQ, SI.m, SBA.m2.ha, lat.deg)
    if (fn.impute.alternative.mort == "B2007"){
        mort.alt <- with(common.vars, 
                         ifelse(B2007(
                             spp       = spp[i.far],
                             dbh.mm    =
                             tr$data[["dbh.mm"]][, this.period][i.far],
                             SBA.m2.ha = SBA.m2.ha[i.far]) >=
                                runif(length(tr$data[["ustandID"]][i.far]), 
                                      0, 1), "dead", "alive")
                         )
    }
    
    if (fn.impute == "impute.tree"){ ## If imputing BA.inc
        ## convert BA.inc in dbh.inc.mm
        BA.cm2       <- common.vars$tree.BA.m2*100*100
        BA.final.cm2 <- BA.cm2 + imp$BA.cm2.inc
        ## make sure there are no weird numbers
        BA.final.cm2[BA.final.cm2 < min(BA.cm2)] <- min(BA.cm2)
        dbh.final.mm <- sqrt(BA.final.cm2*10*10/pi)*2
        dbh.inc.mm   <- dbh.final.mm - tr$data[["dbh.mm"]][, this.period]
    } else {
        dbh.inc.mm <- imp$dbh.inc.mm
    }
    
    dbh.inc.mm[i.far]   <- dbh.inc.mm.alt
    imp$life.fin[i.far] <- mort.alt
    mort.imputation.result <<- imp$life.fin
    
    if (fn.growth.pars$only.mort) {
        return(imp$life.fin)
    } else {
        return(round(dbh.inc.mm))
    }
} 
## reassignInPackage("dbhi.imputation", "skogsim", dbhi.imputation)
