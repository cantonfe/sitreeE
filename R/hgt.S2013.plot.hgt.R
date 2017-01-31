
## As far as I can see in his code he only picks mineral soils, he removes jorddybe = NA
## But since we need to forecast for all plots we have to choose a jorddybe.
hgt.S2013.plot <-
    function (
        uplotID.tr      = uplotID.tr,
        QMD.cm          = QMD.cm,
        alt.m           = fl$alt.m[i.stand],
        PBAL.m2.ha      = PBAL.m2.ha,
        plot.size.m2    = fl$plot.size.m2[i.stand],
        soil.depth.1234 = fl$soil.depth.1234[i.stand],
        utm.s33.ov.m    = fl$utm.s33.ov.m[i.stand],
        utm.s33.sn.m    = fl$utm.s33.sn.m[i.stand],
        xkti_Dg         = xkti_Dg, ## the same definition as xkti_Dg_22
        spp             = spp,
        model.spru      = fn.growth.pars$model.spru,
        model.pine      = fn.growth.pars$model.pine,
        model.birc      = fn.growth.pars$model.birc,
        model.me.spru   = fn.growth.pars$model.me.spru,
        model.me.pine   = fn.growth.pars$model.me.pine,
        model.me.birc   = fn.growth.pars$model.me.birc,
        ranef           = pars.functions$S2013.plot.hgt$re
        )
{

    height <- rep(NA, length(QMD.cm))
    i.spru <- spp == "spruce"
    i.pine <- spp == "pine"
    i.birc <- spp %in% c("birch", "other")
    
    pred.nore.m <- S2013.hgt (
        QMD.cm          = QMD.cm,
        alt.m           = alt.m,
        PBAL.m2.ha      = PBAL.m2.ha,
        plot.size.m2    = plot.size.m2,
        soil.depth.1234 = soil.depth.1234,
        utm.s33.ov.m    = utm.s33.ov.m,
        utm.s33.sn.m    = utm.s33.sn.m,
        xkti_Dg         = xkti_Dg, ## the same definition as xkti_Dg_22
        spp             = spp,
        model.spru      = model.spru,
        model.pine      = model.pine,
        model.birc      = model.birc,
        type            = "link"
        )/10  ## dm to m
    
  i.exist.all <- rep(FALSE, length(uplotID.tr))
  i.exist.all[i.spru] <- uplotID.tr[i.spru] %in% as.character(ranef$spru$uplotID)
  i.exist.all[i.pine] <- uplotID.tr[i.pine] %in% as.character(ranef$pine$uplotID)
  i.exist.all[i.birc] <- uplotID.tr[i.birc] %in% as.character(ranef$birc$uplotID)


    if(sum(!i.exist.all) > 0) {
        ## Predictions without the plot specific random effects
        pred.all <- S2013.hgt (
            QMD.cm          = QMD.cm,
            alt.m           = alt.m,
            PBAL.m2.ha      = PBAL.m2.ha,
            plot.size.m2    = plot.size.m2,
            soil.depth.1234 = soil.depth.1234,
            utm.s33.ov.m    = utm.s33.ov.m,
            utm.s33.sn.m    = utm.s33.sn.m,
            xkti_Dg         = xkti_Dg, ## the same definition as xkti_Dg_22
            spp             = spp,
            model.spru      = model.spru,
            model.pine      = model.pine,
            model.birc      = model.birc,
            type            = "response"
            )/10  ## dm to m
    }
    
    ## prediction with the gam/scam without calibration (without predicting ranefs):
    ## The model gives height in m, and we need height in cm
    
    ## SPRUCE
    height[i.spru] <- scale(pred.nore.m[i.spru], scale = FALSE)    
    ## PINE
    height[i.pine] <- scale(pred.nore.m[i.pine], scale = FALSE)
    ## BIRCH
    height[i.birc] <- scale(pred.nore.m[i.birc], scale = FALSE)
    
    ## SPRUCE
    ## which plots do we have random effects for
    i.exist <- uplotID.tr[i.spru] %in% ranef$spru$uplotID
    ## Where to find the random effects
    i.match <- match(uplotID.tr[i.spru][i.exist], ranef$spru$uplotID)
    ## and predict by plot
    height[i.spru][i.exist] <- exp( fixef(model.me.spru$lme)[1]
                                   + ranef$spru$ranef.1[i.match] +
                                   (
                                       fixef(model.me.spru$lme)[2] +
                                       ranef$spru$ranef.2[i.match]
                                       ) * height[i.spru][i.exist] ##centered.predictions
                                   )
    
    ## real.hgt <- tr$data[["height.dm"]][, this.period][i.spru]/10
    ## head(real.hgt)
    ## head(height.gran[i.spru])
    ## head(hgt.gran.me)
    ## var(height.gran[i.spru]  - real.hgt)
    ## var(hgt.gran.me          - real.hgt)
    ## mean(height.gran[i.spru] - real.hgt)
    ## mean(hgt.gran.me         - real.hgt)
        
    ## if no random effect we use the no random effects predictions
    if(sum(!i.exist.all) > 0) height[i.spru][!i.exist] <- pred.all[i.spru][!i.exist]
    
    ## PINE
    ## which plots do we have random effects for
    i.exist <- uplotID.tr[i.pine] %in% ranef$pine$uplotID
    ## Where to find the random effects
    i.match <- match(uplotID.tr[i.pine][i.exist], ranef$pine$uplotID)
    ## and predict by plot
    height[i.pine][i.exist] <- exp( fixef(model.me.pine$lme)[1]
                                   + ranef$pine$ranef.1[i.match] +
                                   (fixef(model.me.pine$lme)[2] +
                                    ranef$pine$ranef.2[i.match] ) *
                                   height[i.pine][i.exist] ##centered.predictions
                                   )
    ## if no random effect we use the no random effects predictions
    if(sum(!i.exist.all) > 0) height[i.pine][!i.exist] <- pred.all[i.pine][!i.exist]
        
    ## BIRC
    ## which plots do we have random effects for
    i.exist <- uplotID.tr[i.birc] %in% ranef$birc$uplotID
    ## Where to find the random effects
    i.match <- match(uplotID.tr[i.birc][i.exist], ranef$birc$uplotID)
    ## and predict by plot
    height[i.birc][ i.exist] <- exp( fixef(model.me.birc$lme)[1]
                                   + ranef$birc$ranef.1[i.match] +
                                   (fixef(model.me.birc$lme)[2] +
                                    ranef$birc$ranef.2[i.match] ) *
                                   height[i.birc][i.exist] ##centered.predictions
                                   )
    ## if no random effect we use the no random effects predictions
    if(sum(!i.exist.all) > 0) height[i.birc][!i.exist] <- pred.all[i.birc][!i.exist]


### for spruce:
### fitJuha_scamm_511_1level_n2_c_dia
### for pine:
### fitJuha_scamm_711_1level_n2_c_diag
### and for birch:
### fitJuha_scamm_411_1level_n2_c_diag
    
    return(height*10 )
}


## reassignInPackage("hgt.S2013.plot", "skogsimExtra", hgt.S2013.plot)
