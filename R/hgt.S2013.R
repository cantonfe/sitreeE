## This functions uses the models produced by Matthias Schmidt while working
## on SoL the summer of 2013

## There are two versions of the models
## Using plot specific calibration     S2013.plot.hgt
## Not using plot specific calibration S2013.hgt

## This function should return height.inc
## Without plot calibration
S2013.hgt <-
    function (
        QMD.cm,
        alt.m           = fl$alt.m[i.stand],
        PBAL.m2.ha,
        plot.size.m2    = fl$plot.size.m2[i.stand],
        soil.depth.1234 = fl$soil.depth.1234[i.stand],
        utm.s33.ov.m = fl$utm.s33.ov.m[i.stand],
        utm.s33.sn.m = fl$utm.s33.sn.m[i.stand],
        xkti_Dg      = xkti_Dg, ## the same definition as xkti_Dg_22
        spp          = spp,
        model.spru   = get(fn.growth.pars$model.spru),
        model.pine   = get(fn.growth.pars$model.pine),
        model.birc   = get(fn.growth.pars$model.birc),
        type = "response"
        ) 
{
    height <- rep(NA, length(QMD.cm))
    i.spru <- spp == "spruce"
    i.pine <- spp == "pine"
    i.birc <- spp %in% c("birch", "other")
    
    ## if no data in jorddybde we assume soil.depth.1234 = 4, since
    ## in all cases in NFI is jorddybe = NA is because it is torv (organic soil)
    ## and because 4 is the most common
    ## But we need to consider that the models were done ignoring organic soils removed

    newdata <- data.frame(
        Dg          = QMD.cm, ##5 64
        HOH         = alt.m,     ## 2 1065
        ## Bal is in m2.plot not in m2.ha
        Bal_weights = PBAL.m2.ha * plot.size.m2 / 10000,#0 2.25
        JORDDYBDE   = soil.depth.1234, ## should be a factor with 4 levels
        UTM_S33_OV  = utm.s33.ov.m,##-55269 755484
        UTM_S33_SN  = utm.s33.sn.m ,##6456716 7743530
        xkti_Dg_22  = xkti_Dg,##-1.6875 0.4083
        xkti_Dg     = xkti_Dg
        )
    newdata$JORDDYBDE[!is.finite(newdata$JORDDYBDE)] <- 4
    
    ## prediction with the gam/scam without calibration (without predicting ranefs):
    ## The model gives height in m, and we need height in cm
    
    ## SPRUCE
    if (sum(i.spru) > 0){
        system.time({
        height[i.spru] <- predict(object = model.spru,
                                  newdata = newdata[i.spru,], block.size = -1,
                              type    = type)
    })
    }
    ## PINE
    if (sum(i.pine) > 0){
        height[i.pine] <- predict(model.pine,
                                  newdata = newdata[i.pine,], block.size = -1,
                                  type    = type)
    }
    ## BIRCH
    if (sum(i.birc) > 0){
        height[i.birc] <- predict(model.birc,
                                  newdata = newdata[i.birc,], block.size = -1,
                                  type    = type)
    }
    ## It returns height
    return(height*10)
}

## predict(model.spru,
##         newdata =
##         data.frame(
##             Dg          = 45.83,
##             HOH         = 360,     ## 2 1065
##             ## Bal is in m2.plot not in m2.ha
##             Bal_weights = 112 * 250 / 10000,#0 2.25
##             JORDDYBDE   = 2, ## should be a factor with 4 levels
##             UTM_S33_OV  = 266730,
##             UTM_S33_SN  = 6811735,
##             xkti_Dg_22  = 0.31 ,
##             xkti_Dg     = 0.31  ) ,
##         type    = "response")
