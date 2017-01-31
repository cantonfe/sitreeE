hgt.Schmidt <- function (tr,
                         fl,
                         this.period,
                         i.period,
                         common.vars,
                         vars.required,
                         period.length,
                   ...) {

  others <- list(...)
  fn.height.1 <- others$fn.height.1
  ## They should return height in dm
  if (fn.height.1 == "S2013.hgt"){
    ## it returns height
    hgt <- with(common.vars,
                S2013.hgt(
                  QMD.cm,
                  alt.m           = fl$alt.m[i.stand],
                  PBAL.m2.ha      = PBAL.m2.ha,
                  plot.size.m2    = fl$plot.size.m2[i.stand],
                  soil.depth.1234 = fl$soil.depth.1234[i.stand],
                  utm.s33.ov.m    = fl$utm.s33.ov.m[i.stand],
                  utm.s33.sn.m    = fl$utm.s33.sn.m[i.stand],
                  xkti_Dg         = xkti_Dg, ## the same definition as xkti_Dg_22
                  spp             = spp,
                  model.spru      = get(others$model.spru),
                  model.pine      = get(others$model.pine),
                  model.birc      = get(others$model.birc)
                )
                )
  }
  
  ## fn = "hgt.plot.S2013"
  if (fn.height.1 == "hgt.S2013.plot"){
    hgt <- with (common.vars,
                 hgt.S2013.plot(
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
                   model.spru      = get(others$model.spru),
                   model.pine      = get(others$model.pine),
                   model.birc      = get(others$model.birc),
                   model.me.spru   = get(others$model.me.spru),
                   model.me.pine   = get(others$model.me.pine),
                   model.me.birc   = get(others$model.me.birc),
                   ranef           = pars.functions$S2013.plot.hgt$re
                 )
                 )
  }
  
  return(hgt)
}
## reassignInPackage("hgt.Schmidt", "skogsimExtra", hgt.Schmidt)
