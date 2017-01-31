B2007 <- function (
    spp           = spp,
    dbh.mm        = dbh.mm,
    SBA.m2.ha     = SBA.m2.ha,
    ...) 
{ ##(spp, dbh.mm, SBA.m2.ha) 

    if (!all(unique(spp) %in% c("spruce", "pine", "birch", "other"))) {
        message("spp should only contain values spruce, pine, birch, other")
    }
    logit <- 
                  pars.functions[["B2007"]][spp, "a0"] +
                  pars.functions[["B2007"]][spp, "a1"] * dbh.mm +
                  pars.functions[["B2007"]][spp, "a2"] * 
                  1e-05 * (dbh.mm)^2 + pars.functions[["B2007"]][spp, "a3"] * 
                  SBA.m2.ha
             
    return(1/(1 + exp(-logit)))
}
## reassignInPackage("B2007", "skogsim", B2007)
