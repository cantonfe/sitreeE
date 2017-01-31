## Paper 3 of Bollandås theisis page 10
B2007hgt <-
function (spp, SI.m, lat.deg, alt.m, height.dm, dbh.mm, SBA.m2.ha) 
{
    ##if(!exists("pars.functions")) data(pars.functions)
    (13      +
               pars.functions[["B2007hgt"]][spp, "beta1"] *
     SI.m    ^ pars.functions[["B2007hgt"]][spp, "beta2"] *
     lat.deg ^ pars.functions[["B2007hgt"]][spp, "beta3"] *
     alt.m   ^ pars.functions[["B2007hgt"]][spp, "beta4"] *
     exp(      pars.functions[["B2007hgt"]][spp, "beta5"] *
         dbh.mm   ^ pars.functions[["B2007hgt"]][spp, "beta6"] *
         SBA.m2.ha ^ pars.functions[["B2007hgt"]][spp, "beta7"])) - height.dm
    ## Bollandås writes BA but he means stand basal area per ha
}
