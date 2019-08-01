

biomass.norway.sitree <- function(dbh.mm, height.dm, tree.sp){
## just to make cran happy
living.branches <- dead.branches <- stem.wood <- stump <- 
bark <- usoil <- rot1 <- rot2 <- foliage <- biomass.aboveground.kg <- biomass.belowground.kg <- biomass.total.kg <- NULL


  class.tree.sp <- sp.classification(tree.sp,
                                     species.spruce = c(1,2,3,21, 29),
                                     species.pine = c(10, 11, 20),
                                     species.harw = c(30, 31))
  i.spru <- class.tree.sp == "spruce" & dbh.mm >0
  i.pine <- class.tree.sp == "pine" & dbh.mm >0
  i.birc <- class.tree.sp %in% c("birch", "other")& dbh.mm >0

  
  ## SPRUCE
  
  d <- dbh.mm/10
  H <- height.dm/10

  ## premade frame
  biomass <- setNames(data.table(matrix(numeric(),
                                        nrow = length(dbh.mm), ncol = 12)),
           c("living.branches", "dead.branches", "stem.wood"  ,
             "stump", "bark", "usoil",
             "rot1", "rot2", "foliage",            
             "biomass.aboveground.kg", "biomass.belowground.kg",
             "total.biomass.kg"))
  
  biomass[i.spru, names(biomass) :=
                    biomass.spruce.M1988(dbh.cm = d[i.spru], H.m = H[i.spru])
          ]
  biomass[i.pine, names(biomass) :=
                    biomass.pine.M1988(dbh.cm = d[i.pine], H.m = H[i.pine])
          ]
  biomass.birc  <-
    data.table(biomass.birch.S2014(dbh.cm = d[i.birc], H.m = H[i.birc]))
  
  biomass[i.birc, names(biomass) :=
                     biomass.birc[, list(living.branches,
                                         dead.branches, stem.wood  ,
                                         stump, bark, usoil,
                                         rot1, rot2, foliage,            
                                         biomass.aboveground.kg,
                                         biomass.belowground.kg,
                                         biomass.total.kg)]]
  ## since these equations produce results different from zero even
  ## when d and H are zero, we should correct it

  return(biomass)
}
