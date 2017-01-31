soilquality.calc <- function(veg.type.A, soil.depth.1234){
  ## Poorest
  soilquality <- rep(NA, length(soil.depth.1234))
  soilquality[
  (   veg.type.A %in% c("PoorWet","PoorDry","PoorMedium"))
  | (veg.type.A =="MediumWet" & soil.depth.1234 %in% c(1,2))
  | (veg.type.A =="MediumDry" & soil.depth.1234 %in% c(1))
  ]   <- "Poorest"
  
  ## Average
  soilquality[
  (  veg.type.A =="MediumWet" &
     soil.depth.1234 %in% c(3:4))
  | (veg.type.A =="MediumDry" &
     soil.depth.1234 %in% c(2:4))
  | (veg.type.A =="MediumMedium" &
     soil.depth.1234 %in% c(1))
  ]   <- "Average"
  
  ## Good
  soilquality[
  (  veg.type.A =="MediumMedium" & soil.depth.1234 %in% c(2:4))
  | (veg.type.A =="RichMedium"   & soil.depth.1234 %in% c(1:2))
  | (veg.type.A =="RichWet"      & soil.depth.1234 %in% c(1))
  | (veg.type.A =="RichDry"      & soil.depth.1234 %in% c(1))
  ]   <- "Good"

  ## AlmostBest
  soilquality[
  (veg.type.A == "RichMedium"        & soil.depth.1234 %in% c(3:4)) |
  (veg.type.A == "RichWet"       & soil.depth.1234 %in% c(2:3)) |
  (veg.type.A == "RichDry"   & soil.depth.1234 %in% c(2:3))
  ]   <- "AlmostBest"
  
  ## Best
  soilquality[
  (veg.type.A =="RichWet" & soil.depth.1234 %in% c(4))
  | (veg.type.A =="RichDry" & soil.depth.1234 %in% c(4))
  ]   <- "Best"

  ## Convert to a factor and guarantee levels order
  soilquality <- factor(
    soilquality,
    levels=c("Poorest","Average","Good","AlmostBest","Best")
  )
  return(soilquality)    
}
