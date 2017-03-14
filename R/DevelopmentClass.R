
## We can't differentiate between classes 11 and 21 so all 11 and 21 will be 11
development.class <- function(age.years, SI.spp, SI.m){

    hogstkl.table <- data.frame(
                              bonitet = rep(c(23,20,17,14,11,8,6),2),
                              spp     = rep(c("conif","harw"),each=7),
                              class3 = c(
                                20,20,25,30,35,45,55,
                                15,15,20,25,25,25,30),
                              class4 = c(
                                40,45,55,60,70,75,85,
                                25,30,40,45,45,45,55),
                              class5 = c(
                                60,70,80,90,100,110,120,
                                40,50,60,70,70,70,80)
                              )
    if (sum (!unique (SI.spp) %in% c(1,2,3)) > 0) message("SI.spp should be 1, 2, or 3")
    
    ## create empty hosgtkl
    dev.class <- rep(11,length(SI.spp))
    
SI.m[SI.m > 23] <- 23
    ## CONIF
    i.conif <- SI.spp %in% c(1,2)
    
    i.bonitet <- match(SI.m[i.conif],
                       hogstkl.table[hogstkl.table$spp == "conif", "bonitet"])
    cc <- age.years[i.conif] >= hogstkl.table[hogstkl.table$spp == "conif",][i.bonitet,
                                                c( "class3", "class4", "class5" )]
  dev.class[i.conif][cc[,"class3"]] <- 31
  dev.class[i.conif][cc[,"class4"]] <- 41
  dev.class[i.conif][cc[,"class5"]] <- 51
    
    ## HARW
    i.harw <- ! i.conif
    
    i.bonitet <- match(SI.m[i.harw],
                       hogstkl.table[hogstkl.table$spp == "harw", "bonitet"])
    cc <- age.years[i.harw] > hogstkl.table[hogstkl.table$spp == "harw",][i.bonitet,
                                                c( "class3", "class4", "class5" )]
    dev.class[i.harw][cc[,"class3"]] <- 31
    dev.class[i.harw][cc[,"class4"]] <- 41
    dev.class[i.harw][cc[,"class5"]] <- 51
    
    return(dev.class)
}
