
age.stand.recalculate <- function(
                                  stand.age.years, #= "stand.age.years.tm1",
                                  stand.age.reg.years,  #= "best.alder.reg.tm1",
                                  sesong,          #= "sesong.tm1",
                                  dev.class,       #= "dev.class.tm1",
                                  SI.spp,          #= "abonitet.tre.tm1",
                                  SI.m,        #= "abonitet.tm1",
                                  hovedhogstaar   #= "hovedhogstaar.tm1"
                                  ){
    
    ##----------------------------------
    ##  CREATING THE VENTETID TABLE
    ventetid <- data.frame(
        bonitet   = c(26, 23, 20, 17, 14, 11,  8,  6),
        granfuru  = c( 0,  0,  0,  0,  5,  5, 15, 15),
        lauv      = c( 0,  0,  0,  0,  0,  5,  5,  5))
    ## ------------------------------------
    ## REDEFINING AGE
    ## -----------------------------------
        
    ## dev.class 2
    ## dev.class or best.alder are not relevant for unproductive forest
    i <- is.finite(dev.class) & dev.class %in% c(21,22) 
    stand.age.years[i] <- stand.age.reg.years[i]
    
    ## dev.class 1
    my.ventetid <- rep(0, length(stand.age.years))
    ## harwoods
    i <- is.finite(SI.spp) & SI.spp %in% c(3)
    my.ventetid[i] <- ventetid$lauv    [match(SI.m[i], ventetid$bonitet)]
    ## spruce-pine
    i <- is.finite(SI.spp) & SI.spp %in% c(1, 2)
    my.ventetid[i] <- ventetid$granfuru[match(SI.m[i], ventetid$bonitet)]
          
    ## If it has been harvested before we redefine age
    jj <- dev.class %in% c(11, 12)  & is.finite(hovedhogstaar)
    stand.age.years[jj] <- sesong[jj] - hovedhogstaar[jj] - my.ventetid[jj]
    
    ##stand.age.years[jj][stand.age.years[jj] > 0] <- 0
    ##kk <- is.finite(dev.class) & dev.class %in% c(11,12)  & !is.finite(hovedhogstaar)
    ##stand.age.years[kk] <- 0
          
    ## if null zero
    stand.age.years[!is.finite(stand.age.years)] <- 0
    return(stand.age.years)
    
}
