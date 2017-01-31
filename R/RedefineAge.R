
#######################################################################################
## This code redefines age, takes into account best.alder.reg for classes 21 and 22
## and ventetid for development classes 11 and 12,
## and makes age = 0  if NULL
## this definition was used to create forecasts for carbon 2011

redefine.age <- function(
                         stand.age.years     ,## "best.alder.tm1",
                         stand.age.years.reg ,## "best.alder.reg.tm1",
                         years.since.harvest ,## "sesong.tm1",
                         development.class   ,## "hogstkl.tm1",
                         SI.spp              ,## "abonitet.tre.tm1",
                         SI.m                ## "abonitet.tm1"
                         ){
    
    ##----------------------------------
    ##  CREATING THE TB.VENTETID TABLE
    tb.ventetid <- data.frame(
        bonitet   = c(26, 23, 20, 17, 14, 11,  8,  6),
        granfuru  = c( 0,  0,  0,  0,  5,  5, 15, 15),
        lauv      = c( 0,  0,  0,  0,  0,  5,  5,  5))
    ## ------------------------------------
    ## REDEFINING AGE
    ## -----------------------------------
    ## if null zero
    stand.age.years[!is.finite(stand.age.years)] <- 0
    
    ## hogstkl 2
    ## if it exists hogstkl 2 after regulation, we will use that class
    i <- ((development.class == 21 |  development.class == 22) &
          is.finite(stand.age.years.reg))
    stand.age.years[i] <- stand.age.years.reg[i]
    
    ## hogstkl 1
    ventetid <- 0
    ## lauv
    i <- is.finite(SI.spp) & SI.spp == 3 # lauv
    ventetid[i] <- tb.ventetid$lauv[match(SI.m[i], tb.ventetid$bonitet)]
    ## gran-furu
    i <- is.finite(SI.spp) & (SI.spp == 2 | SI.spp  == 1 ) 
    ventetid[i] <- tb.ventetid$granfuru[match(SI.m[i], tb.ventetid$bonitet)]
    
    jj <- (development.class == 11 | development.class == 12)  &
        is.finite(years.since.harvest)
    stand.age.years[jj] <-   (years.since.harvest - ventetid)[jj]
    stand.age.years[stand.age.years[jj] > 0] <- 0

    stand.age.years[(development.class == 11 | development.class == 12)  &
        !is.finite(years.since.harvest) & is.finite(development.class)] <- 0
    
    return(stand.age.years)
    
}
