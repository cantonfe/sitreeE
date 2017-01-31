si.change.clim.var <- function(flateid, temp, pp, mo, lat.deg){    
    ## Calculate PET
    PET <- PET.calc(
        flateid       = flateid
      , temp          = temp
      , months        = as.numeric(mo  )
      , lat.degrees   = lat.deg
      , name.months   = 1:12
        )

    i.6 <- mo == 6
    i.456 <- mo %in% 4:6
    
    waterbal <- pp[i.6] - PET[i.6]
    t.early.summer <- aggregate(temp[i.456], by = list(flateid = flateid[i.456]),
                                FUN = sum)
    if (any(t.early.summer$x < 0)) warning(paste0('t.early.summer < 0 is ',
                                                  sum(t.early.summer$x <0)))
    t.early.summer$x[t.early.summer$x < 0] <- 0 ## it shouldn't happen, but in case
    
    return(data.frame  (
        flateid        = t.early.summer$flateid,
        waterbal       = waterbal,
        t.early.summer = t.early.summer$x)
           )
}
