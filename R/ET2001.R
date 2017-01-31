ET2001 <-
function (spp, dbh.mm, SI.m, PBAL.m2.ha, pr.gran.ba) 
{
    u.spp <- unique(spp)
    if (!all(u.spp %in% c("spruce", "pine", "birch", "other"))) {
        message("spp should only contain values spruce, pine, birch, other")
    }
    di <- rep(NA, length(spp))
    for (my.spp in u.spp) {
        if (my.spp == "spruce") {
            a0 <- 8.0599
            b1 <- -6.702
            b2 <- -0.0281
            b3 <- -0.0264
            b4 <- -0.0132
        }
        else if (my.spp == "pine") {
            a0 <- 8.4904
            b1 <- -14.266
            b2 <- -0.0462
            b3 <- -0.0761
            b4 <- 0
        }
        else if (my.spp == "birch") {
            a0 <- 4.8923
            b1 <- -2.528
            b2 <- 0
            b3 <- 0
            b4 <- 0
        }
        else if (my.spp == "other") {
            a0 <- 5.1575
            b1 <- -7.3544
            b2 <- -0.0199
            b3 <- 0
            b4 <- 0
        }
        i.tree <- spp == my.spp
        di[i.tree] <- 1 - (1 + exp(-(a0 + b1 * (dbh.mm[i.tree]/10)^-1 + 
            b2 * PBAL.m2.ha[i.tree] + b3 * SI.m[i.tree] + b4 * 
            pr.gran.ba[i.tree])))^-1
    }
    return(di)
}
