## Rune email -03-01-2017
## Some of the functions are different for diameter classes. When I computed the 
## increment (volume today compared with volume one year ago) some trees shifted 
##function because the “diameter one year ago” was just below the diameter limit. I 
## use the f_dbh  to choose function. This is  to make sure the two volumes for the 
## increment  comes from the same function.
## For volume estimation dbh and f_dbh are always the same, but in the 
##“tilvekstberegning” they are different.
FuruVol <-
function (dbh, f.dbh, trh, bark, enhet) 
{
    dmb <- dbh/10
    f.dmb <- round(f.dbh/10, 1)
    h <- trh/10
    vol <- rep(NA, length(dbh))
    b <- 3.17935 + 1.0289 * dmb - 0.27023 * dmb/h
    b <- round(b, 2)
    if (bark == "ub") {
        dub <- dmb - (b/10)
        dub <- round(dub, 3)
        f.dub <- f.dmb - ((3.17935 + 1.0289 * f.dmb - 0.27023 * 
            f.dmb/h)/10)
        f.dub <- round(dub, 1)
    }
    if (bark == "mb") {
        i <- f.dmb < 11.1
        vol[i] <- 0.6716 + (0.075708 * (dmb[i]^2)) + (0.029679 * 
            (dmb[i]^2) * h[i]) + (0.004341 * dmb[i] * (h[i]^2))
        i <- f.dmb > 11
        vol[i] <- -6.3954 + (0.178053 * (dmb[i]^2)) + (0.03317 * 
            (dmb[i]^2) * h[i]) - (0.003008 * (dmb[i]^2) * b[i])
    } else if (bark == "ub") {
        i <- f.dub < 11.1
        vol[i] <- 2.3393 + (0.010045 * (dub[i]^2)) + (0.038834 * 
            (dub[i]^2) * h[i]) + (0.002732 * dub[i] * (h[i]^2))
        i <- f.dub > 11
        vol[i] <- -3.5425 + (0.128182 * (dub[i]^2)) + (0.028268 * 
            (dub[i]^2) * h[i]) + (0.008216 * dub[i] * (h[i]^2))
    }
    if (enhet == "l") {
        return(round(vol))
    }
    else if (enhet == "c") {
        return(round(vol * 100))
    }
}
