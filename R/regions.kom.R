regions.kom <-
function (kom) 
{
    fylkene <- extract.fylkene(kom)
    regions <- rep(0, length(kom))
    fylkene.1 <- c("01", "02", "03", "07")
    komnr.1 <- c("0430", "0432", "0434", "0436", "0437", "0438", 
        "0439", "0441", "0511", "0512", "0513", "0514", "0515", 
        "0516", "0517", "0519", "0520", "0543", "0544", "0545", 
        "0618", "0619", "0620", "0633", "0826", "0827", "0828", 
        "0829", "0830", "0831", "0833", "0834")
    fylkene.2 <- c("09", "10")
    fylkene.3 <- c("11", "12", "14", "15")
    fylkene.4 <- c("16", "17", "18", "19")
    regions[fylkene %in% fylkene.1] <- "\303\203\302\230stland"
    regions[fylkene %in% fylkene.2] <- "Telemark"
    regions[fylkene %in% fylkene.3] <- "Vest"
    regions[fylkene %in% fylkene.4] <- "Nord"
    regions[kom %in% komnr.1] <- "Telemark"
    i <- ((fylkene %in% c("04", "05", "06", "08")) & (regions == 
        0))
    regions[i] <- "\303\203\302\230stland"
    return(regions)
}
