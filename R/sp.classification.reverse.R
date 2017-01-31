sp.classification.reverse <-
function (sp.class) 
{
    tree.sp <- rep(0, length(sp.class))
    tree.sp[sp.class %in% "spruce"] <- treslag.gran[1]
    tree.sp[sp.class %in% "pine"] <- treslag.furu[1]
    tree.sp[sp.class %in% "birch"] <- treslag.lauv[1]
    return(tree.sp)
}
