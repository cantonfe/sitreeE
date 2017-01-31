sp.prop <-
function (ustandID, BA.mm2, tree.sp) 
{
    u.spp <- levels(tree.sp)
    sa <- as.data.frame.matrix(xtabs(BA.mm2 ~ ustandID + tree.sp, 
        drop.unused.levels = TRUE))
    sa[, u.spp[!u.spp %in% names(sa)]] <- 0
    sa$ustandID <- factor(rownames(sa), levels = levels(ustandID))
    sa[, u.spp] <- prop.table(as.matrix(sa[, u.spp]), margin = 1)
    sa
}
