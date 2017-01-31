sp.classification.biodiv <- function (tree.sp) 
{
  sp.clas <- factor(rep("other", length(tree.sp)), levels = c("spruce","pine", "borbl", "tembl","other"))
  sp.clas[tree.sp %in% treslag.gran] <- "spruce"
  sp.clas[tree.sp %in% treslag.furu] <- "pine"
  sp.clas[tree.sp %in% treslag.borbl] <- "borbl"
  sp.clas[tree.sp %in% treslag.tembl] <- "tembl"
  return(sp.clas)
}
