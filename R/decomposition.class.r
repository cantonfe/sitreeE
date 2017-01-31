## defining the decomposition class
decomposition.class <- function(density)
{
  decomp.class <- rep(6, length(density))
  decomp.class[density > 0.05] <- 5
  decomp.class[density > 0.25] <- 4
  decomp.class[density > 0.50] <- 3
  decomp.class[density > 0.75] <- 2
  decomp.class[density > 0.95] <- 1
  decomp.class[is.na(density)] <- NA
  return(decomp.class)
}