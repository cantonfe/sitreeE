## defining the decomposition class
decomp.class.func <- function(decomp)
{
  decomp.class <- rep(5, length(decomp))
  decomp.class[decomp > 0.25] <- 4
  decomp.class[decomp > 0.50] <- 3
  decomp.class[decomp > 0.75] <- 2
  decomp.class[decomp > 0.95] <- 1
  decomp.class[is.na(decomp)] <- NA
  return(decomp.class)
}
