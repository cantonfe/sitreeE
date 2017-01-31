## function to calculate the density after decomposition
decompose.dead.wood <- function(init.dry.dens,k.fac,time.since.death)
{
  density <- init.dry.dens*exp(-k.fac*time.since.death)
  return(density)
}