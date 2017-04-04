
AM2016ClimateSensitiveSINorway <- function(soilquality,
                                           t.early.summer,
                                           waterbal,
                                           SI.spp){
  newdata <- data.frame(soilquality, t.early.summer, waterbal, SI.spp)

  ff <- 
    function (mod2, j2, alpha, waterbal) 
    {
      my.waterbal <- ifelse(waterbal >= j2, waterbal - j2, 0)
      mia <- my.waterbal/alpha
      return(mod2/alpha * mia^(mod2 - 1) * exp(-mia^mod2))
    }
  my.f.si <- function (mod2, j2, alpha, waterbal, epsilon) 
  {
    fnl <- (1/epsilon * ff(mod2, j2, alpha, waterbal))
    return(fnl)
  }

  
  coef.spruce = list(
    my.pot = c( 15.49293622, 21.77273291, 22.94929955, 26.17747846),
    mod1   = c(  0.19072939,  0.11998223,  0.12135588, 0.09345424),
    j1     = c(  4.14512039,  2.71120367,  2.11304962, 1.71432839)
  )
  coef.pine <-
    list(
      epsilon = 0.0076,
      my.pot = c(9.4690313, 13.3916910, 14.7683895, 17.8809481),
      mod1  = 0.2067467,
      j1    = c( 2.4028455, 2.9210633, 2.4688852, 3.0053384),
      mod2  =  1.5041493,
      j2    = -103.5199224,
      alpha =  98.0556598)
  coef.birch <- list(
    epsilon = 0.0060,
    my.pot = c(13.0409597, 18.0118682, 22.5634407, 25.9417875),
    mod1   =  0.1124784,
    j1     = 2.7009295 ,
    mod2   = 1.8938957,
    j2     = -115.2986498,
    alpha  = 137.4866614
  )
  md.birch <-  "6 + (coef.birch$my.pot[soilquality] - 6) *
                         my.f.si(mod2 = coef.birch$mod2, j2 = coef.birch$j2,
                         alpha = coef.birch$alpha,
                         waterbal= waterbal,
                         epsilon = coef.birch$epsilon)*
                         ((1 + exp(-coef.birch$mod1 *
                         t.early.summer +
                         coef.birch$j1))^-1)"
  md.pine <- "6 + (coef.pine$my.pot[soilquality] - 6) *
                  my.f.si(coef.pine$mod2, coef.pine$j2, coef.pine$alpha,
                  waterbal, coef.pine$epsilon) *
                  ((1 + exp(-coef.pine$mod1 * t.early.summer +
                  coef.pine$j1[soilquality]))^-1)"
  md.spruce <- " 6 + (coef.spruce$my.pot[soilquality] - 6) *
               ((1 + exp(-coef.spruce$mod1[soilquality] *
               t.early.summer + coef.spruce$j1[soilquality]))^-1)"
  
  SI.hist <- rep(NA, length(soilquality))
  if (sum(SI.spp == 1) > 0){
    SI.hist[SI.spp == 1] <- eval(parse(text = md.spruce), newdata[SI.spp == 1,])
  }
  if (sum(SI.spp == 2) > 0){
    SI.hist[SI.spp == 2] <- eval(parse(text = md.pine), newdata[SI.spp == 2,])
  }
  if (sum(SI.spp == 3) > 0){
    SI.hist[SI.spp == 3] <- eval(parse(text = md.birch), newdata[SI.spp == 3,])
  }
  
  return(SI.hist)
  
}
