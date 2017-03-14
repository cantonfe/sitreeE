###########################
## biomass function      ##
## after Marklund (1988) ##
###########################
## I think this is M1988 code with potentially varying coefficients and
## with standard errors, so this is somehow a duplicate of M1988.Rune

M1988 <- function(
   dbh.cm   ## diameter in cm
  , height.m ## height in m
  , standard.error = F ## should the standard error be returned additionally
  , se.factor = 1 ## multiplication factor to increase or decrease se (affects bias correction)
  , exp.transf = T ## if F, half the variance is subtracted
  ## function parameters stem
  , stem.wood.i = 0
  , stem.wood.d = 0
  , stem.wood.h = 0
  , stem.wood.lh = 0
  , stem.wood.se = 0
  , stem.wood.add = 0
  ## function parameters bark
  , bark.i = 0
  , bark.d = 0
  , bark.h = 0
  , bark.lh = 0
  , bark.se = 0
  , bark.add = 0
  ## function parameters living branches
  , living.branches.i = 0
  , living.branches.d = 0
  , living.branches.h = 0
  , living.branches.lh = 0
  , living.branches.se = 0
  , living.branches.add = 0
  ## function parameters dead branches
  , dead.branches.i = 0
  , dead.branches.d = 0
  , dead.branches.h = 0
  , dead.branches.lh = 0
  , dead.branches.se = 0
  , dead.branches.add = 0
  ## function parameters stump
  , stump.i = 0
  , stump.d = 0
  , stump.h = 0
  , stump.lh = 0
  , stump.se = 0
  , stump.add = 0
  ## function parameters stump and roots 
  , stump.roots.i = 0
  , stump.roots.d = 0
  , stump.roots.h = 0
  , stump.roots.lh = 0
  , stump.roots.bias = 0
  , stump.roots.se = 0
  , stump.roots.add = 0
){
  
  ## initialise the return variables
  biom.stump.roots <- biom.living.branches <- biom.dead.branches <-
    biom.stem.wood <- biom.stump <- biom.bark <-
    se.stump.roots <- se.living.branches <- se.dead.branches <-
    se.stem.wood <- se.stump <- se.bark <- NULL
  
  ## stem wood biomass without bark
  tmp <- stem.wood.i + stem.wood.d * dbh.cm/(dbh.cm+stem.wood.add) +
      stem.wood.h * height.m + stem.wood.lh * log(height.m)
  ## standard error
  se <- se.stem.wood <- stem.wood.se* se.factor
  if(!exp.transf){biom.stem.wood <- tmp-se^2/2}
  if(exp.transf){biom.stem.wood <- exp(tmp)}
  
  ## biomass bark
  tmp <- bark.i + bark.d * dbh.cm/(dbh.cm+bark.add) + bark.h * height.m +
      bark.lh * log(height.m)
  ## standard error
  se <- se.bark <- bark.se* se.factor
  if(!exp.transf){biom.bark <- tmp-se^2/2}
  if(exp.transf){biom.bark <- exp(tmp)}
  
  ## living branches including needles
  tmp <- living.branches.i +
    living.branches.d * dbh.cm/(dbh.cm + living.branches.add) +
    living.branches.h * height.m + living.branches.lh * log(height.m)
  ## standard error
  se <- se.living.branches <- living.branches.se* se.factor
  if(!exp.transf){biom.living.branches <- tmp-se^2/2}
  if(exp.transf){biom.living.branches <- exp(tmp)}
  
  ## dead branches
  tmp <- dead.branches.i + dead.branches.d * dbh.cm/(dbh.cm+dead.branches.add) +
      dead.branches.h * height.m + dead.branches.lh * log(height.m)
  ## standard error
  se <- se.dead.branches <- dead.branches.se* se.factor
  if(!exp.transf){biom.dead.branches <- tmp-se^2/2}
  if(exp.transf){biom.dead.branches <- exp(tmp)}
  
  ## stump wood biomass (with bark?)
  tmp <- stump.i + stump.d * dbh.cm/(dbh.cm+stump.add)
  ## standard error
  se <- se.stump <- stump.se* se.factor
  if(!exp.transf){biom.stump <- tmp-se^2/2}
  if(exp.transf){biom.stump <- exp(tmp)}
  
  ## bias correction factor added to intercept as for other functions
  tmp <- stump.roots.bias + stump.roots.i + stump.roots.d *
      (dbh.cm*10) / ((dbh.cm*10) + stump.roots.add)
  ## standard error
  se <- se.stump.roots <- stump.roots.se* se.factor
  if(!exp.transf){biom.stump.roots <- tmp-log(1000)-(se)^2/2}
  if(exp.transf){biom.stump.roots <- exp(tmp)/1000}
  
  
  ## output of the function
  ## only estimates
  if(!standard.error){
    return(cbind(biom.living.branches.kg = biom.living.branches
                 , biom.dead.branches.kg = biom.dead.branches
                 , biom.stem.wood.kg = biom.stem.wood
                 , biom.stump.kg = biom.stump
                 , biom.bark.kg = biom.bark
                 , biom.stump.roots.kg = biom.stump.roots))
  }
  ## estimates and standard errors
  if(standard.error){
    return(cbind(biom.living.branches.kg = biom.living.branches
                 , se.living.branches.kg = se.living.branches
                 , biom.dead.branches.kg = biom.dead.branches
                 , se.dead.branches.kg = se.dead.branches 
                 , biom.stem.wood.kg = biom.stem.wood
                 , se.stem.wood.kg = se.stem.wood
                 , biom.stump.kg = biom.stump
                 , se.stump.kg = se.stump
                 , biom.bark.kg = biom.bark
                 , se.bark.kg = se.bark
                 , biom.stump.roots.kg = biom.stump.roots
                 , se.stump.roots.kg = se.stump.roots))
  }
  
}
