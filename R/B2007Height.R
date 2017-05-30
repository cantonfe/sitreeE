B2007.height <- function (common.vars,
                         this.period,
                         tr,
                         dbh.inc.mm, ...) 
{
  i.to.tree <- common.vars$i.stand
  b.spruce <- list(b1 = 31569,
                   b2 = 0.1543,
                   b3 = -1.0825,
                   b4 = -0.0221,
                   b5 = -33.332,
                   b6 = -0.5436,
                   b7 = -0.1233 
                   )
  b.pine <- list(b1 = 9655,
                 b2 = 0.3994,
                 b3 = -1.1371,
                 b4 = 0,
                 b5 = -105.50,
                 b6 = -0.8410,
                 b7 = -0.2193
                 )
  b.birch <- list(b1 =  830.30,
                  b2 =  0.3854,
                  b3 = -0.4165 ,
                  b4 = -0.0220 ,
                  b5 =  -14.821,
                  b6 =  -0.431,
                  b7 = -0.1230 
                  )
  b.other <- list(b1 =  101572 ,
                  b2 =   0.3023,
                  b3 = -1.5432 ,
                  b4 = 0,
                  b5 =  -15.455,
                  b6 =  -0.4166,
                  b7 = -0.1207
                  )
  spp <- common.vars$spp
  height <- next.height <- rep(NA, nrow(tr$data$dbh.mm))

  ## height this.period according to korf
  i.spruce <- spp == 'spruce'
  i.pine <- spp == 'pine'
  i.birch <- spp == 'birch'
  i.other <- spp == 'other'
  
  height[i.spruce] <-
    B2007.height.function(
      SI = fl$SI.m[i.to.tree][i.spruce],
      LAT = fl$lat.deg[i.to.tree][i.spruce],
      ALT = fl$alt.m[i.to.tree][i.spruce],
      DBH = tr$data$dbh.mm[spp == 'spruce', this.period],
      BA  = common.vars$SBA.m2.ha[i.spruce],
      b = b.spruce)

    height[i.pine] <-
    B2007.height.function(
      SI = fl$SI.m[i.to.tree][i.pine],
      LAT = fl$lat.deg[i.to.tree][i.pine],
      ALT = fl$alt.m[i.to.tree][i.pine],
      DBH = tr$data$dbh.mm[spp == 'pine', this.period],
      BA  = common.vars$SBA.m2.ha[i.pine],
      b = b.pine)
  
    height[i.birch] <-
    B2007.height.function(
      SI = fl$SI.m[i.to.tree][i.birch],
      LAT = fl$lat.deg[i.to.tree][i.birch],
      ALT = fl$alt.m[i.to.tree][i.birch],
      DBH = tr$data$dbh.mm[spp == 'birch', this.period],
      BA  = common.vars$SBA.m2.ha[i.birch],
      b = b.birch)
  
    height[i.other] <-
    B2007.height.function(
      SI = fl$SI.m[i.to.tree][i.other],
      LAT = fl$lat.deg[i.to.tree][i.other],
      ALT = fl$alt.m[i.to.tree][i.other],
      DBH = tr$data$dbh.mm[spp == 'other', this.period],
      BA  = common.vars$SBA.m2.ha[i.other],
      b = b.other)
  

  ## height next.period according to korf
  ##browser()
   next.height[i.spruce] <-
    B2007.height.function(
      SI = fl$SI.m[i.to.tree][i.spruce],
      LAT = fl$lat.deg[i.to.tree][i.spruce],
      ALT = fl$alt.m[i.to.tree][i.spruce],
      DBH = tr$data$dbh.mm[spp == 'spruce', this.period]+
+                      dbh.inc.mm [spp == 'spruce'],
      BA  = common.vars$SBA.m2.ha[i.spruce],
      b = b.spruce)

    next.height[i.pine] <-
    B2007.height.function(
      SI = fl$SI.m[i.to.tree][i.pine],
      LAT = fl$lat.deg[i.to.tree][i.pine],
      ALT = fl$alt.m[i.to.tree][i.pine],
      DBH = tr$data$dbh.mm[spp == 'pine', this.period]+
+                      dbh.inc.mm [spp == 'pine'],
      BA  = common.vars$SBA.m2.ha[i.pine],
      b = b.pine)
  
    next.height[i.birch] <-
    B2007.height.function(
      SI = fl$SI.m[i.to.tree][i.birch],
      LAT = fl$lat.deg[i.to.tree][i.birch],
      ALT = fl$alt.m[i.to.tree][i.birch],
      DBH = tr$data$dbh.mm[spp == 'birch', this.period]+
+                      dbh.inc.mm [spp == 'birch'],
      BA  = common.vars$SBA.m2.ha[i.birch],
      b = b.birch)
  
    next.height[i.other] <-
    B2007.height.function(
      SI = fl$SI.m[i.to.tree][i.other],
      LAT = fl$lat.deg[i.to.tree][i.other],
      ALT = fl$alt.m[i.to.tree][i.other],
      DBH = tr$data$dbh.mm[spp == 'other', this.period]+
+                      dbh.inc.mm [spp == 'other'],
      BA  = common.vars$SBA.m2.ha[i.other],
      b = b.other)

  ## height should never be below 13
  ##height[height < 13] <- 13
  height.inc.dm <- next.height - height

  print(quantile(height.inc.dm))
  
  return(height.inc.dm)
  
}

B2007.height.function <- function(SI, LAT, ALT, DBH, BA, b){
  13 + (b[['b1']]* SI ^ b[['b2']] * LAT ^ b[['b3']] * ALT ^ b[['b4']]) *
  exp(b[['b5']] * DBH^ b[['b6']] * BA ^ b[['b7']])
}
