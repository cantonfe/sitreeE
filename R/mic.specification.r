#######################
## mic.specification ##
#######################
## function for specifying the management intensity class

mic.specification <- function(mic.spruce.high.si,
                              mic.spruce.medium.si,
                              mic.spruce.low.si,
                              
                              mic.pine.high.si,
                              mic.pine.medium.si,
                              mic.pine.low.si,
                              
                              mic.other.high,
                              mic.other.medium,
                              mic.other.low){
  
  ## check, if correct values were chosen
  ## pine
  if(!(mic.pine.high.si %in% c(0:4)))stop("mic.pine.high.si must be 1, 2, 3 or 4")
  if(!(mic.pine.medium.si %in% c(0:4)))stop("mic.pine.medium.si must be 1, 2, 3 or 4")
  if(!(mic.pine.low.si %in% c(0,1,3)))stop("mic.pine.low.si must be 1 or 3")
  ## spruce
  if(!(mic.spruce.high.si %in% c(0,5:9)))stop("mic.spruce.high.si must be 5, 6, 7, 8 or 9")
  if(!(mic.spruce.medium.si %in% c(0,5:9)))stop("mic.spruce.medium.si must be 5, 6, 7, 8 or 9")
  if(!(mic.spruce.low.si %in% c(0,5,7,9)))stop("mic.spruce.low.si must be 5, 7 or 9")
  ## other
  #if(!(mic.other.high %in% 9))stop("mic.other.high must be 9")
  #if(!(mic.other.medium %in% 9))stop("mic.other.medium must be 9")
  #if(!(mic.other.low %in% 9))stop("mic.other.low must be 9")
  
  ## an empty list
  mic.specifications <- list(
    ## thinning -> TRUE/FALSE
    thinning.pine.high.si=FALSE,
    thinning.pine.medium.si=FALSE,
    thinning.spruce.high.si=FALSE,
    thinning.spruce.medium.si=FALSE,
    ## height triggering thinning                         
    thinning.height.dm.pine.high.si=NA,
    thinning.height.dm.pine.medium.si=NA,
    thinning.height.dm.spruce.high.si=NA,
    thinning.height.dm.spruce.medium.si=NA,
    ## type of regeneration                         
    reg.type.pine.high.si=NA,
    reg.type.pine.medium.si=NA,
    reg.type.pine.low.si=NA,
    reg.type.spruce.high.si=NA,
    reg.type.spruce.medium.si=NA,
    reg.type.spruce.low.si=NA,
    ## main regeneration species                         
    reg.main.spec.pine.high.si=NULL,
    reg.main.spec.pine.medium.si=NULL,
    reg.main.spec.pine.low.si=NULL,
    reg.main.spec.spruce.high.si=NULL,
    reg.main.spec.spruce.medium.si=NULL,
    reg.main.spec.spruce.low.si=NULL,
    ## number of regeneration plants of the main species
    reg.main.n.plants.pine.high.si=NULL,
    reg.main.n.plants.pine.medium.si=NULL,
    reg.main.n.plants.pine.low.si=NULL,
    reg.main.n.plants.spruce.high.si=NULL,
    reg.main.n.plants.spruce.medium.si=NULL,
    reg.main.n.plants.spruce.low.si=NULL,
    ## additional regeneration species                         
    reg.add.spec.pine.high.si=NULL,
    reg.add.spec.pine.medium.si=NULL,
    reg.add.spec.pine.low.si=NULL,
    reg.add.spec.spruce.high.si=NULL,
    reg.add.spec.spruce.medium.si=NULL,
    reg.add.spec.spruce.low.si=NULL,
    ## number of regeneration plants of the additional species
    reg.add.n.plants.pine.high.si=NULL,
    reg.add.n.plants.pine.medium.si=NULL,
    reg.add.n.plants.pine.low.si=NULL,
    reg.add.n.plants.spruce.high.si=NULL,
    reg.add.n.plants.spruce.medium.si=NULL,
    reg.add.n.plants.spruce.low.si=NULL,
    ## wating time 
    waiting.time.pine.high.si=0,
    waiting.time.pine.medium.si=0,
    waiting.time.pine.low.si=0,
    waiting.time.spruce.high.si=0,
    waiting.time.spruce.medium.si=0,
    waiting.time.spruce.low.si=0,
    ## type of final harvest
    final.felling.pine.high.si=NA,
    final.felling.pine.medium.si=NA,
    final.felling.pine.low.si=NA,
    final.felling.spruce.high.si=NA,
    final.felling.spruce.medium.si=NA,
    final.felling.spruce.low.si=NA,
    ## minimal number of seed trees per ha
    seed.trees.n.min.pine.high.si=0,
    seed.trees.n.min.pine.medium.si=0,
    seed.trees.n.min.pine.low.si=0,
    seed.trees.n.min.spruce.high.si=0,
    seed.trees.n.min.spruce.medium.si=0,
    seed.trees.n.min.spruce.low.si=0,
    ## maximal number of seed trees per ha
    seed.trees.n.max.pine.high.si=0,
    seed.trees.n.max.pine.medium.si=0,
    seed.trees.n.max.pine.low.si=0,
    seed.trees.n.max.spruce.high.si=0,
    seed.trees.n.max.spruce.medium.si=0,
    seed.trees.n.max.spruce.low.si=0,
    ## % of volume left on the plot after clear cut
    prop.vol.left.cc.pine.high.si=0,
    prop.vol.left.cc.pine.medium.si=0,
    prop.vol.left.cc.pine.low.si=0,
    prop.vol.left.cc.spruce.high.si=0,
    prop.vol.left.cc.spruce.medium.si=0,
    prop.vol.left.cc.spruce.low.si=0
    )
  
  ########## 
  ## Pine ##
  ##########
  
  ##---------##
  ## high si ##
  ##---------##
  
  if(mic.pine.high.si %in% 1){
    mic.specifications$thinning.pine.high.si <- F
    mic.specifications$reg.type.pine.high.si <- "natural"
    mic.specifications$reg.main.spec.pine.high.si <- 10
    mic.specifications$waiting.time.pine.high.si <- 5
    mic.specifications$reg.main.n.plants.pine.high.si <- 2200
    mic.specifications$final.felling.pine.high.si <- "seed.tree"
    mic.specifications$seed.trees.n.min.pine.high.si <- 25
    mic.specifications$seed.trees.n.max.pine.high.si <- 75
    mic.specifications$prop.vol.left.cc.pine.high.si <- 18
  }
  
  if(mic.pine.high.si %in% 2){
    mic.specifications$thinning.pine.high.si <- T
    mic.specifications$thinning.height.dm.pine.high.si <- 180
    mic.specifications$reg.type.pine.high.si <- "natural"
    mic.specifications$reg.main.spec.pine.high.si <- 10
    mic.specifications$waiting.time.pine.high.si <- 5
    mic.specifications$reg.main.n.plants.pine.high.si <- 2200
    mic.specifications$final.felling.pine.high.si <- "seed.tree"
    mic.specifications$seed.trees.n.min.pine.high.si <- 25
    mic.specifications$seed.trees.n.max.pine.high.si <- 75
    mic.specifications$prop.vol.left.cc.pine.high.si <- 18
  }
  
  if(mic.pine.high.si %in% 3){
    mic.specifications$thinning.pine.high.si <- F
    mic.specifications$reg.type.pine.high.si <- "natural"
    mic.specifications$reg.main.spec.pine.high.si <- 10
    mic.specifications$reg.add.spec.pine.high.si <- 3
    mic.specifications$waiting.time.pine.high.si <- 5
    mic.specifications$reg.main.n.plants.pine.high.si <- 1500
    mic.specifications$reg.add.n.plants.pine.high.si <- 200
    mic.specifications$final.felling.pine.high.si <- "seed.tree"
    mic.specifications$seed.trees.n.min.pine.high.si <- 25
    mic.specifications$seed.trees.n.max.pine.high.si <- 75
    mic.specifications$prop.vol.left.cc.pine.high.si <- 18
  }
  
  if(mic.pine.high.si %in% 4){
    mic.specifications$thinning.pine.high.si <- T
    mic.specifications$thinning.height.dm.pine.high.si <- 180
    mic.specifications$reg.type.pine.high.si <- "natural"
    mic.specifications$reg.main.spec.pine.high.si <- 10
    mic.specifications$reg.add.spec.pine.high.si <- 3
    mic.specifications$waiting.time.pine.high.si <- 5
    mic.specifications$reg.main.n.plants.pine.high.si <- 1500
    mic.specifications$reg.add.n.plants.pine.high.si <- 200
    mic.specifications$final.felling.pine.high.si <- "seed.tree"
    mic.specifications$seed.trees.n.min.pine.high.si <- 25
    mic.specifications$seed.trees.n.max.pine.high.si <- 75
    mic.specifications$prop.vol.left.cc.pine.high.si <- 18
  }
  
  ##-----------##
  ## medium si ##
  ##-----------##
  if(mic.pine.medium.si %in% 1){
    mic.specifications$thinning.pine.medium.si <- F
    mic.specifications$reg.type.pine.medium.si <- "natural"
    mic.specifications$reg.main.spec.pine.medium.si <- 10
    mic.specifications$waiting.time.pine.medium.si <- 10
    mic.specifications$reg.main.n.plants.pine.medium.si <- 1800
    mic.specifications$final.felling.pine.medium.si <- "seed.tree"
    mic.specifications$seed.trees.n.min.pine.medium.si <- 25
    mic.specifications$seed.trees.n.max.pine.medium.si <- 75
    mic.specifications$prop.vol.left.cc.pine.medium.si <- 18
  }
  
  if(mic.pine.medium.si %in% 2){
    mic.specifications$thinning.pine.medium.si <- T
    mic.specifications$thinning.height.dm.pine.medium.si <- 160
    mic.specifications$reg.type.pine.medium.si <- "natural"
    mic.specifications$reg.main.spec.pine.medium.si <- 10
    mic.specifications$waiting.time.pine.medium.si <- 10
    mic.specifications$reg.main.n.plants.pine.medium.si <- 1800
    mic.specifications$final.felling.pine.medium.si <- "seed.tree"
    mic.specifications$seed.trees.n.min.pine.medium.si <- 25
    mic.specifications$seed.trees.n.max.pine.medium.si <- 75
    mic.specifications$prop.vol.left.cc.pine.medium.si <- 18
  }
  
  if(mic.pine.medium.si %in% 3){
    mic.specifications$thinning.pine.medium.si <- F
    mic.specifications$reg.type.pine.medium.si <- "natural"
    mic.specifications$reg.main.spec.pine.medium.si <- 10
    mic.specifications$reg.add.spec.pine.medium.si <- 3
    mic.specifications$waiting.time.pine.medium.si <- 10
    mic.specifications$reg.main.n.plants.pine.medium.si <- 1200
    mic.specifications$reg.add.n.plants.pine.medium.si <- 200
    mic.specifications$final.felling.pine.medium.si <- "seed.tree"
    mic.specifications$seed.trees.n.min.pine.medium.si <- 25
    mic.specifications$seed.trees.n.max.pine.medium.si <- 75
    mic.specifications$prop.vol.left.cc.pine.medium.si <- 18
  }
  
  if(mic.pine.medium.si %in% 4){
    mic.specifications$thinning.pine.medium.si <- T
    mic.specifications$thinning.height.dm.pine.medium.si <- 160
    mic.specifications$reg.type.pine.medium.si <- "natural"
    mic.specifications$reg.main.spec.pine.medium.si <- 10
    mic.specifications$reg.add.spec.pine.medium.si <- 3
    mic.specifications$waiting.time.pine.medium.si <- 10
    mic.specifications$reg.main.n.plants.pine.medium.si <- 1200
    mic.specifications$reg.add.n.plants.pine.medium.si <- 200
    mic.specifications$final.felling.pine.medium.si <- "seed.tree"
    mic.specifications$seed.trees.n.min.pine.medium.si <- 25
    mic.specifications$seed.trees.n.max.pine.medium.si <- 75
    mic.specifications$prop.vol.left.cc.pine.medium.si <- 18
  }
  
  ##--------##
  ## low si ##
  ##--------##
  if(mic.pine.low.si %in% 1){
    mic.specifications$reg.type.pine.low.si <- "natural"
    mic.specifications$reg.main.spec.pine.low.si <- 10
    mic.specifications$waiting.time.pine.low.si <- 10
    mic.specifications$reg.main.n.plants.pine.low.si <- 1800
    mic.specifications$final.felling.pine.low.si <- "seed.tree"
    mic.specifications$seed.trees.n.min.pine.low.si <- 25
    mic.specifications$seed.trees.n.max.pine.low.si <- 75
    mic.specifications$prop.vol.left.cc.pine.low.si <- 18
  }
  if(mic.pine.low.si %in% 3){
    mic.specifications$reg.type.pine.low.si <- "natural"
    mic.specifications$reg.main.spec.pine.low.si <- 10
    mic.specifications$reg.add.spec.pine.low.si <- 3
    mic.specifications$waiting.time.pine.low.si <- 10
    mic.specifications$reg.main.n.plants.pine.low.si <- 1200
    mic.specifications$reg.add.n.plants.pine.low.si <- 200
    mic.specifications$final.felling.pine.low.si <- "seed.tree"
    mic.specifications$seed.trees.n.min.pine.low.si <- 25
    mic.specifications$seed.trees.n.max.pine.low.si <- 75
    mic.specifications$prop.vol.left.cc.pine.low.si <- 18
  }
  
  
  ############ 
  ## Spruce ##
  ############
  
  ##---------##
  ## high si ##
  ##---------##
  
  if(mic.spruce.high.si %in% 5){
    mic.specifications$thinning.spruce.high.si <- F
    mic.specifications$reg.type.spruce.high.si <- "planting"
    mic.specifications$reg.main.spec.spruce.high.si <- 1
    mic.specifications$waiting.time.spruce.high.si <- 0
    mic.specifications$reg.main.n.plants.spruce.high.si <- 3000
    mic.specifications$final.felling.spruce.high.si <- "clear.cut"
    mic.specifications$prop.vol.left.cc.spruce.high.si <- 18
  }
  
  if(mic.spruce.high.si %in% 6){
    mic.specifications$thinning.spruce.high.si <- T
    mic.specifications$thinning.height.dm.spruce.high.si <- 180
    mic.specifications$reg.type.spruce.high.si <- "planting"
    mic.specifications$reg.main.spec.spruce.high.si <- 1
    mic.specifications$waiting.time.spruce.high.si <- 0
    mic.specifications$reg.main.n.plants.spruce.high.si <- 3000
    mic.specifications$final.felling.spruce.high.si <- "clear.cut"
    mic.specifications$prop.vol.left.cc.spruce.high.si <- 18
  }
  
  if(mic.spruce.high.si %in% 7){
    mic.specifications$thinning.spruce.high.si <- F
    mic.specifications$reg.type.spruce.high.si <- "planting"
    mic.specifications$reg.main.spec.spruce.high.si <- 1
    mic.specifications$reg.add.spec.spruce.high.si <- 3
    mic.specifications$waiting.time.spruce.high.si <- 0
    mic.specifications$reg.main.n.plants.spruce.high.si <- 2500
    mic.specifications$reg.add.n.plants.spruce.high.si <- 200
    mic.specifications$final.felling.spruce.high.si <- "clear.cut"
    mic.specifications$prop.vol.left.cc.spruce.high.si <- 18
  }
  
  if(mic.spruce.high.si %in% 8){
    mic.specifications$thinning.spruce.high.si <- T
    mic.specifications$thinning.height.dm.spruce.high.si <- 180
    mic.specifications$reg.type.spruce.high.si <- "planting"
    mic.specifications$reg.main.spec.spruce.high.si <- 1
    mic.specifications$reg.add.spec.spruce.high.si <- 3
    mic.specifications$waiting.time.spruce.high.si <- 0
    mic.specifications$reg.main.n.plants.spruce.high.si <- 2500
    mic.specifications$reg.add.n.plants.spruce.high.si <- 200
    mic.specifications$final.felling.spruce.high.si <- "clear.cut"
    mic.specifications$prop.vol.left.cc.spruce.high.si <- 18
  }
 
  if(mic.spruce.high.si %in% 9){
    mic.specifications$thinning.spruce.high.si <- F
    mic.specifications$reg.type.spruce.high.si <- "planting"
    mic.specifications$reg.main.spec.spruce.high.si <- 1
    mic.specifications$reg.add.spec.spruce.high.si <- 3
    mic.specifications$waiting.time.spruce.high.si <- 0
    mic.specifications$reg.main.n.plants.spruce.high.si <- 1800
    mic.specifications$reg.add.n.plants.spruce.high.si <- 200
    mic.specifications$final.felling.spruce.high.si <- "clear.cut"
    mic.specifications$prop.vol.left.cc.spruce.high.si <- 18
  }
  
  ##-----------##
  ## medium si ##
  ##-----------##
  if(mic.spruce.medium.si %in% 5){
    mic.specifications$thinning.spruce.medium.si <- F
    mic.specifications$reg.type.spruce.medium.si <- "planting"
    mic.specifications$reg.main.spec.spruce.medium.si <- 1
    mic.specifications$waiting.time.spruce.medium.si <- 0
    mic.specifications$reg.main.n.plants.spruce.medium.si <- 2000
    mic.specifications$final.felling.spruce.medium.si <- "clear.cut"
    mic.specifications$prop.vol.left.cc.spruce.medium.si <- 18
  }
  
  if(mic.spruce.medium.si %in% 6){
    mic.specifications$thinning.spruce.medium.si <- T
    mic.specifications$thinning.height.dm.spruce.medium.si <- 160
    mic.specifications$reg.type.spruce.medium.si <- "planting"
    mic.specifications$reg.main.spec.spruce.medium.si <- 1
    mic.specifications$waiting.time.spruce.medium.si <- 0
    mic.specifications$reg.main.n.plants.spruce.medium.si <- 2000
    mic.specifications$final.felling.spruce.medium.si <- "clear.cut"
    mic.specifications$prop.vol.left.cc.spruce.medium.si <- 18
  }
  
  if(mic.spruce.medium.si %in% 7){
    mic.specifications$thinning.spruce.medium.si <- F
    mic.specifications$reg.type.spruce.medium.si <- "planting"
    mic.specifications$reg.main.spec.spruce.medium.si <- 1
    mic.specifications$reg.add.spec.spruce.medium.si <- 3
    mic.specifications$waiting.time.spruce.medium.si <- 0
    mic.specifications$reg.main.n.plants.spruce.medium.si <- 1500
    mic.specifications$reg.add.n.plants.spruce.medium.si <- 200
    mic.specifications$final.felling.spruce.medium.si <- "clear.cut"
    mic.specifications$prop.vol.left.cc.spruce.medium.si <- 18
  }
  
  if(mic.spruce.medium.si %in% 8){
    mic.specifications$thinning.spruce.medium.si <- T
    mic.specifications$thinning.height.dm.spruce.medium.si <- 160
    mic.specifications$reg.type.spruce.medium.si <- "planting"
    mic.specifications$reg.main.spec.spruce.medium.si <- 1
    mic.specifications$reg.add.spec.spruce.medium.si <- 3
    mic.specifications$waiting.time.spruce.medium.si <- 0
    mic.specifications$reg.main.n.plants.spruce.medium.si <- 1500
    mic.specifications$reg.add.n.plants.spruce.medium.si <- 200
    mic.specifications$final.felling.spruce.medium.si <- "clear.cut"
    mic.specifications$prop.vol.left.cc.spruce.medium.si <- 18
  }
  
  if(mic.spruce.medium.si %in% 9){
    mic.specifications$thinning.spruce.medium.si <- F
    mic.specifications$reg.type.spruce.medium.si <- "planting"
    mic.specifications$reg.main.spec.spruce.medium.si <- 1
    mic.specifications$reg.add.spec.spruce.medium.si <- 3
    mic.specifications$waiting.time.spruce.medium.si <- 0
    mic.specifications$reg.main.n.plants.spruce.medium.si <- 1200
    mic.specifications$reg.add.n.plants.spruce.medium.si <- 200
    mic.specifications$final.felling.spruce.medium.si <- "clear.cut"
    mic.specifications$prop.vol.left.cc.spruce.medium.si <- 18
  }
  
  ##--------##
  ## low si ##
  ##--------##
  if(mic.spruce.low.si %in% 5){
    mic.specifications$reg.type.spruce.low.si <- "planting"
    mic.specifications$reg.main.spec.spruce.low.si <- 1
    mic.specifications$waiting.time.spruce.low.si <- 0
    mic.specifications$reg.main.n.plants.spruce.low.si <- 2000
    mic.specifications$final.felling.spruce.low.si <- "clear.cut"
    mic.specifications$prop.vol.left.cc.spruce.low.si <- 18
  }
  
  if(mic.spruce.low.si %in% 7){
    mic.specifications$reg.type.spruce.low.si <- "planting"
    mic.specifications$reg.main.spec.spruce.low.si <- 1
    mic.specifications$reg.add.spec.spruce.low.si <- 3
    mic.specifications$waiting.time.spruce.low.si <- 10
    mic.specifications$reg.main.n.plants.spruce.low.si <- 1500
    mic.specifications$reg.add.n.plants.spruce.low.si <- 200
    mic.specifications$final.felling.spruce.low.si <- "clear.cut"
    mic.specifications$prop.vol.left.cc.spruce.low.si <- 18
  }
  
  if(mic.spruce.low.si %in% 9){
    mic.specifications$reg.type.spruce.low.si <- "planting"
    mic.specifications$reg.main.spec.spruce.low.si <- 1
    mic.specifications$reg.add.spec.spruce.low.si <- 3
    mic.specifications$waiting.time.spruce.low.si <- 0
    mic.specifications$reg.main.n.plants.spruce.low.si <- 1200
    mic.specifications$reg.add.n.plants.spruce.low.si <- 200
    mic.specifications$final.felling.spruce.low.si <- "clear.cut"
    mic.specifications$prop.vol.left.cc.spruce.low.si <- 18
  }
  
  ########### 
  ## Other ##
  ###########
  
  ##---------##
  ## high si ##
  ##---------##
  mic.specifications$reg.type.other.high.si <- "natural"
  mic.specifications$reg.main.spec.other.high.si <- 30
  mic.specifications$reg.add.spec.other.high.si <- 1
  mic.specifications$waiting.time.other.high.si <- 5
  mic.specifications$reg.main.n.plants.other.high.si <- 2000
  mic.specifications$reg.add.n.plants.other.high.si <- 400
  #mic.specifications$final.felling.other.high.si <- "none"
    
  ##-----------##
  ## medium si ##
  ##-----------##
  mic.specifications$reg.type.other.medium.si <- "natural"
  mic.specifications$reg.main.spec.other.medium.si <- 30
  mic.specifications$reg.add.spec.other.medium.si <- 1
  mic.specifications$waiting.time.other.medium.si <- 5
  mic.specifications$reg.main.n.plants.other.medium.si <- 1000
  mic.specifications$reg.add.n.plants.other.medium.si <- 400
  #mic.specifications$final.felling.other.medium.si <- "none"
  
  ##--------##
  ## low si ##
  ##--------##
  mic.specifications$reg.type.other.low.si <- "natural"
  mic.specifications$reg.main.spec.other.low.si <- 30
  mic.specifications$reg.add.spec.other.low.si <- 1
  mic.specifications$waiting.time.other.low.si <- 5
  mic.specifications$reg.main.n.plants.other.low.si <- 1000
  mic.specifications$reg.add.n.plants.other.low.si <- 200
  #mic.specifications$final.felling.other.low.si <- "none"
  
  return(mic.specifications)
  
}
