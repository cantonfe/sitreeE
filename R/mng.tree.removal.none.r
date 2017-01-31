########################
## tree.removal.mic.r ##
########################
## tree removal according to MIC

tree.removal.none <- function(
                             tr
                            ,fl
                            ,common.vars
                            ,management
                            ,this.period
                            ,i.period
                           , mng.options
                           , ...){

  final.felling <- rep(F,length(tr$data$ustandID))
  
  return(final.felling)
  
}
## reassignInPackage("tree.removal.none", "skogsimExtra", tree.removal.none)
