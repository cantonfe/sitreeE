mort.imputation <-
    function (fn.tree.removal = "mort.imputation", removed = removed,
              common.vars = common.vars, this.period = this.period, tr = tr, fl=fl, ...) 
{
  ## Results might have been created while imputing dbh.inc
    fn.growth.pars <- list(...)$fn.growth.pars
    
    if (fn.growth.pars$only.mort){    
        mort.imputation.result <- do.call(what = "dbhi.imputation", 
                              args = list(
                                  common.vars = common.vars,
                                  this.period = this.period, 
                                  tr = tr,
                                  fl = fl,
                                  only.mort = TRUE,
                                  ...))
   } 
    
    mort <- ifelse(mort.imputation.result == "dead", TRUE, FALSE )
    print(table(mort))
    ## in the "removed" object we have predicted which trees inside plots with
    ## thinning or final felling harvest are alive, dead, or removed
    if (fn.tree.removal$fn.tree.removal == "tree.removal.imp"){
        mort[attr(removed, "dead")]  <- TRUE
        mort[attr(removed, "alive")] <- FALSE
    }
    
    return(mort)
}
## reassignInPackage("mort.imputation", "skogsim", mort.imputation)
