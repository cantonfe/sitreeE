## Management following the scenarios defined by Birger and Hanne

management.none <- function(
                            tr = tr,
                            fl = fl,
                            common.vars = common.vars,
                            this.period = this.period,
                            next.period = next.period,
                            i.period = i.period,
                            mng.options    = mng.options,
                            ...){
  management <- list(
    management    = fl$management[, next.period]
  )  
  
  return(management)

}



## reassignInPackage("management.none", "skogsimExtra", management.none)
