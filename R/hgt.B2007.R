hgt.B2007 <-
function (common.vars = common.vars) 
{
    with(common.vars,
         B2007hgt(spp       = spp,
                  SI.m      = fl[["SI.m"]][i.stand], 
                  lat.deg   = fl[["lat.deg"]][i.stand],
                  alt.m     = fl[["alt.m"]][i.stand], 
                  height.dm = tr$data[["height.dm"]][, this.period],
                  dbh.mm    = tr$data[["dbh.mm"]][, this.period],
                  SBA.m2.ha = SBA.m2.ha
                  )
         )
    
}
