stand.comp.func <- function(perc.con=NA,perc.dec=NA,perc.spruce=NA,perc.temp.dec=NA)
{
  
  #if(any(is.na(c(perc.con,perc.dec,perc.temp.dec))))stop("'perc.con', 'perc.dec' and 'perc.temp.dec' cannot be NA")
  
  if(all(is.na(c(perc.con,perc.dec,perc.temp.dec)))){stand.comp.class <- NA}else{
    
    stand.comp.class <- ifelse(perc.con>=75 & perc.dec<25 & perc.spruce<50,1,
                               ifelse(perc.con>=75 & perc.dec<25 & perc.spruce>=50,2,
                                      ifelse(perc.con>=50 & perc.con<75 & perc.dec>=25 & perc.dec<=50 & perc.temp.dec==0,3,
                                             ifelse(perc.con>=50 & perc.con<75 & perc.dec>=25 & perc.dec<50 & perc.temp.dec>0 & perc.temp.dec<25,4,
                                                    ifelse(perc.con>=50 & perc.con<75 & perc.dec>=25 & perc.dec<50 & perc.temp.dec>=25 & perc.temp.dec<50,5,
                                                           ifelse(perc.con<50 & perc.dec>=50 & perc.temp.dec==0,4,
                                                                  ifelse(perc.con<50 & perc.dec>=50 & perc.temp.dec>0 & perc.temp.dec<25,5,
                                                                         ifelse(perc.con<50 & perc.dec>=50 & perc.temp.dec>=25,6,NA))))))))
  }
  return(stand.comp.class)
}
