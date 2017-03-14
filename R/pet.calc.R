
## Calculate potential evapotranspiration according to Thornwaite 1948
PET.calc <- function(
                     flateid   = NULL ## variable which identifies plots
                   , temp      = NULL ## variable which identifies temperature
                   , months    = NULL  ## variable which identifies the month
                   , lat.degrees   = NULL ## latitude variable in degrees
                     ## from January to December how are the months identified, e.g. "Jan", "Feb"
                   , name.months   = substr(paste("0",seq(1:12),sep=""),
                                            nchar(1:12),3 )
                     ) {
  
  if (length(unique(months)) != 12) {
    stop('To correctly calculate PET you need information on the 12 months')
  }
  ## var.temp  is the average daily temperature (degrees Celsius;
  ## if this is negative, use 0)
  ##of the month being calculated
  temp[temp < 0] <- 0
  
  ## I HEAT INDEX, sum of the 12 monthly index values i
  ## i for each month is derived from mean monthly temperature using the formula
  dt.PET <- data.frame(flateid = flateid,
                       temp = temp,
                       months = months,
                       lat.degrees = lat.degrees)
  dt.PET$i <-  (dt.PET$temp/5)^1.514 
  I  <- aggregate(data.frame(I = dt.PET$i),
                  list(flateid = flateid),
                  sum)
  dt.PET$I <- I$I[match(dt.PET$flateid, I$flateid)]
  
  ## m
  dt.PET$m <- (6.75e-7 * dt.PET$I^3 -
                 7.71e-5 * dt.PET$I^2 +
                 1.79e-2 * dt.PET$I +
                 0.492)
  
  ## NDM is the NUMBER OF DAYS PER MONTH
  ## we will ignore leap years
  num.year <- 1
  info.months <- data.frame(
    name.months = name.months,
    NDM.year =
      c(as.POSIXlt(seq(as.Date("2007-02-01"),
                       by="month",
                       length.out=num.year*12)-1)$mday)
  )
  dt.PET$NDM <- info.months$NDM.year[match(dt.PET$months,
                                           info.months$name.months)]
  
  sa <- cbind(c(9:(365+8)),
              rep(info.months$name.months, info.months$NDM.year))
  info.months$day.year <- aggregate(sa[,1], list(sa[,2]),mean)$x
  
  
  ## average day of the month starting Dec 22 0,...
  dt.PET$day.year <- info.months$day.year[match(dt.PET$months ,
                                                info.months$name.months)]
  
  ## N is the maximum number of sun hours; following Herbert Glarner,
  ## http://herbert.gandraxa.com/length_of_day.xml, 2010
  Axis     <- 23.439 ## This is a constant angle between the equatorial plane and the ecliptic
  ### plane
  Axis.rad <- Axis * pi /180
  j        <- pi/182.5 ## 182.625 if we accounted for leap years
  dt.PET$N.m   <- 1 - (
    tan( dt.PET$lat.degrees*pi/180 ) *
      tan(Axis.rad * cos(j * dt.PET$day.year) )
  )
  
  ##Adjust the limits of m to be between 0...2; then
  dt.PET$N.m[dt.PET$N.m < 0] <- 0
  dt.PET$N.m[dt.PET$N.m > 2] <- 2
  dt.PET$b <- acos(1 - dt.PET$N.m)/pi ## exposed frction of the sun's circle,acos in radians
  dt.PET$N <- dt.PET$b * 24 ## number of hours 
  
  ## K is a correction coefficient computed as a function of the latitude and month by
  dt.PET$K <- (dt.PET$N/12)*(dt.PET$NDM/30)
  
  dt.PET$PET <- 16 * dt.PET$K * (10 * dt.PET$temp /dt.PET$I)^dt.PET$m
  
  return(dt.PET$PET)
  
}
