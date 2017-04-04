\name{AM2016ClimateSensitiveSINorway}
\alias{AM2016ClimateSensitiveSINorway}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Climate-sensitive site index models for Norway
}
\description{
Implementation of models for climate-sensitive site index models for
Norway as described in Antón-Fernández et al. (2016).

}
\usage{
AM2016ClimateSensitiveSINorway(soilquality, t.early.summer, waterbal, SI.spp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{soilquality}{
A factor with levels 1 to 5 indicating the soilquality category. 1
being the poorest soils and 5 the best soils
}
  \item{t.early.summer}{
A vector with sum temperatures (in C) in spring and  early summer (april, june and july)
}
  \item{waterbal}{
A vector with the montly moisture surplus in June (difference between
the 30-year mean precipitation in June and mean potential evapotranspiration in June.). 
}
  \item{SI.spp}{
SI species, that is, the species for which SI should be calculated. 1 = spruce, 2 = pine, 3 = birch.
}
}

\value{
Returns a vector with the estimated SI. 
}
\references{
Anton-Fernandez, Clara, Blas Mola-Yudego, Lise Dalsgaard, and Rasmus Astrup. 2016. “Climate-Sensitive Site Index Models for Norway.” Canadian Journal of Forest Research 46 (6)

}
\author{
Clara Anton-Fernandez
}


\examples{
AM2016ClimateSensitiveSINorway (soilquality = as.factor(c(1,2,3,4)),
                                t.early.summer = c(10,20,30,10),
                                waterbal = c(-40, 20,10,10),
                                SI.spp = c(1,2,2,3))
  
}
