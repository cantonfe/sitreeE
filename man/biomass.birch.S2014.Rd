\name{biomass.birch.S2014}
\alias{biomass.birch.S2014}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Implements biomass functions for birch for Norway from Smith et al (2014, 2016)
}
\description{
Implements biomass functions for birch for Norway from Smith et al (2014, 2016). Total biomass aboveground does include stump calculated using biomass.birch.M1988. Total biomass belowground does not include stump.
}
\usage{
biomass.birch.S2014(dbh.cm, H.m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dbh.cm}{
%%     ~~Describe \code{dbh.cm} here~~
}
  \item{H.m}{
%%     ~~Describe \code{H.m} here~~
}
}
\details{
It uses stump.roots from biomass.birch.M1988 to calculate aboveground biomass (in kg) and belowground biomass (in kg). 
Aboveground biomass is calculated as Smith's aboveground biomass (2014) + stump.roots. 
Belowground biomass is calcualted as Smith's belowground biomass (2016) - stump.roots.
}
\value{
Returns a data.frame with the following biomass components: biomass.total.kg, biomass.aboveground.kg.S2014, biomass.belowground.kg.S2014, biomass.belowground.kg,  biomass.aboveground.kg, living.branches, dead.branches, stem.wood, stump.roots, bark, usoil, rot1, rot2, foliage)
}
\references{
Smith, Aaron, Aksel Granhus, and Rasmus Astrup. 2016. “Functions for Estimating Belowground and Whole Tree Biomass of Birch in Norway.” Scandinavian Journal of Forest Research 31 (6): 568–82. 
Smith, Aaron, Aksel Granhus, Rasmus Astrup, Ole Martin Bollandsås, and Hans Petersson. 2014. “Functions for Estimating Aboveground Biomass of Birch in Norway.” Scandinavian Journal of Forest Research 29 (6): 565–78. 
}
\author{
Clara Anton Fernandez (caf@nibio.no)
}

\seealso{
\code{\link{biomass.Norway}}, \code{\link{biomass.birch.M1988}}
}
\examples{
biomass.birch.S2014(dbh.cm = c(10, 20), H.m = c(12, 16))
}

