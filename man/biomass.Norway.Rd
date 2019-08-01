\name{biomass.Norway}
\alias{biomass.Norway}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Biomass functions for Norway
}
\description{
Implements the currently used functions to estimate biomass in Norway.
}
\usage{
biomass.Norway(tr, this.period)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{
A \emph{trList} class object.
}
  \item{this.period}{
The period for which to calculate biomass.
}
}
\details{
If \emph{tr} is of class \emph{trListDead} biomass is estimated for the
last measurement.
It uses \emph{biomass.spruce.M1988}, \emph{biomass.pine.M1988},
\emph{biomass.birch.S2014}, \emph{biomass.birch.M1988} (to add the stump
to aboveground biomass and remove it from belowground biomass from
biomass.birch.S2014 equations).
}
\value{
Returns a list
  \item{biomass.kg }{A data.frame with total biomass, biomass
    aboveground, biomass belowground, all in kg }
  \item{biomass.kg.components }{A data.frame with biomass components
    (see \code{\link{biomass.birch.M1988}}) in kg}
}
\references{
 Marklund, L. G. 1988. “Biomassafunktioner för tall, gran och björk i Sverige (Biomass functions for pine, spruce and birch in Sweden).” Report 45. Umeå, Sweden: Swedish University of Agricultural Sciences. Department of Forest Survey. 

Petersson, Hans, and Göran Ståhl. 2006. “Functions for Below-Ground
Biomass of Pinus Sylvestris, Picea Abies, Betula Pendula and Betula
Pubescens in Sweden.” Scandinavian Journal of Forest Research 21 (S7):
84–93. doi:10.1080/14004080500486864.
Smith, Aaron, Aksel Granhus, and Rasmus Astrup. 2016. “Functions for Estimating Belowground and Whole Tree Biomass of Birch in Norway.” Scandinavian Journal of Forest Research 31 (6): 568–82. 
Smith, Aaron, Aksel Granhus, Rasmus Astrup, Ole Martin Bollandsås, and Hans Petersson. 2014. “Functions for Estimating Aboveground Biomass of Birch in Norway.” Scandinavian Journal of Forest Research 29 (6): 565–78. 


}
\author{
Clara Anton Fernandez \email{caf@nibio.no}
}

\seealso{
  \code{\link{biomass.birch.S2014}}, \code{\link{biomass.birch.M1988}}, \code{\link{biomass.pine.M1988}}, \code{\link{biomass.spruce.M1988}}
  
}
\examples{
res <- sitree (tree.df   = tr,
                 stand.df  = fl,
                 functions = list(
                     fn.growth     = 'grow.dbhinc.hgtinc',
                     fn.mort       = 'mort.B2007',
                     fn.recr       = 'recr.BBG2008',
                     fn.management = 'management.prob',
                     fn.tree.removal = 'mng.tree.removal',
                     fn.modif      = NULL, 
                     fn.prep.common.vars = 'prep.common.vars.fun'
                 ),
                 n.periods = 5,
                 period.length = 5,
                 mng.options = NA,
                 print.comments = FALSE,
                 fn.dbh.inc = "dbhi.BN2009",
                 fn.hgt.inc =  "height.korf", 
                 species.spruce = c(1, 2, 3),
                 species.pine = c(10, 11, 20, 21, 29),
                 species.harw = c(30, 31),
                 fun.final.felling = "harv.prob",
                 fun.thinning      = "thin.prob",
                 'BN2009',
                 'BBG2008', 'SBA.m2.ha', 'spp','pr.spru.ba', 'QMD.cm',
                 per.vol.harv = 0.83
                 )
biomass.Norway (tr = res$live,
                this.period = "t1") 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
