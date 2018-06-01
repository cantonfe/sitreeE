\name{biomass.birch.M1988}
\alias{biomass.birch.M1988}
\alias{biomass.pine.M1988}
\alias{biomass.spruce.M1988}

\title{
Marklund's biomass equations
}
\description{
Implements Marklund's (1988) biomass equations for above-ground biomass and Petersson and Ståhl (2006) for below-ground biomass.
}
\usage{
biomass.spruce.M1988(dbh.cm, H.m)
biomass.pine.M1988(dbh.cm, H.m)
biomass.birch.M1988(dbh.cm, H.m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dbh.cm}{
A vector with the dbh (diameter at breast height) of the trees, in cm.
}
  \item{H.m}{
A vector with the heights of the trees, in meters.
}
}

\value{
It returns a data.frame with the following biomass components in kg: living.branches, dead.branches, stem.wood, stump.roots, bark, usoil, rot1, rot2, and foliage.

}
\references{
Marklund, L. G. 1988. “Biomassafunktioner för tall, gran och björk i Sverige [Biomass functions for pine, spruce and birch in Sweden].” Report 45. Umeå, Sweden: Swedish University of Agricultural Sciences. Department of Forest Survey.
Petersson, Hans, and Göran Ståhl. 2006. “Functions for Below-Ground Biomass of Pinus Sylvestris, Picea Abies, Betula Pendula and Betula Pubescens in Sweden.” Scandinavian Journal of Forest Research 21 (S7): 84–93. 
}
\author{
Clara Anton Fernandez \email{caf@nibio.no}
}

\examples{
biomass.spruce.M1988(dbh.cm = c(10, 20), H.m = c(8, 12))

}

