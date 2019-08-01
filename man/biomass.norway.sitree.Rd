\name{biomass.norway.sitree}
\alias{biomass.norway.sitree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates biomass for trees using the Norwegian biomass equations
}
\description{
Calculates biomass 
}
\usage{
biomass.norway.sitree(dbh.mm, height.dm, tree.sp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dbh.mm}{
Diameter at breast height in mm.
}
  \item{height.dm}{
Tree height in dm.
}
  \item{tree.sp}{
Tree species according to the Norwegian NFI. It will use
 \code{\link{biomass.spruce.M1988}} when tree.sp is 1, 2, 3, 21, or 29,
\code{\link{biomass.pine.M1988}} when tree.sp is 10, 11, or 20, and  \code{\link{biomass.birch.S2014}} otherwise.

}
}
\value{
Returns a data.table containing the 12 columns resulting from applying
the biomass functions.
}


\seealso{
\code{\link{biomass.birch.S2014}}, \code{\link{biomass.pine.M1988}}, \code{\link{biomass.spruce.M1988}}
}
\examples{
bio.nor <- biomass.norway.sitree (dbh.mm = c(50, 60, 100 ), height.dm = c(40, 60,
                                 80),
                       tree.sp = c(1, 10, 30))
				
}

