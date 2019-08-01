\name{biomass.sitree}
\alias{biomass.sitree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Biomass for live, dead, or removed trees using Norwegian biomass functions
}
\description{
Calculates biomass for trees using the Norwegian biomass equations. 
}
\usage{
biomass.sitree(tr, plot.data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{
A trList or trListDead object.
}
  \item{plot.data}{
A data.frame or list with plot information. It should have at least 
plot.id and tree2ha.
}
}

\value{
Returns a data.table containing the 12 columns resulting from applying
the biomass functions.
}
\author{
Clara Anton Fernandez \email{caf@nibio.no}
}

\seealso{
 \code{\link{biomass.birch.S2014}}, \code{\link{biomass.birch.M1988}},
 \code{\link{biomass.pine.M1988}}, \code{\link{biomass.spruce.M1988}}, \code{\link{biomass.norway.sitree}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
result.sitree <- sitree (tree.df   = stand.west.tr,
                           stand.df  = stand.west.st,
                           functions = list(
                             fn.growth     = 'grow.dbhinc.hgtinc',
                             fn.mort       = 'mort.B2007',
                             fn.recr       = 'recr.BBG2008',
                             fn.management = NULL,
                             fn.tree.removal = NULL,
                             fn.modif      = NULL, 
                             fn.prep.common.vars = 'prep.common.vars.fun'
                           ),
                           n.periods = 20,
                           period.length = 5,
                           mng.options = NA,
                           print.comments = FALSE,
                           fn.dbh.inc = "dbhi.BN2009",
                           fn.hgt.inc =  "height.korf", 
                           species.spruce = c(1, 2, 3),
                           species.pine = c(10, 11, 20, 21, 29),
                           species.harw = c(30, 31),
                           'BN2009',
                           'BBG2008', 'SBA.m2.ha', 'spp','pr.spru.ba', 'QMD.cm'
                         )
## The function is currently defined as
biom <- biomass.sitree(tr = result.sitree$live, plot.data = result.sitree$plot.data)
biom
}
