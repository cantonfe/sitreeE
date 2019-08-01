\name{biomass.M1988}
\alias{biomass.M1988}
\title{
Biomass functions of Marklund (1988) and Petterson and Sthål (2006)
}
\description{
It implements Marklund (1988) -G20, G12, G16, G8, G5, G26, G31, G28- and
Petterson & Sthål (2006) equation for roots < 2mm. 
}
\usage{
biomass.M1988(tr, spp, this.period)
}
\arguments{
  \item{tr}{
A trList or trListDead object.
}
  \item{spp}{
A vector with the species for each tree in the tr object. The species
can be "spruce", "pine", "birch", or "other" (which is treated as birch).
}
  \item{this.period}{
The period for which biomass should be calculated, e.g. "t1".
}
}

\value{
It returns a data frame with columns for living branches, dead branches,
stem, stump, bark, stump roots, roots1, roots2, and foliage. Units are kg. 
}
\references{
  Marklund, L. G. 1988. “Biomassafunktioner för tall, gran och björk i Sverige (Biomass functions for pine, spruce and birch in Sweden).” Report 45. Umeå, Sweden: Swedish University of Agricultural Sciences. Department of Forest Survey. 

Petersson, Hans, and Göran Ståhl. 2006. “Functions for Below-Ground
Biomass of Pinus Sylvestris, Picea Abies, Betula Pendula and Betula
Pubescens in Sweden.” Scandinavian Journal of Forest Research 21 (S7):
84–93. doi:10.1080/14004080500486864.

}
\author{
Clara Anton Fernandez (caf@nibio.no)
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

spp <- sp.classification(tree.sp = tr$tree.sp,
                         species.spruce = c(1, 2, 3),
                         species.pine = c(10, 11, 20, 21, 29),
                         species.harw = c(30, 31)
                         )

biomass.kg <- biomass.M1988(res$live, spp , this.period = "t1")

}
