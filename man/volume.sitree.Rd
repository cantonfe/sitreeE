\name{volume.sitree}
\alias{volume.sitree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Volume for sitree output for Norwegian conditions
}
\description{
It calculates volume following the Norwegian national forest inventory
equations for a trList or trListDead object
}
\usage{
volume.sitree(tr, plot.data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tr}{
a trListDead or trList object
}
  \item{plot.data}{
a list or data.frame containing at least a 'kom' and 'tree2ha'
column/element. kom is the kommune (municipality) code, and tree2ha
should be the expansion factor to go from tree to per ha basis.
}
}
\details{
It uses the volume.norway function to estimate the volume for all trees
with dbh.mm greater than 0. It returns NA when dbh.mm is 0 or lower.
tree2ha is included to facilitate the calculation of per ha values.
}
\value{
  It returns a data.table with columns for treeid, plot.id, dbh.mm,
  height.dm, kom, tree2ha, tree.sp, vol.w.tr.m3 (volume with bark in m3
  per tree), and vol.wo.tr.m3 (volume without bark in m3 per tree)
}

\author{
Clara Anton Fernandez
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
volume.sitree(tr = res$live, plot.data = res$plot.data)
}
