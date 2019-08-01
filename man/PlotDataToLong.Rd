\name{PlotDataToLong}
\alias{PlotDataToLong}

\title{
Convert Plot data to a data.frame/data.table format
}
\description{
It attemps to convert the plot level information to a data.frame/data.list format. For example, if management is an element of the plot list (pd) it will melt it to a long format.
}
\usage{
PlotDataToLong(pd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pd}{
A list with plot information. It especifically looks for two elements 'stand.age.years' and 'management'.
}
}

\value{
It returns a data.table with all vector from pd as columns, and dataframes melted.
}


\examples{
result.sitree <- sitree (tree.df   = tr,stand.df  = fl,
functions = list(fn.growth     ='grow.dbhinc.hgtinc',
fn.mort       ='mort.B2007',
fn.recr       ='recr.BBG2008',
fn.management ='management.prob',
fn.tree.removal ='mng.tree.removal',
fn.modif      = NULL,
fn.prep.common.vars ='prep.common.vars.fun'),
n.periods = 5,
period.length = 5,mng.options = NA,
print.comments = FALSE,
fn.dbh.inc = "dbhi.BN2009",
fn.hgt.inc =  "height.korf",
species.spruce = c(1, 2, 3),
species.pine = c(10, 11, 20, 21, 29),species.harw = c(30, 31),
fun.final.felling = "harv.prob",fun.thinning      =
"thin.prob",'BN2009','BBG2008',
'SBA.m2.ha','spp','pr.spru.ba','QMD.cm',per.vol.harv = 0.83)

PlotDataToLong(result.sitree$plot.data)

}


