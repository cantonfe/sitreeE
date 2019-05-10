\name{top.height}
\alias{top.height}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Top height of the n thickest trees
}
\description{
Average height of the n thickest trees per ha
}
\usage{
top.height(thickness, height, num.trees.per.ha, plot.id, plot.size.m2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{thickness}{
A vector with the thickness for every tree. Other variables can be used instead to thickness. This is only used to order the trees.
}
  \item{height}{
Height of the trees. 
}
  \item{num.trees.per.ha}{
Number of trees per ha that the top height should correspond to. E.g. 100 trees per ha.
}
  \item{plot.id}{
Vector with the plot.id to which every tree corresponds to.
}
  \item{plot.size.m2}{
Plot size in square meters. It can be either a single number if all plots have the same size of a vector of equal length as thickness, height, and plot.id with the corresponding plot size for each tree. 
}
}
\details{
thickness, height, and plot.id should have the same length, that is, one value per tree. 
}
\value{
It returns a data.frame with two columns: top.heights in the same units as height, and plot.id.
}

\examples{
top.height(thickness = runif(100, 10,40), height = runif(100, 12, 45), 
    num.trees.per.ha = 100, plot.id = 1, plot.size.m2 = 250)
}

