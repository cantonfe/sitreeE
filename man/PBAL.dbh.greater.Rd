\name{PBAL.dbh.greater}
\alias{PBAL.dbh.greater}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Basal area of larger trees which are at least X cm larger than the tree
of interest 
}
\description{
Calculates the basal area (in cm2 if dbh is in mm) of trees that are at
least X (in the same units as dbh) larger than the tree of interest for a list of trees.
}
\usage{
PBAL.dbh.greater(dbh.mm, dbh.mm.diff)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dbh.mm}{
a vector of dbh in mm
}
  \item{dbh.mm.diff}{
minimum dbh difference between the tree and the tree of interest to be considered
}
}
\details{
It estimates the basal area (in cm2) of trees that are at least 
dbh.mm.diff mm larger than the tree of interest.
}
\value{
It returns a list with the basal area of larger trees.
}

\author{
Cara Antón-Fernández (email: caf@nibio.no)
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{PBAL}}
}
\examples{
PBAL.dbh.greater(c(100,89,51,74,4), 3)

aggregate(dbh ~ plot.id, data = tr, FUN = PBAL.dbh.greater, dbh.mm.diff = 2)

}