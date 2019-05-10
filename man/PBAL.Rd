\name{PBAL}
\alias{PBAL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Basal area of larger trees
}
\description{
It calculates the basal area of larger trees for a plot.
}
\usage{
PBAL(BA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BA}{
A vector of trees basal area
}
}

\value{
It returns a vector with the sum of the basal areas of larger
trees. Trees with similar BA are not considered larger.
}


\examples{
PBAL(c(1,2,3,4,4))
}

