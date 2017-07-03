\name{lorey.height}
\alias{lorey.height}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lorey's height
}
\description{
Calculates Lorey's height (mean height weigted by basal area). If
group.id is not NULL, it will calculate Lorey's height for each group. 
}
\usage{
lorey.height(BA, height, group.id = NULL)
}
\arguments{
  \item{BA}{
A vector with the basal areas of the trees
}
  \item{height}{
A vector with the height of the trees
}
  \item{group.id}{
An optional  vector with a grouping variable.
}
}

\value{
If a grouping variable is provided it returns a data frame with two
columns (group.id, and lorey.height). If a grouping variable is not
provided it will return the Lorey's height. 
}

\author{
Clara Anton Fernandez (caf@nibio.no)
}

\seealso{
\code{\link{height.of.X.tallest.trees}}
}
\examples{
BA <- pi*(tr$dbh/2)^2
lorey.height(BA, tr$height)
lorey.height(BA, tr$height, tr$plot.id)
}
