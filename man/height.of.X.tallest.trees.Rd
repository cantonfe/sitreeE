\name{height.of.X.tallest.trees}
\alias{height.of.X.tallest.trees}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean height of X tallest trees
}
\description{
It calculates the mean height of the X tallest trees by grouping
variable (e.g. the plot ID)
}
\usage{
height.of.X.tallest.trees(height, uplotID, num.trees)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{height}{
A vector with heights
}
  \item{uplotID}{
A vector with the grouping variable, most often this would be the plot ID.
}
  \item{num.trees}{
Number of trees used to calculate the mean height. 
}
}

\value{
It returns a data.frame with two columns containing the uplotID and the
mean height of the X tallest trees.
}

\author{
  Clara Antón Fernández (caf@nibio.no)
}
\note{
This function can be used to calculate the average of the X largest
values of any variable grouped by a grouping variable. It is mostly a
wrapper for \code{aggregate}.
}


\seealso{
\code{\link{lorey.height}}
}
\examples{
height.of.X.tallest.trees(height = tr$height,
                               uplotID = tr$plot.id, 5)
                               
}
