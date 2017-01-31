\name{stand.comp.func}
\alias{stand.comp.func}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stand Composition by species group
}
\description{
Stand composition by species group
}
\usage{
stand.comp.func(perc.con = NA, perc.dec = NA, perc.spruce = NA, perc.temp.dec = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{perc.con}{
Percentatge of conifers in the plot. 
}
  \item{perc.dec}{
Percentatge of deciduous in the plot. 
}
  \item{perc.spruce}{
Percentatge of spruce in the plot
}
  \item{perc.temp.dec}{
%%     ~~Describe \code{perc.temp.dec} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (perc.con = NA, perc.dec = NA, perc.spruce = NA, perc.temp.dec = NA) 
{
    if (all(is.na(c(perc.con, perc.dec, perc.temp.dec)))) {
        stand.comp.class <- NA
    }
    else {
        stand.comp.class <- ifelse(perc.con >= 75 & perc.dec < 
            25 & perc.spruce < 50, 1, ifelse(perc.con >= 75 & 
            perc.dec < 25 & perc.spruce >= 50, 2, ifelse(perc.con >= 
            50 & perc.con < 75 & perc.dec >= 25 & perc.dec <= 
            50 & perc.temp.dec == 0, 3, ifelse(perc.con >= 50 & 
            perc.con < 75 & perc.dec >= 25 & perc.dec < 50 & 
            perc.temp.dec > 0 & perc.temp.dec < 25, 4, ifelse(perc.con >= 
            50 & perc.con < 75 & perc.dec >= 25 & perc.dec < 
            50 & perc.temp.dec >= 25 & perc.temp.dec < 50, 5, 
            ifelse(perc.con < 50 & perc.dec >= 50 & perc.temp.dec == 
                0, 4, ifelse(perc.con < 50 & perc.dec >= 50 & 
                perc.temp.dec > 0 & perc.temp.dec < 25, 5, ifelse(perc.con < 
                50 & perc.dec >= 50 & perc.temp.dec >= 25, 6, 
                NA))))))))
    }
    return(stand.comp.class)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
