\name{plot.MCMC.SSB}
\alias{plot.MCMC.SSB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to plot a time series of Spawning Stock Biomass (SSB) with 95\% confidence intervals estimated by Monte Carlo Markov Chain (MCMC).
}
\description{
TO DO}
\usage{
plot.MCMC.SSB(filename ="", path = "", mgt.ref.points = TRUE, ref.points.label.x.axis = 1975)
}
\arguments{
  \item{x}{
TODO
%%     ~~Describe \code{x} here~~
}
}
\details{
TODO
%%  ~~ If necessary, more details than the description above ~~
}
\value{
TODO
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
TODO
%% ~put references to the literature/web site here ~
}
\author{
TODO
%%  ~~who you are~~
}
\note{
TODO
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
TODO
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# Plot MCMC
plot.MCMC.SSB(mcmcfilename = system.file("extdata", "quant.v1", package = "ProcessCASALoutput"),
              path = "",
              mgt.ref.points = TRUE,
              ref.points.label.x.axis = 1978)

# Plot MCMC projections
plot.MCMC.SSB(mcmcfilename = system.file("extdata", "proj-FC500YCSR2006-2015.dat", package = "ProcessCASALoutput"),
              projection.from = 2018)

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
