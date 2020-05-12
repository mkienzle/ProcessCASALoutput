\name{plot.MCMC.posterior}
\alias{plot.MCMC.posterior}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to plot a time series of Spawning Stock Biomass (SSB) with 95\% confidence intervals estimated by Monte Carlo Markov Chain (MCMC).
}
\description{
TO DO}
\usage{
plot.MCMC.posterior(filename,
                    path = "",
                    label = "NA",
                    var = "NA",
                    x.axis.label = "NA")
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
# Plotting only the MCMC results
my.plot1 = plot.MCMC.posterior(filename = system.file("extdata", "quant.v1", package = "ProcessCASALoutput"),
                              var = "B0",
                              label = "Model XYZ",
                              x.axis.label = expression(B[0]),
                              xlim = c(6e4, 9e4),
                              x.breaks.major.steps = 1e4,
                              x.breaks.minor.steps = 5e3)
plot(my.plot1)

# Plotting the MCMC results and the prior (lognormal.prior only available for the moment)
my.plot2 = plot.MCMC.posterior(filename = system.file("extdata", "quant.v1", package = "ProcessCASALoutput"),
                               var = "B0",
                               label = "Model XYZ",
                               x.axis.label = expression(B[0]),
                               xlim = c(2e4, 12e4),
                               x.breaks.major.steps = 2e4,
                               x.breaks.minor.steps = 1e4,
                               lognormal.prior = c(log(7e4), 0.2))

plot(my.plot2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
