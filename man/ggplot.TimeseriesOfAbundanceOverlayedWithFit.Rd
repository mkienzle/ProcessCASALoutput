\name{ggplot.TimeseriesOfAbundanceOverlayedWithFit}
\alias{ggplot.TimeseriesOfAbundanceOverlayedWithFit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to plot the log-likelihood profile of a parameter
}
\description{
TO DO}
\usage{
ggplot.TimeseriesOfAbundanceOverlayedWithFit( datafile = NA,
                                              path = NA,
                                              AbundIndex.Name = NA, y.axis.lab = "", xlab = c(2000, 2018),
                                              ylim = c(0, 1200), increment = 1e2,
                                              title = "",
                                              x.axis.step = 5,
                                              add.line = TRUE)
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

# An example
ggplot.TimeseriesOfAbundanceOverlayedWithFit(
                                              datafile = system.file("extdata/Model 1", "CASAL-MPDoutput.txt", package = "ProcessCASALoutput"),
                                              path = ".",
                                              AbundIndex.Name = "wcsiTANbio",
                                              y.axis.lab = "Survey",
                                              xlab = c(1999, 2018),
                                              x.axis.step = 2,
                                              ylim = c(0,4000),
                                              increment = 1000,
                                              add.line = FALSE)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
