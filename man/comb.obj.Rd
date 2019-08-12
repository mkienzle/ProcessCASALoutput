\name{comb.obj}
\alias{comb.obj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to combine CASAL objects
}
\description{
TO DO}
\usage{
comb.obj(obj1, obj2)
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

# combine the fit to 2 fishery dataset (one with selectivity until 2005 and one after 2005)
library(casal)
plot.MCMC.SSB(obj1 = extract.fits(system.file("extdata", "model2WithSel/CASAL-MPDoutput.txt", package = "ProcessCASALoutput"))$wcsiTRLagePre2005,
              obj2 = extract.fits(system.file("extdata", "model2WithSel/CASAL-MPDoutput.txt", package = "ProcessCASALoutput"))$wcsiTRLageFrom2005)


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
