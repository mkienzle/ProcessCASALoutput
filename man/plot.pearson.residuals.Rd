\name{plot.pearson.residuals}
\alias{plot.pearson.residuals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to plot Pearson's residuals
}
\description{
TO DO}
\usage{
plot.pearson.residuals(obj1, obj2, combine = FALSE, year.lab = 1990:2010, age.lab = seq(3,20,2)){

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
plot.pearson.residuals(obj1 = extract.fits(system.file("extdata/Model 1", "CASAL-MPDoutput.txt", package = "ProcessCASALoutput"))$wcsiTRLage,
                       year.lab = seq(1970, 2020,5),
                       age.lab = seq(0,30,2)))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
