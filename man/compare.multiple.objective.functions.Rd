\name{compare.multiple.objective.functions}
\alias{compare.multiple.objective.functions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function that compares Maximum Likelihood components between several CASAL models
}
\description{
TO DO}
\usage{
compare.multiple.objective.functions(filenames, models.names)
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
res = compare.multiple.objective.functions(
            filenames = c(system.file("extdata/MOdel 4", "CASAL-MPDoutput.txt", package = "ProcessCASALoutput"),
                          system.file("extdata/Model 5", "CASAL-MPDoutput.txt", package = "ProcessCASALoutput")),
                     models.names = c("Survey index of abundance",
                                      "CPUE index of abundance"))

print(res)


}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
