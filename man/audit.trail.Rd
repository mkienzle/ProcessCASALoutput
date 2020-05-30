\name{audit.trail}
\alias{audit.trail}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function that compares some Maximum Likelihood quantities estimated by CASAL different CASAL models
}
\description{
TO DO}
\usage{
audit.trail(models.path, models.name)
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

# What is the effect of updating (in turn) data in a CASAL model
at.res = audit.trail(models.path = c(system.file("extdata/MOdel 1", "CASAL-MPDoutput.txt", package = "ProcessCASALoutput"),
                                     system.file("extdata/Model 2", "CASAL-MPDoutput.txt", package = "ProcessCASALoutput"),
                                     system.file("extdata/Model 3", "CASAL-MPDoutput.txt", package = "ProcessCASALoutput")),
                     models.name = c("LIN7_M=0.18+NewCatchTS",
                                     "+TrawlCPUE(deltalog)",
                                     "+ScientificSurveyIndex"))

print(at.res)

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
