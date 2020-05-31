\name{FreeParProfile}
\alias{FreeParProfile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to plot the log-likelihood profile of a parameter
}
\description{
TO DO}
\usage{
FreeParProfile( filename,
                free.par = "initialization.B0",
                comp = NA, scaled = TRUE,
                combine.penalties = TRUE,
                add.total = FALSE,
                x.axis.title = "NA",
                prior.penalties.keywords = c("prior","cl"),
                legend.position = "right")
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
FreeParProfile(filename = system.file("extdata/Model 1", "Profile-Output.txt", package = "ProcessCASALoutput"),
               comp = NA,
               combine.penalties = TRUE,
               add.total = TRUE,
               x.axis.title = "B0",
               prior.penalties.keywords = c("prior", "cl", "average"))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
