\name{Clustered_covariance_estimate}
\alias{Clustered_covariance_estimate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Clustered_covariance_estimate
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Clustered_covariance_estimate(g,cluster_index)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{A vector accroding to which clustered standard errors are calculated. Should be of type "matrix."
%%     ~~Describe \code{x} here~~
}
\item{cluster_index}{A vector that allows the reader to indicate whether any of the results are taken from the same study, in which case standard errors will be clustered by study, as in the minimum wage application discussed in Andrews and Kasy. Should be of column vecotr with number of entries equal to the number of rows in g, and entries should be assigned the same value if and only if they’re in the same cluster. Should be of type "matrix."
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
\author{Brit Sharoni
%%  ~~who you are~~
}
\note{Part of the functions needed in order to replicate the results from Andrews and Kasy.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
