\name{ComputingMetastudyGMMObjective}
\alias{ComputingMetastudyGMMObjective}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ComputingMetastudyGMMObjective
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MetastudyGMMObjective_new(beta_theta,betap,cutoffs,symmetric,X,sigma,cluster_ID)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta_theta}{A vector whose entries equal the values for optimization of the mean and standard error of the distribution of the true treatment effects. Should be of type "matrix."
%%     ~~Describe \code{x} here~~
}
\item{betap}{A vector whose entries equal the values for optimization of the publica-
tion probability step values. This vector should be of the same size the vector cutoffs. Should be of type "matrix."
}
\item{cutoffs}{A strictly increasing vector whose entries are the thresholds for the steps of the publication probability p. Should be a column vector of size k × 1, for k being the number of thresholds. Should be of type "matrix."
}
\item{symmetric}{A dummy variable. symmetric = 1 imposes the assumption the publication probability p is symmetric around zero. By contrast, symmetric = 0 allows the publication probability to be asymmetric around zero. Should be of type "numeric."
}
\item{X}{The treatment effect estimates. Should be a column vector of size n × 1, for n being the number of estimates. Should be of type "matrix."
}
\item{sigma}{The associated standard errors of the estimates. Should be the same dimension as X. Should be of type "matrix."
}
\item{cluster_ID}{A vector that allows the reader to indicate whether any of the results are taken from the same study, in which case standard errors will be clustered by study, as in the minimum wage application discussed in Andrews and Kasy. Should be the same dimension as X, and entries should be assigned the same value if and only if they’re in the same cluster. Should be of type "matrix" for k>1, and "numeric" for k=1.
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
