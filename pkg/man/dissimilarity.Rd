\name{dissimilarity}
\alias{dissimilarity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate dissimilarity matrix
}
\description{
Given a data.frame() or matrix of n columns containing data, calculates the n by n dissimilarity matrix containing Kolmogorov-Smirnov distances for each data pair.
}
\usage{
dissimilarity(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
input data.frame() or matrix containing the data to be compared in columns
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
Vermeesch, P., 2013, Multi-sample comparison of detrital age distributions. Chemical Geology, v.341, 140-146. doi:10.1016/j.chemgeo.2013.01.010.
\url{http://www.ucl.ac.uk/~ucfbpve/mudisc/}
}
\author{
Martin Rittner
}
\note{
Based on code published by Pieter Vermeesch, see references.
See \code{\link{plotMDS}} for an example of use.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plotMDS}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
