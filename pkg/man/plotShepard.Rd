\name{plotShepard}
\alias{plotShepard}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a Shepard plot of calculated MDS map
}
\description{
Creates a Shepard plot and calculates the stress factor for an MDS map.
}
\usage{
plotShepard(mds, diss, xlab = "dissimilarity", ylab = "distance", title = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mds}{
data.frame() or matrix with at least 2 columns representing x and y coordinates calculated by cmdscale() or isoMDS(). mds can be: data.frame(x,y,any,further,columns,...)
}
  \item{diss}{
(dis-)similarity matrix of samples
}
  \item{xlab}{
optional string overriding x-axis label
}
  \item{ylab}{
optional string overriding y-axis label
}
  \item{title}{
optional string giving plot title
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
\author{
Martin Rittner
}
\note{
%%  ~~further notes~~
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
