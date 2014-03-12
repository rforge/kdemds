\name{plotMDS}
\alias{plotMDS}
% \alias{plotmapMDS}
% \alias{plotmapPCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot MDS maps
}
\description{
Plotting function for multi-dimensional scaling (MDS) maps
}
\usage{
plotMDS(mds, diss, col = "area", sym = "", nearest = TRUE, labels = TRUE, symbols = TRUE)
% plotmapMDS(mds, diss)
% plotmapPCA(conf, diss)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mds}{
data.frame() or matrix with at least 2 columns representing x and y coordinates calculated by cmdscale() or isoMDS(). mds can be: data.frame(x,y,any,further,columns,...)
}
  \item{diss}{
(dis-)similarity matrix of samples
}
  \item{col}{
string giving the name of the column in mds to be used for colour scale
}
  \item{sym}{
string giving the name of column in mds to be used for symbol scale
}
  \item{nearest}{
boolean - plot lines connecting nearest neighbours?
}
  \item{labels}{
boolean - plot data labels (taken from row.names of mds)?
}
  \item{symbols}{
boolean - plot data points (useful if plotting only labels)?
}
}
\details{
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
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
