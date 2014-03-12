\name{kde}
\alias{kde}
\alias{optimal_bw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gaussian kernel density estimator
}
\description{
State-of-the-art gaussian kernel density estimator for one-dimensional data; The estimator does not use the commonly employed 'gaussian rule of thumb'. As a result it outperforms many plug-in methods on multimodal densities with widely separated modes.
}
\usage{
kde(data, n, MIN, MAX, bandwidth)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a vector of data from which the density estimate is constructed
}
  \item{n}{
the number of mesh points used in the uniform discretization of the interval [MIN, MAX]; n has to be a power of two; if n is not a power of two, then n is rounded up to the next power of two; the default value of n is n=2^12
}
  \item{MIN}{
%%     ~~Describe \code{MIN} here~~
}
  \item{MAX}{
defines the interval [MIN,MAX] on which the density estimate is constructed the default values of MIN and MAX are:
MIN=min(data)-Range/10 and MAX=max(data)+Range/10, where Range=max(data)-min(data)
}
  \item{bandwidth}{
optional. Manual override of automatically calculated bandwidth
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
matrix 'out' of with two rows of length 'n', where out[2,] are the density values on the mesh out[1,]
}
\references{
Z. I. Botev, J. F. Grotowski and D. P. Kroese
"Kernel Density Estimation Via Diffusion"
Annals of Statistics, 2010, Volume 38, Number 5, Pages 2916-2957
for questions email: botev@maths.uq.edu.au
}
\author{
Martin Rittner
}
\note{
\code{optimal_bw(data, n)} is a "dirty hack", a stripped copy of kde(), performing the same calculations but returning the calculated optimal bandwidth instead of the kde.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plotKDE}}
}
\examples{
data=c(rnorm(10^3),rnorm(10^3)*2+30)
d=kde(data)
plot(d[1,],d[2,],type='l',xlab='x',ylab='density f(x)')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
