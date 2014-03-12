\name{plotKDE}
\alias{plotKDE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot KDEs of age distributions
}
\description{
Flexible wrapper to plot KDEs of age populations for geochronology.
}
\usage{
plotKDE(data, title, limits = c(0, 3000), breaks = NA, bandwidth = NULL, fcolour = NA, splitat = NA, plot = names(data), periods = FALSE, classes = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
input ages, either as a data.frame() with samples in separate columns for creating stacked KDEs, or for a single KDE plot, a numeric vector or a data.frame() with one column (or only one selected, see "plot" below)
}
  \item{title}{
optional string giving the title for single KDE plot, or vector of strings, same length as (selected columns of) data
}
  \item{limits}{
optional vector of length 2 giving the age range to plot, or vector of length 4 giving the age ranges for the two split plots (see "splitat" below)
}
  \item{breaks}{
tba, x-axis breaks, optional
}
  \item{bandwidth}{
optional bandwidth overriding automatic bandwidth calculation. By default, one common bandwidth based on individual "optimal" bandwidths is chosen for all plots, if several are compared. Half bandwidth is used for younger ages in split plot; vector of length 2 to override split plots separately. Set to -1 or FALSE to use calculated "optimal" bandwidth for each plot.
}
  \item{fcolour}{
optional fill colour, either single colour to be applied to all KDEs, or vector of colour definitions of same length as classes.
}
  \item{splitat}{
optional age to split plot at. Half-plots will occupy same amount of space. Limits of length 4 will override splitat.
}
  \item{plot}{
optional string vector of column names to select from data. Other columns will be ignored. If given, title and classes must be of same length as resulting selection.
}
  \item{periods}{
optional - plot major geological boundaries for guidance?
}
  \item{classes}{
optinal vector of same length as (selected) data, giving a classification for each data column, determining fill colour. If unspecified, colours are chosen randomly, unless given in fcolour. Set classes=-1 to plot all samples in the same colour (setable by fcolour).
}
	\item{hist}{
optionally underlie a histogram? Maximum count will be scaled to max height of KDE, binwidth is calculated optimal or given kernel bandwidth.
}
	\item{markers}{
optional, either "dash" or "circle" - plot markers at x-position (age) of data.
}
	\item{order}{
optional, have multiple KDEs stacked in alphabetical order of names(data) (default), or in order as entered (order=FALSE)?
}
	\item{logx}{
	optional, should x-axis (ages) be plotted in log-scale?
	}
	\item{method}{
	optional, method to be used for KDE calculation. Possible values: "botev" for use of Vermeesch, 2012 (following Botev et al., 2010), "R" for use of "standard" density() function of package stats, in which case optimal bandwidth is calculated following Sircombe & Hazelton, 2004.
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
%%  ~~further notes~~
Currently, age markers do not work well with stacked plots, only single KDE plots.
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
