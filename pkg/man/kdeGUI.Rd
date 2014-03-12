\name{kdeGUI}
\alias{kdeGUI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper GUI for plotKDE()
}
\description{
Helper function displaying a simple GUI for use with plotKDE()
}
\usage{
kdeGUI(toolkit="tcltk")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{toolkit}{
GUI toolkit to be used. Currently, "tcltk" seems to work on all platforms, "RGtk2" is difficult to install on Mac.
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
}
\author{
Martin Rittner
}
\note{
%%  ~~further notes~~
Tcl/tk seems to work on most platforms.
	Windows: install R, RStudio, Strawberry Perl and Tcl/Tk before installing required R packages
  Mac: R, RStudio, packages - check http://r.research.att.com/ for R updates and to try installing Gtk2
  Linux: R, RStudio, packages
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
