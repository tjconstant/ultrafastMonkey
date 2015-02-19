\name{disp.grid}
\alias{disp.grid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper function for akima interpolations
}
\description{
This function takes standard long dataset expected by the package's image.plot based functions and returns the appropriate interpolated matrices for the xyz values. Mostly used internally in disp.plot and ggdisp.plot.
}
\usage{
disp.grid(x, 
          y, 
          z, 
          nx = length(unique(x)), 
          ny = length(unique(y)), 
          method = "bilinear")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x vector
}
  \item{y}{
y vector
}
  \item{z}{
z vector
}
  \item{nx, ny}{
The square image will be constructed from nx by ny polygons. If nx and ny are equal to the defaults, length(unique(x)) and length(unique(y)), the plot produced will be a standard (uninterpolated) colorplot. Higher values of nx and ny will produce smoother plots.
}
  \item{method}{
  Specifies the interpolation method to use. Default is 'bilinear' using akima::interp, you can also use 'bicubic' (from akima::bicubic.grid).
}

}
\details{

}
\value{
returns a list
\item{x}{a nx by ny matrix of interpolated x values}
\item{y}{a nx by ny matrix of interpolated y values}
\item{z}{a nx by ny matrix of interpolated z values}
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[akima]{interp}},
\code{\link[akima]{bicubic.grid}}
}
\examples{
data(SPPdispersion)
a<-disp.grid( SPPdispersion$wavelength*1e9,
              SPPdispersion$angle,
              SPPdispersion$reflection)
image(a$x)
image(a$z)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
