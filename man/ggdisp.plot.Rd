\name{ggdisp.plot}
\alias{ggdisp.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting of irregular spaced matrix data with aribitary transformations using ggplot
}
\description{
ggplot2 wrapper for disp.plot3. Plotting of irregular spaced matrix data with aribitary transformations using ggplot package.
}
\usage{
ggdisp.plot(x,y=NA,z=NA,fx=x,fy=y,midpoint=FALSE,col=c("black","white"))
}

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
  \item{fx}{
functional transformation of length(x)
}
  \item{fy}{
functional transformation of length(y)
}
  \item{midpoint}{
see ?poly.image
}
  \item{col}{
a vector of R colors for the gradient on the resulting image plot.
  }
  }
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

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


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

data(SPPdispersion)

x<-SPPdispersion$wavelength*1e9
y<-SPPdispersion$angle
z<-SPPdispersion$reflection

kx<-function(x,y) (2*pi/(x*1e-9))*sin(y*pi/180)
omega<-function(x,y) 2*pi*3e8/(x*1e-9)

ggdisp.plot(x,y,z)

ggdisp.plot(x,y,z,fx=kx,fy=omega,nx=200,ny=200,method="bicubic")

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
