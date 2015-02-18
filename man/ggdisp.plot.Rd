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

library(fields)

library(ggplot2)

data(SPPdispersion) #load example dataset

wavelength<-SPPdispersion$wavelength

angle<-SPPdispersion$angle

reflection<-SPPdispersion$reflection

ggdisp.plot(wavelength,angle,reflection) #plot data

kx<-(2*pi/wavelength)*sin(angle*pi/180)#transformation to dispersion plots
omega<-2*pi*3e8/wavelength

ggdisp.plot(wavelength,angle,reflection,kx,omega) #plot transformed data

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
