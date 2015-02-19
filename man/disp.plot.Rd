\name{disp.plot}
\alias{disp.plot}
\alias{nothing_x}
\alias{nothing_y}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting of irregular spaced matrix data with aribitary transformations.
}
\description{
Polygon 2D plotting of irregular spaced matrix data with aribitary transformations.
}
\usage{
disp.plot(x,y,z,fx=nothing_x,fy=nothing_y,nx=length(unique(x)),ny=length(unique(y)),method="bilinear",...)
nothing_x(x,y)
nothing_y(x,y)
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
function of x and y returning the functional transformation of the x-axis
}
  \item{fy}{
function of x and y returning the functional transformation of the y-axis
}
  \item{nx,ny}{
The square image will be constructed from nx by ny polygons. If nx and ny are equal to the defaults, length(unique(x)) and length(unique(y)), the plot produced will be a standard (uninterpolated) colorplot. Higher values of nx and ny will produce smoother plots.
}
  \item{method}{
  Specifies the interpolation method to use. Default is 'bilinear' using akima::interp, you can also use 'bicubic' (from akima::bicubic.grid).
  }
  \item{...}{
  The usual arguments to the image function as x,y,or z or as a list with x,y,z as components. One can also include a breaks argument for an unequal color scale with color scale boundaries at the breaks (see example in the image.plot documentation).
  }
}
\details{
In this function fx and fy are passed as functions, not raw vectors. See the examples for how to use this method to plot dispersion.

nothing_x(x,y) and nothing_y(x,y) are the default functions for transformations, and do nothing (i.e. return x and y respectivley)
}
\value{
Returns an image plot of the data
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Tom Constant
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[fields]{image.plot}},
\code{\link[akima]{interp}},
\code{\link[akima]{bicubic.grid}}
}
\examples{
data(SPPdispersion)

x<-SPPdispersion$wavelength*1e9
y<-SPPdispersion$angle
z<-SPPdispersion$reflection

kx<-function(x,y) (2*pi/(x*1e-9))*sin(y*pi/180)
omega<-function(x,y) 2*pi*3e8/(x*1e-9)

disp.plot(x,y,z)

disp.plot(x,y,z,fx=kx,fy=omega,nx=200,ny=200,method="bicubic")

# Polar Coordinates example

# Edit SPPdisperison to vary from wavelength from 0 to 400 and angle 0 to 360 degrees
# Note, apart from the example, theres absolutley no reason to do this

x <- SPPdispersion$wavelength*1e9-400
y <- 359*(SPPdispersion$angle-min(SPPdispersion$angle))/(max(SPPdispersion$angle)-min(SPPdispersion$angle))
z <- SPPdispersion$reflection

# Polar Coordinate Transform
fx <- function(x,y) x*sin(y*pi/180)
fy <- function(x,y) x*cos(y*pi/180)

disp.plot(x,y,z,fx,fy,nx=300,ny=300,method = "bicubic")
}

