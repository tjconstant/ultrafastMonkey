\name{disp.plot}
\alias{disp.plot}
\alias{nothing_x}
\alias{nothing_y}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting of irregular spaced matrix data with aribitary transformations. New Super-Experimental version.
}
\description{
New Super-Experimental disp.plot3 function including gridded bivariate interpolation for irregular data from the akima package.
}
\usage{
disp.plot6(x,y,z,fx=nothing_x,fy=nothing_y,nx=length(unique(x)),ny=length(unique(y)),method="bilinear",...)
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
The square image will be constructed from nx by ny polygons. If nx and ny are equal to the defaults, length(unique(x)) and length(unique(y)), the plot produced will be identical to disp.plot3. Higher values of nx and ny will produce smoother plots.
}
  \item{method}{
  Specifies the interpolation method to use. Default is 'bilinear' using akima::interp, you can also use 'bicubic' (from akima::bicubic.grid).
  }
  \item{...}{
  The usual arguments to the image function as x,y,or z or as a list with x,y,z as components. One can also include a breaks argument for an unequal color scale with color scale boundaries at the breaks (see example in the image.plot documentation).
  }
}
\details{
A new version of the disp.plot function for plotting non-linearlly spaced data, with the addition of bivariate smoothing of the dataset. Experimental!

Because the smoothing function is always called, disp.plot4 is x5 times slower than disp.plot3 when producing the same image.

Also note, the use of fx and fy is different in this case. In this function fx and fy are passed as functions, not raw vectors. See the examples for how to use this method to plot dispersion.

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

disp.plot6(x,y,z)

disp.plot6(x,y,z,fx=kx,fy=omega,nx=200,ny=200,method="bicubic")
}

