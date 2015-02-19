\name{ggdisp.plot}
\alias{ggdisp.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting of irregular spaced matrix data with aribitary transformations using ggplot
}
\description{
Plotting of irregular spaced matrix data with aribitary transformations using ggplot package.
}
\usage{

ggdisp.plot(x, 
            y,
            z,
            fx=nothing_x,
            fy=nothing_y,
            nx=length(unique(x)),
            ny=length(unique(y)),
            method="bilinear",
            midpoint=FALSE,
            return_data=F,
            ...)
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
This function works much the same as disp.plot, however in order to assign labels to each polygon set, so that geom_tile can work correctly, tweaks were needed to be made to poly.image(). These are included in the code. 
}
\value{
Either a ggplot object or the dataset required to plot (depending on return_data flag)
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


\seealso{
\code{\link{disp.plot}}
}
\examples{

data(SPPdispersion)

x<-SPPdispersion$wavelength*1e9
y<-SPPdispersion$angle
z<-SPPdispersion$reflection

kx<-function(x,y) (2*pi/(x*1e-9))*sin(y*pi/180)
omega<-function(x,y) 2*pi*3e8/(x*1e-9)

# Show plots
ggdisp.plot(x,y,z)

ggdisp.plot(x,y,z,fx=kx,fy=omega,nx=200,ny=200,method="bicubic")

# Example for using facets

p1<-data.frame(ggdisp.plot(x,y,z,fx=kx,fy=omega,return_data = T),label="Frequency")
p2<-data.frame(ggdisp.plot(x,y,z,fx=kx,return_data = T),label="Angle")

p <- ggplot(rbind(p1,p2), aes(x=xps, y=yps))+
  geom_polygon(aes(fill=zps,color=zps,group=ids))+
  xlab("x")+
  ylab("y")+facet_grid(label~.,scale="free")

print(p)
}

