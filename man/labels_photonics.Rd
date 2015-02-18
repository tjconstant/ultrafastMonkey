\name{labels_photonics}
\alias{labels_photonics}
\alias{label_in_plane_wavevector}
\alias{label_angular_frequency}
\alias{label_polar_angle}
\alias{label_azimuthal_angle}
\alias{label_reflection}
\alias{label_transmission}
\alias{label_wavelength}
\alias{label_frequency}
\title{
Convenient Labelling for Photonics Graphs
}
\description{
Quick expression generation for axes labels, etc.
}
\usage{
label_in_plane_wavevector(subscript,si_prefix)
label_angular_frequency()
label_polar_angle(unit)
label_azimuthal_angle(unit)
label_reflection(subscript)
label_transmission(subscript)
label_wavelength(si_prefix)
label_frequency(si_prefix)
}

\arguments{
  \item{subscript}{
A charcter string for using in the relavent subscript of the expression.
}
  \item{si_prefix}{
A character string for the SI unit scale prefix. Greek letters may be typed, for example "mu" will display as the greek symbol mu. The default is "phantom()", which produces no prefix.
}
  \item{unit}{
Unit to use for angles. Default is degree, producing a degree symbol.  
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A R expression for use in labels and text.}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
plot(NA,NA,xlim=c(0,1),ylim=c(0,1))
text(0.5,1,label_angular_frequency())
text(0.5,0.9,label_azimuthal_angle())
text(0.5,0.8,label_in_plane_wavevector(subscript="gamma",si_prefix="c"))
text(0.5,0.7,label_polar_angle())
text(0.5,0.6,label_reflection("TM"))
text(0.5,0.5,label_wavelength(si_prefix="mu"))
text(0.5,0.4,label_frequency(si_prefix="G"))
}

