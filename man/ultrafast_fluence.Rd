\name{ultrafast_fluence}
\alias{ultrafast_fluence}

\title{
Calculation of Incident Fluence from a Pulsed Laser System
}
\description{
Calculation of the incident power per unit area (fluence) from a pulsed laser system given various parameters
}
\usage{
ultrafast_fluence(power_mW = 1, 
                  freq_khz = 1.05, 
                  spot_diameter_microns = 100, 
                  beam_angle_deg = 60)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{power_mW}{
Average power in mW
}
  \item{freq_khz}{
Repetition rate of the laser system
}
  \item{spot_diameter_microns}{
Spot diameter at normal incidence in microns
}
  \item{beam_angle_deg}{
Incident angle of beam on surface in degrees. Used to calulate ellipsity of beam spot
}
}
\details{

}
\value{
Returns Fluence in 3 common units of measurement. Also provides a warning in relation to graphene damage thresholds [1].
}
\references{
[1] Alexeev, E., Moger, J. & Hendry, E. Photo-induced doping and strain in exfoliated graphene. Appl. Phys. Lett. 103, 2011–2015 (2013).
}
\author{
Tom Constant
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ultrafast_fluence(power_mW = 0.3,
                  spot_diameter_microns = 200,
                  beam_angle_deg = 20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
