\name{four_wave_mixing}
\alias{four_wave_mixing}
Calculation of Four-Wave Mixing Parameters
\title{
Function to calculate various four wave mixing parameters
}
\description{
This function will attempt to calculate incident beams wavelengths lambda.1 and lambda.2, the difference-frequency between these wavelengths, the generated four-wave mixed wavelength and the angles of emmsison. 

The function is smart enough to calculate any paramters missing in the call providing sufficent other parameters have been specified.

The proccess assumes the nonlinear process:
2f1-f2=f3

correspondingly;
(2/lambda.1)-(1/lambda.2) = (1/lambda.3)
}
\usage{
four_wave_mixing(lambda.1 = NA, lambda.2 = NA, lambda.3 = NA, difference.frequency = NA, theta.1 = NA, theta.2 = NA, theta.3 = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lambda.1}{
wavelength 1 [nm]
}
  \item{lambda.2}{
wavelength 2 [nm]
}
  \item{lambda.3}{
wavelength 3 [nm]
}
  \item{difference.frequency}{
difference frequency [THz]
}
  \item{theta.1}{
angle of beam 1 [deg]
}
  \item{theta.2}{
angle of beam 2 [deg]
}
  \item{theta.3}{
angle of beam 3 in degrees
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns a data.frame of parameters
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
tjc
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
four_wave_mixing(lambda.3=550,difference.frequency = 1:10,theta.1 = 20,theta.2 = 40)

four_wave_mixing(lambda.1 = 550:560,lambda.2 = 600,theta.1 = 1,theta.2 = 2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
