\name{label_scientific10}
\alias{label_scientific10}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Formatting for x10^n 
}
\description{
Returns an expression list for numbers formatted as m x 10^n.
}
\usage{
label_scientific10(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

par(mar=c(4.5,6.5,1,1))
plot(c(1,2,3),c(1,2,3)*1e15,yaxt='n',ylab="",xlab=label_polar_angle()) #turn off ylabels and yaxis

axis(2,at=axTicks(2),label_scientific10(axTicks(2)),las=1)

mtext(text=label_angular_frequency(),side=2,line=5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
