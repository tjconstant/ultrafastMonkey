\name{read.asc}
\alias{read.asc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read in an asc spectra file.
}
\description{
Reads in a asc spectra file, including any acquisition parameters
}
\usage{
read.asc(file, acquisition.appended = T, quick_plot=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
The file to be read
}
  \item{acquisition.appended}{
Flag to include appened acquisition data to the resulting list. currently unfinished.
}
  \item{quick_plot}{
Flag to generate quick plot of data imported.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list object containing
\item{data}{a data.frame of the x,y data}
\item{details}{a character vector containing the acquisition parameters}
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
