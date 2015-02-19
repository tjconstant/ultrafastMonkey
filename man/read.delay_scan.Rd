\name{read.delay_scan}
\alias{read.delay_scan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quick analysis and plot of time dynamics
}
\description{
Takes an average time dynamics dataset and returns a dataframe and optionally plots data.
}
\usage{
read.delay_scan(filename, time_max = 1, zero_offset = 1:10, quick_plot = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
The filename of the averaged output file from labview
}
  \item{time_max}{
maximum time in the scan
}
  \item{zero_offset}{
Add a zero offset if required
}
  \item{quick_plot}{
Toggle a quick plot
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns a dataframe of time vs signal
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

fake_data<-(3+rnorm(1:400,sd=5e-2)+(exp(-((1:400)-100)^2/20^2)))

# make a fake delay_scan temp file for use in example
tf<-tempfile()
writeLines(text = as.character(fake_data),tf)

#use read.delay_scan()
fake_delay_scan<-read.delay_scan(tf,time_max = 4,time_offset = 1)

#delete temp file
unlink(tf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
