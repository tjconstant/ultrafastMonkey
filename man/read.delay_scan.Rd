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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (filename, time_max = 1, zero_offset = 1:10, quick_plot = T) 
{
    data <- read.table(filename)$V1
    time <- seq(0, time_max, , length(read.table(filename)$V1))
    data_offset <- data - mean(data[zero_offset])
    if (quick_plot) {
        plot(time, data_offset, xlab = "time (ps)", pch = 16)
        grid()
        lines(time, data_offset)
    }
    return(data.frame(time, data_offset))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
