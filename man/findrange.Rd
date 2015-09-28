\name{findrange}
\alias{findrange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
findrange(xx, fstart, fend, lead = 0, trail = 0, nthstart = 1, nthend = 1, val = F, strict = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xx}{
%%     ~~Describe \code{xx} here~~
}
  \item{fstart}{
%%     ~~Describe \code{fstart} here~~
}
  \item{fend}{
%%     ~~Describe \code{fend} here~~
}
  \item{lead}{
%%     ~~Describe \code{lead} here~~
}
  \item{trail}{
%%     ~~Describe \code{trail} here~~
}
  \item{nthstart}{
%%     ~~Describe \code{nthstart} here~~
}
  \item{nthend}{
%%     ~~Describe \code{nthend} here~~
}
  \item{val}{
%%     ~~Describe \code{val} here~~
}
  \item{strict}{
%%     ~~Describe \code{strict} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (xx, fstart, fend, lead = 0, trail = 0, nthstart = 1, 
    nthend = 1, val = F, strict = F, ...) 
{
    lead <- sort(lead)
    trail <- sort(trail)
    ptstart <- which(stout <- fstart(xx, ...))[nthstart]
    if (is.na(ptstart)) 
        return(NULL)
    ptend <- which((stend <- fend(xx, ptstart, ...))[ptstart:length(stout)])[nthend] + 
        ptstart - 1
    if (is.na(ptend)) 
        if (strict) 
            return(NULL)
        else ptend <- length(stout)
    leadidx <- lead + ptstart
    trailidx <- trail + ptend
    out <- sort(unique(c(leadidx, (max(leadidx) + 1):(min(trailidx) - 
        1), trailidx)))
    if (any(is.na(out) | out < 1)) 
        if (strict) 
            return(NULL)
    return(na.omit(out[out > 0]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
