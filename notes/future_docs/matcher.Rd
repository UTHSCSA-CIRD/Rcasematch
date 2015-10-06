\name{matcher}
\alias{matcher}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
matcher(cases, controls, groupby, patient = "patient_num", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cases}{
%%     ~~Describe \code{cases} here~~
}
  \item{controls}{
%%     ~~Describe \code{controls} here~~
}
  \item{groupby}{
%%     ~~Describe \code{groupby} here~~
}
  \item{patient}{
%%     ~~Describe \code{patient} here~~
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
function (cases, controls, groupby, patient = "patient_num", 
    ...) 
{
    matches <- unique(cases[, c(patient, groupby)])
    nm <- nrow(matches)
    matches$ctrl_pn <- NA
    outprep <- vector("list", nm)
    pb <- txtProgressBar(0, nm, style = 3)
    counter <- 0
    for (ii in sample(seq(nm), nm)) {
        iimatched <- merge(matches[ii, ], controls, by = groupby, 
            all.y = F, suffixes = c(".x", ""))
        if (nrow(iimatched) > 0) {
            outprep[[ii]] <- iimatched[sample(seq(nrow(iimatched)), 
                1), names(controls)]
            iipat <- matches[ii, "ctrl_pn"] <- outprep[[ii]][, 
                patient]
            controls <- controls[iipat != controls[[patient]], 
                ]
        }
        counter <- counter + 1
        setTxtProgressBar(pb, counter)
    }
    close(pb)
    out <- do.call(rbind, outprep)
    invisible(list(control_matched = out, matches = matches))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
