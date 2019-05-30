\name{Qfn}
\alias{Qfn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to compute the criteria values Q.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Qfn(X, S, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{S}{
%%     ~~Describe \code{S} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
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
function (X, S, N) 
{
    beta1 <- -1 * (1/(sigma^2))
    dmat <- e2dist(X, S)^2
    lammat <- exp(beta0 + beta1 * dmat)
    lamvec <- exp(beta0 + beta1 * dmat[1:length(dmat)])
    lamJ <- as.vector(t(lammat) \%*\% rep(1, nrow(X)))
    pbar <- as.vector(1 - exp(-t(lammat) \%*\% rep(1, nrow(X))))
    pbar <- mean(pbar)
    M1 <- rep(1, ntraps * nrow(S))
    M2 <- dmat[1:length(dmat)]
    I11 <- (1/nrow(S)) * sum(lamvec)
    I12 <- (1/nrow(S)) * sum(lamvec * M2)
    I21 <- (1/nrow(S)) * sum(lamvec * M2)
    I22 <- (1/nrow(S)) * sum(lamvec * M2 * M2)
    I <- matrix(c(I11, I12, I21, I22), nrow = 2, byrow = TRUE)
    I <- N * pbar * I
    V <- solve(I)
    Q1 <- sum(diag(V))
    sumsJ <- as.vector(t(lammat * lammat * (diag(V)[1] + (dmat^2) * 
        diag(V)[2] - 2 * dmat * V[1, 2])) \%*\% rep(1, nrow(X)))
    var.pbar <- ((1/nrow(S))^2) * sum(exp(-lamJ) * exp(-lamJ) * 
        sumsJ)
    part1 <- (N * N * var.pbar)
    part2 <- N * (1 - pbar)/pbar
    total <- part1 + part2
    newpart2 <- N * (1 - pbar) * (var.pbar + 1)/pbar
    old <- N * N * var.pbar + newpart2
    fixed <- N * pbar * ((1 - pbar) + N * pbar) * (var.pbar/(pbar^4))
    Q1 <- part1
    Q2 <- newpart2
    Q3 <- total
    Q4 <- Q1
    Q5 <- fixed
    Q6 <- 1 - pbar
    Q7 <- var.pbar
    c(Q1, Q2, Q3, Q4, Q5, Q6, Q7)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
