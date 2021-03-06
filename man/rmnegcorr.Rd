% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_munging_functions.R
\name{rmnegcorr}
\alias{rmnegcorr}
\title{Remove negative correlations from a correlation matrix}
\usage{
rmnegcorr(corrmatrix, matrix, dim, niv = 0.1)
}
\arguments{
\item{corrmatrix}{correlation matrix (as produced by varnastats::corrmat())}

\item{matrix}{underlying data.frame}

\item{dim}{switch to define if the correlation matrix should be created
for columns or rows.

1: table is created for column (variables) relations.

2: table is created for row (objects) relations.}

\item{niv}{decision niveau. The smaller niv the weaker the overlap of two
variables/objects can be, to still be recognised as the cause for a positiv relation.

default: 0.1 -> 10\%}
}
\value{
correlation matrix without negative relations
}
\description{
\code{rmnegcorr} removes "negative correlations" from a correlation matrix.
A negative correlation is defined as a relation between to variables/objects,
that have a high correlation value due to NOT frequently appearing together.
This function can be applied for correlation matrizes created for variable or
object relations.
}
\examples{
testmatrixrand <- data.frame(
   matrix(base::sample(0:1,400,replace=TRUE), nrow=20, ncol=20)
)

testmatrixrand[,1] <- c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0)
testmatrixrand[,2] <- c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1)

testcorr <- corrmat(testmatrixrand, "chi2", chi2limit = 0.1, dim = 1)

rmnegcorr(testcorr, testmatrixrand, dim = 1)

}
