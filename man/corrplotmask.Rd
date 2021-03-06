% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{corrplotmask}
\alias{corrplotmask}
\title{Mask function for corrplot::corrplot()}
\usage{
corrplotmask(corrmatrix, xmatrix = "notact", mypath,
  voi = 1:ncol(corrmatrix), w = 3000, h = 3000)
}
\arguments{
\item{corrmatrix}{correlation matrix (as produced by varnastats::corrmat())}

\item{xmatrix}{correlation matrix only with the values 0 and 1, where 1 serves as
a marker (as produced by varnastats::corrmat(method="chi2")). Relations with the
value 1 will be marked with an X in the corrplot.

default = 0 (nothing is marked)}

\item{mypath}{file.path where the plot file should be stored

mypath <- file.path("~/path/to/my/directory/",paste("myfilename", ".png", sep = ""))}

\item{voi}{vector of indezes of variables/objects that should be shown on the x-axis.

default = 1:length(corrmatrix[1,]) (every variable/object is shown)}

\item{w}{width of the resulting graphic file in px

default = 3000}

\item{h}{height of the resulting graphic file in px

default = 3000}
}
\value{
graphic file of a correlation matrix plot
}
\description{
\code{corrplotmask} is an input mask for the \code{corrplot()} function of the
package corrplot. \code{corrplot()} visualizes correlation matrizes. See
\code{?corrplot()} for further info.
\code{corrplotmask} allows to get a nice, basic corrplot graphic as a png-file
that is directly saved to the file system. It doesn't allow to define specific
plot settings.
}
\examples{
testmatrixrand <- data.frame(
   matrix(base::sample(0:1,400,replace=TRUE), nrow=20, ncol=20)
)

testcorr <- corrmat(testmatrixrand, "lambda", chi2limit = 0.1, dim = 1)
xtestcorr <- corrmat(testmatrixrand, "chi2", chi2limit = 0.1, dim = 1)

testpath <- file.path(".",paste("testfile", ".png", sep = ""))

corrplotmask(
 corrmatrix = testcorr,
 xmatrix = xtestcorr,
 mypath = testpath,
 voi = 1:10,
 w = 500,
 h = 1000
)

}
