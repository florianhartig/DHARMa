% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/testsResiduals.R
\name{testZeroInflation}
\alias{testZeroInflation}
\title{Tests for zero-inflation in the residuals}
\usage{
testZeroInflation(simulationOutput, print = T, plot = T)
}
\arguments{
\item{simulationOutput}{an object with simulated residuals created by \code{\link{simulateResiduals}}}

\item{print}{whether to print output}

\item{plot}{whether to plot output}
}
\description{
Tests for zero-inflation in the residuals
}
\details{
shows the expected distribution of zeros against the observed
}
\seealso{
\code{\link{testUniformDistribution}}, \code{\link{testSimulatedResiduals}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
}
