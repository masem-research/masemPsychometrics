% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{lavaanMIExtract}
\alias{lavaanMIExtract}
\title{lavaan-specific function: return modification indices related to a certain variable}
\usage{
lavaanMIExtract(RelevantVariable, lavaanMIObject, sortByMI = TRUE)
}
\arguments{
\item{RelevantVariable}{character. Relevant variable to look for and print out modification indices.}

\item{lavaanMIObject}{lavaan object. Output object of `laavan::modindices()` function}

\item{sortByMI}{boolean. Sort in decreasing order modification indices by mi value?}
}
\value{
data.frame. DataFrame with modification indicies (mi values) of a certain variable.
}
\description{
Input is object with output of modindices-function lavaan package.
}
\details{
what it does: looks for relevant variable in variable lhs or rhs and returns only
these fields
}
\examples{
# official lavaan example
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9'
# fit the model to the data
fit <- cfa(HS.model, data=HolzingerSwineford1939)
# get the modification indices
mi_model <- modindices(fit, minimum.value = 10, sort = TRUE)
# look for x9 related modification indicies
lavaanMIExtract(RelevantVariable = "x9",
                lavaanMIObject = mi_model,
                sortByMI = TRUE)
}
