#' @title lavaan-specific function: return modification indices related to a certain variable
#'
#' @description Input is object with output of modindices-function lavaan package.
#'
#' @details what it does: looks for relevant variable in variable lhs or rhs and returns only
#' these fields
#'
#'
#' @param RelevantVariable character. Relevant variable to look for and print out modification indices.
#' @param lavaanMIObject lavaan object. Output object of `laavan::modindices()` function
#' @param sortByMI boolean. Sort in decreasing order modification indices by mi value?
#'
#' @return data.frame. DataFrame with modification indicies (mi values) of a certain variable.
#' @export
#'
#' @examples
#' # official lavaan example
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9'
#' # fit the model to the data
#' fit <- cfa(HS.model, data=HolzingerSwineford1939)
#' # get the modification indices
#' mi_model <- modindices(fit, minimum.value = 10, sort = TRUE)
#' # look for x9 related modification indicies
#' lavaanMIExtract(RelevantVariable = "x9",
#'                 lavaanMIObject = mi_model,
#'                 sortByMI = TRUE)
lavaanMIExtract <- function(RelevantVariable, lavaanMIObject, sortByMI = TRUE) {
  ## look either in variable lhs or rhs
  lhsRelevantVariable <-  lavaanMIObject$lhs == RelevantVariable
  rhsRelevantVariable <-  lavaanMIObject$rhs == RelevantVariable
  ## Combine both vectors using logical operator OR
  filterVector <- lhsRelevantVariable | rhsRelevantVariable
  ## use filter to get reduced data.frame:
  lavaanMIObjectReduced <- lavaanMIObject[filterVector,]
  ## sort by MI?
  if (sortByMI) {
    lavaanMIObjectReduced <- lavaanMIObjectReduced[order(lavaanMIObjectReduced$mi, decreasing = T),]
  }
  return(lavaanMIObjectReduced)
}





#' @title lavaan: Extract standardized directed paths
#'
#' @description Extract directed paths from the result of `lavaan::standardizedSolution()` function
#'
#' @param lavaanStandardizedSolutionDF lavaan.data.frame. Contains standardized coefficients from call of
#' `lavaan::standardizedSolution()` function
#'
#' @return data.frame. DataFrame with standardized directed paths.
#' @export
#'
#' @examples
#' ## Official example provided in lavaan package
#' ## The industrialization and Political Democracy Example
#' ## Bollen (1989), page 332
#' ## Specify model
#' model <- '
#'   # latent variable definitions
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + a*y2 + b*y3 + c*y4
#'      dem65 =~ y5 + a*y6 + b*y7 + c*y8
#'
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'
#'   # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8'
#' ## fit model to data
#' fit <- sem(model, data = PoliticalDemocracy)
#' ## get standardized solutions
#' fit_stdSolution <- standardizedSolution(fit)
#' ## Get standardized directed paths
#' lavaanExtractStandardizedDirectedPaths(lavaanStandardizedSolutionDF = fit_stdSolution)
lavaanExtractStandardizedDirectedPaths <- function(lavaanStandardizedSolutionDF) {
  if (!inherits(lavaanStandardizedSolutionDF, "lavaan.data.frame")) {
    stop("Input object is not a lavaan.data.frame.
         \nDid you request something else?
         \nPlease try lavaan::standardizedSolution(fit)")
  }
  ## Extract relevant parameters
  stdSolutionReducedDataFrame <- data.frame(lhs = lavaanStandardizedSolutionDF$lhs,
                                            op = lavaanStandardizedSolutionDF$op,
                                            rhs = lavaanStandardizedSolutionDF$rhs,
                                            est.std = lavaanStandardizedSolutionDF$est.std)
  # extract path coefficients
  stdSolutionReducedDataFrame <- stdSolutionReducedDataFrame[stdSolutionReducedDataFrame$op == "~",]
  # delete row.names
  row.names(stdSolutionReducedDataFrame) <- NULL
  # return
  return(stdSolutionReducedDataFrame)
}

