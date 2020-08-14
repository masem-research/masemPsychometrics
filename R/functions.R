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


