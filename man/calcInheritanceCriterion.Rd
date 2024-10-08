% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_L0_auxilary.R
\name{calcInheritanceCriterion}
\alias{calcInheritanceCriterion}
\title{Calculate the inheritance criterion}
\usage{
calcInheritanceCriterion(
  x,
  queenTrait = 1,
  workersTrait = 2,
  use = "gv",
  simParamBee = NULL
)
}
\arguments{
\item{x}{\code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}} or
\code{\link[SIMplyBee]{MultiColony-class}}}

\item{queenTrait}{numeric (column position) or character (column name), trait
that represents queen's effect on the colony value; if \code{NULL}
then this effect is 0}

\item{workersTrait}{numeric (column position) or character (column name), trait
that represents workers' effect on the colony value; if \code{NULL}
then this effect is 0}

\item{use}{character, the measure to use for the calculation, being
either "gv" (genetic value), "ebv" (estimated breeding value),
or "pheno" (phenotypic value)}

\item{simParamBee}{\code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters}
}
\value{
integer when \code{x} is
  \code{\link[SIMplyBee]{Colony-class}} and a named list when \code{x} is
  \code{\link[SIMplyBee]{MultiColony-class}}, where names are colony IDs
}
\description{
Level 0 function that calculates the inheritance criterion as the
  sum of the queen (maternal) and workers (direct) effect from the queen,
  as defined by Du et al. (2021). This can be seen as the expected value
  of drones from the queen or half the expected value of virgin queens from
  the queen.
}
\examples{
founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
SP <- SimParamBee$new(founderGenomes)
\dontshow{SP$nThreads = 1L}
meanA <- c(10, 10 / SP$nWorkers)
varA <- c(1, 1 / SP$nWorkers)
corA <- matrix(data = c( 1.0, -0.5,
                        -0.5,  1.0), nrow = 2, byrow = TRUE)
SP$addTraitA(nQtlPerChr = 100, mean = meanA, var = varA, corA = corA,
name = c("queenTrait", "workersTrait"))
varE <- c(3, 3 / SP$nWorkers)
corE <- matrix(data = c(1.0, 0.3,
                        0.3, 1.0), nrow = 2, byrow = TRUE)
SP$setVarE(varE = varE, corE = corE)
basePop <- createVirginQueens(founderGenomes)

drones <- createDrones(x = basePop[1], nInd = 1000)
droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)

# Create a Colony and a MultiColony class
colony <- createColony(x = basePop[2])
colony <- cross(colony, drones = droneGroups[[1]])

apiary <- createMultiColony(basePop[3:4], n = 2)
apiary <- cross(apiary, drones = droneGroups[c(2, 3)])

calcInheritanceCriterion(colony, queenTrait = 1, workersTrait = 2)
calcInheritanceCriterion(apiary, queenTrait = 1, workersTrait = 2)

apiary[[2]] <- removeQueen(apiary[[2]])
calcInheritanceCriterion(apiary, queenTrait = 1, workersTrait = 2)

}
\references{
Du, M., et al. (2021) Short-term effects of controlled mating and selection
  on the genetic variance of honeybee populations. Heredity 126, 733–747.
  \doi{10.1038/s41437-021-00411-2}
}
\seealso{
\code{\link[SIMplyBee]{calcSelectionCriterion}} and
  \code{\link[SIMplyBee]{calcPerformanceCriterion}} and  as well as
  \code{vignette(topic = "QuantitativeGenetics", package = "SIMplyBee")}
}
