% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_L0_auxilary.R
\name{getCasteSex}
\alias{getCasteSex}
\title{Get sex of individuals of a caste, or sex of all members of colony}
\usage{
getCasteSex(x, caste = "all", collapse = FALSE, simParamBee = NULL)
}
\arguments{
\item{x}{\code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
\code{\link[SIMplyBee]{MultiColony-class}}}

\item{caste}{character, "queen", "fathers", "workers", "drones",
"virginQueens", or "all"}

\item{collapse}{logical, if \code{TRUE}, the function will return a single
vector with sex information}

\item{simParamBee}{\code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters}
}
\value{
when \code{x} is \code{\link[AlphaSimR]{Pop-class}} for \code{caste != "all"}
 or list for \code{caste == "all"} with sex nodes named by caste;
   when \code{x} is \code{\link[SIMplyBee]{Colony-class}} return is a named list of
  \code{\link[AlphaSimR]{Pop-class}} for \code{caste != "all"}
  or named list for \code{caste == "all"} indluding caste members sexes;
   when \code{x} is \code{\link[SIMplyBee]{MultiColony-class}} return is a named list of
  \code{\link[AlphaSimR]{Pop-class}} for \code{caste != "all"} or named list of lists of
  \code{\link[AlphaSimR]{Pop-class}} for \code{caste == "all"} indluding caste members sexes
}
\description{
Level 0 function that returns the sex individuals of a caste. To
  get the individuals, use \code{\link[SIMplyBee]{getCastePop}}. To get individuals'
  caste, use \code{\link[SIMplyBee]{getCaste}}.
}
\examples{
founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
SP <- SimParamBee$new(founderGenomes)
\dontshow{SP$nThreads = 1L}
basePop <- createVirginQueens(founderGenomes)

drones <- createDrones(x = basePop[1], nInd = 1000)
droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)

# Create a Colony and a MultiColony class
colony <- createColony(x = basePop[2])
colony <- cross(colony, drones = droneGroups[[1]])
colony <- buildUp(x = colony, nWorkers = 20, nDrones = 5)
colony <- addVirginQueens(colony, nInd = 5)

apiary <- createMultiColony(basePop[3:4], n = 2)
apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
apiary <- buildUp(x = apiary, nWorkers = 10, nDrones = 2)
apiary <- addVirginQueens(apiary, nInd = 4)

getCasteSex(x = drones)
getCasteSex(x = colony)
getCasteSex(x = apiary, caste = "workers")
getCasteSex(x = apiary)
getCasteSex(x = apiary, caste = "virginQueens")
# Collapse information into a single vector
getCasteSex(colony, caste = "all", collapse = TRUE)

# Create a data.frame with sex, colony, and caste information
(tmpC <- getCaste(apiary[[1]]))
(tmpS <- getCasteSex(apiary[[1]]))
(tmpI <- getCasteId(apiary[[1]]))
tmp <- data.frame(caste = unlist(tmpC), sex = unlist(tmpS), id = unlist(tmpI))
head(tmp)
tail(tmp)

(tmpC <- getCaste(apiary))
(tmpS <- getCasteSex(apiary))
(tmpI <- getCasteId(apiary))
tmp <- data.frame(caste = unlist(tmpC), sex = unlist(tmpS), id = unlist(tmpI))
tmp$colony <- sapply(
  X = strsplit(
    x = rownames(tmp), split = ".",
    fixed = TRUE
  ),
  FUN = function(z) z[[1]]
)
head(tmp)
tail(tmp)
}
\seealso{
\code{\link[SIMplyBee]{getCaste}}

\code{\link[SIMplyBee]{getCastePop}} and \code{\link[SIMplyBee]{getCaste}}
}
