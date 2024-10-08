% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_L1_Pop.R
\name{pullDroneGroupsFromDCA}
\alias{pullDroneGroupsFromDCA}
\title{Pulls drone groups from a Drone Congregation Area (DCA)}
\usage{
pullDroneGroupsFromDCA(DCA, n, nDrones = NULL, simParamBee = NULL, ...)
}
\arguments{
\item{DCA}{\code{\link[AlphaSimR]{Pop-class}}, population of drones}

\item{n}{integer, number of drone groups to be created}

\item{nDrones}{numeric of function, number of drones that a virgin queen
mates with; if \code{NULL} then \code{\link[SIMplyBee]{SimParamBee}$nFathers} is used}

\item{simParamBee}{\code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters}

\item{...}{additional arguments passed to \code{nDrones} when this argument is a function}
}
\value{
list of \code{\link[AlphaSimR]{Pop-class}}
}
\description{
Level 1 function that pulls drone groups from a Drone
  Congregation Area (DCA) to use them later in mating. Within the function
  drones are pulled (removed) from the DCA to reflect the fact that drones
  die after mating, so they can't be present in the DCA anymore. Be careful
  what you do with the DCA object outside function to avoid drone "copies".
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
colony <- addDrones(colony, nInd = 100)

# Create colony DCA
DCA <- createDCA(colony)
pullDroneGroupsFromDCA(DCA, n = 4, nDrones = 5)
pullDroneGroupsFromDCA(DCA, n = 5, nDrones = nFathersPoisson)

}
