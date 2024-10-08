% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_L2_Colony.R
\name{setLocation}
\alias{setLocation}
\title{Set colony location}
\usage{
setLocation(x, location = c(0, 0))
}
\arguments{
\item{x}{\code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}}

\item{location}{numeric, list, or data.frame, x and y coordinates of colony
locations as
\code{c(x1, y1)} (the same location set to all colonies),
\code{list(c(x1, y1), c(x2, y2))}, or
\code{data.frame(x = c(x1, x2), y = c(y1, y2))}}
}
\value{
\code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}} with set
  location
}
\description{
Level 2 function that to set a Colony or MultiColony object
  location to (x, y) coordinates.
}
\examples{
founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 50)
SP <- SimParamBee$new(founderGenomes)
\dontshow{SP$nThreads = 1L}
basePop <- createVirginQueens(founderGenomes)
drones <- createDrones(basePop[1], n = 1000)
droneGroups <- pullDroneGroupsFromDCA(drones, n = 4, nDrones = 10)

# Create Colony and MultiColony class
colony <- createColony(x = basePop[2])
colony <- cross(colony, drones = droneGroups[[1]])
apiary <- createMultiColony(basePop[3:5])
apiary <- cross(apiary, drones = droneGroups[2:4])

getLocation(colony)
getLocation(apiary)

loc <- c(1, 1)
colony <- setLocation(colony, location = loc)
getLocation(colony)

# Assuming one location (as in bringing colonies to one place!)
apiary <- setLocation(apiary, location = loc)
getLocation(apiary)

# Assuming different locations
locList <- list(c(0, 0), c(1, 1), c(2, 2))
apiary <- setLocation(apiary, location = locList)
getLocation(apiary)

locDF <- data.frame(x = c(0, 1, 2), y = c(0, 1, 2))
apiary <- setLocation(apiary, location = locDF)
getLocation(apiary)
}
