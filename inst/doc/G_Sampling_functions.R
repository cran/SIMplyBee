## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  include = TRUE
)

## -----------------------------------------------------------------------------
library(package = "SIMplyBee")

## -----------------------------------------------------------------------------
founderGenomes <- quickHaplo(nInd = 20, nChr = 1, segSites = 100)
SP <- SimParamBee$new(founderGenomes)
basePop <- createVirginQueens(founderGenomes)

# Create a DCA from the first 10 base virgin queens
DCA <- createDrones(x = basePop[1:10], nInd = 100)

# Create an apiary with 10 virgin colonies
apiary <- createMultiColony(basePop[11:20])

## ----echo = FALSE, fig.height = 3, fig.width = 6------------------------------
oldpar <- par(mfrow = c(1,2))
hist(nFathersPoisson(n = 1000), main = "Average = 15", xlab = "nFathers")
# Change the average to 10
hist(nFathersPoisson(n = 1000, average = 10), main = "Average = 10", xlab = "nFathers")
par(oldpar)

## -----------------------------------------------------------------------------
droneGroups <- pullDroneGroupsFromDCA(DCA = DCA, n = 10, nDrones = nFathersPoisson)
apiary <- cross(apiary, drones = droneGroups, checkCross = "warning")

## -----------------------------------------------------------------------------
nFathers(apiary)
mean(nFathers(apiary))

## ----echo = FALSE, fig.height = 3, fig.width = 3------------------------------
hist(swarmPUnif(n = 1000, min = 0.5, max = 0.7),
     xlab = "swarmP", main = "min=0.5, max=0.7", xlim = c(0, 1))

## -----------------------------------------------------------------------------
apiary <- buildUp(apiary, nWorkers = 1000, nDrones = 100)
tmp <- swarm(apiary, p = swarmPUnif(n = 10, min = 0.5, max = 0.7))
nWorkers(tmp$swarm)

