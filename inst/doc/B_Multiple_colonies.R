## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  include = TRUE
)

## -----------------------------------------------------------------------------
library(package = "SIMplyBee")

## -----------------------------------------------------------------------------
# Create an empty apiary
emptyApiary <- createMultiColony()
emptyApiary

## -----------------------------------------------------------------------------
# Create an empty apiary with 10 colony slots
emptyApiary1 <- createMultiColony(n = 10)
emptyApiary1

## -----------------------------------------------------------------------------
# Create 20 founder genomes
founderGenomes <- quickHaplo(nInd = 30, nChr = 1, segSites = 100)
# Set up new global simulation parameters
SP <- SimParamBee$new(founderGenomes)
# Create a base population of 20 virgin queens
basePop <- createVirginQueens(founderGenomes)
# Create a DCA from the drones of the first 10 queens
DCA <- createDrones(basePop[1:10], nInd = 100)

## -----------------------------------------------------------------------------
# Create an apiary with the remaining virgin queens
apiary1 <- createMultiColony(x = basePop[11:20])
# Set the location of the apiary
apiary1 <- setLocation(apiary1, c(1,1))

## -----------------------------------------------------------------------------
# Check whether all the colonies are virgin
isQueenPresent(apiary1)
isVirginQueensPresent(apiary1)

## -----------------------------------------------------------------------------
# Get the IDs of the colonies
getId(apiary1)
# Select colonies according to IDs
selectColonies(apiary1, ID = c(1,2))
# Randomly select a given percentage of colonies
selectColonies(apiary1, p = 0.1)

## -----------------------------------------------------------------------------
# Pull one colony - returns a list with $remnant and $pulled nodes
pullColonies(apiary1, n = 1)

## -----------------------------------------------------------------------------
removeColonies(apiary1, ID = 13)

## -----------------------------------------------------------------------------
# Pull 10 groups of drones from the DCA
droneGroups <- pullDroneGroupsFromDCA(DCA, n = 10, nDrones = nFathersPoisson)
# Cross all virgin queens in the apiary to the selected drones
apiary1 <- cross(apiary1, drones = droneGroups)
# Check whether the queens are present (and hence mated)
isQueenPresent(apiary1)

## -----------------------------------------------------------------------------
# Build-up all the colonies in the apiary1
apiary1 <- buildUp(apiary1, nWorkers = 1000, nDrones = 100)

## -----------------------------------------------------------------------------
# Initiate apiary2 at the location (2,2)
apiary2 <- createMultiColony(basePop[21:30])
apiary2 <- setLocation(apiary2, c(2,2))

## -----------------------------------------------------------------------------
apiary1 <- replaceDrones(apiary1)

## -----------------------------------------------------------------------------
# Check whether all colonies in apiary2 are virgin
isQueenPresent(apiary2)
isVirginQueensPresent(apiary2)
# Create a DCA from all the drones in apiary
DCA <- createDCA(apiary1)
# Check how big is the DCA
DCA
# Sample drones groups from the DCA
droneGroups <- pullDroneGroupsFromDCA(DCA, 
                                      n = nColonies(apiary2), 
                                      nDrones = nFathersPoisson)
# Cross virgin queens in apiary2 to selected drones
apiary2 <- cross(apiary2, drones = droneGroups)

