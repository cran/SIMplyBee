## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  include = TRUE
)

## -----------------------------------------------------------------------------
library(package = "SIMplyBee")

## -----------------------------------------------------------------------------
# Simulate 40 founder genomes
founderGenomes <- quickHaplo(nInd = 50, nChr = 1, segSites = 100)
# Set global population paramaters
SP <- SimParamBee$new(founderGenomes)
# Create a base population of 40 virgin queens
basePop <- createVirginQueens(founderGenomes)

# Prepare populations with a single virgin queen
virginQueen1 <- basePop[1]
virginQueen2 <- basePop[2]
virginQueen3 <- basePop[3]
# Prepare populations with multiple virgin queens
virginQueens1 <- basePop[4:6]
virginQueens2 <- basePop[7:9]
virginQueens3 <- basePop[10:12]
# Prepare virgin Colony objects
colony1 <- createColony(basePop[13])
colony2 <- createColony(basePop[14])
colony3 <- createColony(basePop[15])
colony4 <- createColony(basePop[16])
# Prepare virgin MultiColony objects
apiary1 <- createMultiColony(basePop[17:21])
apiary2 <- createMultiColony(basePop[22:26])
apiary3 <- createMultiColony(basePop[27:31])
apiary4 <- createMultiColony(basePop[32:41])

## -----------------------------------------------------------------------------
# Create a DCA from the remaining virgin queens
DCA <- createDrones(basePop[42:50], nInd = 1000)

## -----------------------------------------------------------------------------
# Pre-select drone (father) populations from a DCA
droneGroups <- pullDroneGroupsFromDCA(DCA, n = 20, nDrones = nFathersTruncPoisson)

## -----------------------------------------------------------------------------
sapply(droneGroups, FUN = nInd)

## -----------------------------------------------------------------------------
# A single virgin queen
virginQueen1 <- cross(virginQueen1, drones = droneGroups[[1]])
nFathers(virginQueen1)

## -----------------------------------------------------------------------------
# Multiple virgin queens
virginQueens1 <- cross(virginQueens1, drones = droneGroups[2:4])
nFathers(virginQueens1)

## -----------------------------------------------------------------------------
# A colony
colony1 <- cross(colony1, drones = droneGroups[[5]])
nFathers(colony1)

## -----------------------------------------------------------------------------
# An apiary
apiary1 <- cross(x = apiary1, drones = droneGroups[6:10])
nFathers(apiary1)

## -----------------------------------------------------------------------------
# Create a combined cross for mating a single queen (virginQueen2) and a population
# of virgin queen (virginQueens2)
(crossPlanQueens <- createRandomCrossPlan(IDs = c(getId(virginQueen2),
                                                  getId(virginQueens2)),
                                         drones = DCA,
                                         nDrones = 15))

## -----------------------------------------------------------------------------
# Cross a single virgin queen
virginQueen2 <- cross(virginQueen2, drones = DCA, crossPlan = crossPlanQueens)
nFathers(virginQueen2)
# Cross multiple virgin queens
virginQueens2 <- cross(virginQueens2, drones = DCA, crossPlan = crossPlanQueens)
nFathers(virginQueens2)

## -----------------------------------------------------------------------------
(crossPlanColonies <- createRandomCrossPlan(IDs = c(getId(colony2), getId(apiary2)),
                                           drones = DCA,
                                           nDrones = nFathersPoisson))

## -----------------------------------------------------------------------------
# Cross a single colony
colony2 <- cross(colony2, drones = DCA, crossPlan = crossPlanColonies)
nFathers(colony2)
# Cross an apiary
apiary2 <- cross(x = apiary2, drones = DCA, crossPlan = crossPlanColonies)
nFathers(apiary2)

## -----------------------------------------------------------------------------
# Create a DCA at a mating station from colony1
(matingStationDCA <- createMatingStationDCA(colony1, nDPQs = 20, nDronePerDPQ = 1000))

## -----------------------------------------------------------------------------
# Mate only an apiary
crossPlanMatingStation <- createRandomCrossPlan(IDs = c(getId(colony3), 
                                                        getId(apiary3)),
                                                drones = matingStationDCA,
                                                nDrones = nFathersTruncPoisson)
# Cross a colony
colony3 <- cross(colony3, crossPlan = crossPlanMatingStation, drones = matingStationDCA)
nFathers(colony3)
# Cross an apiary
apiary3 <- cross(apiary3, crossPlan = crossPlanMatingStation, drones = matingStationDCA)
nFathers(apiary3)

## -----------------------------------------------------------------------------
# Create a single drone for single drone insemination
singleDrone = createDrones(colony2, nInd = 1)
# Create a cross plan for crossing some of the colonies in an open DCA, 
# some with single drone, and some on a mating station
crossPlanApiary4 <- c(
  createRandomCrossPlan(IDs = getId(apiary4)[1], 
                        drones = singleDrone, 
                        nDrones = 1),
  createRandomCrossPlan(IDs = getId(apiary4)[2:6], 
                        drones = DCA, 
                        nDrones = nFathersTruncPoisson),
  createRandomCrossPlan(IDs = getId(apiary4)[7:10], 
                        drones = matingStationDCA, 
                        nDrones = nFathersTruncPoisson)
  )

apiary4 <- cross(apiary4, 
                 crossPlan = crossPlanApiary4, 
                 drones = c(singleDrone, DCA, matingStationDCA))
nFathers(apiary4)

