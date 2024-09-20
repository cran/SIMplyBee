## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  include = TRUE
)

## ----crossing_decision_tree, echo=FALSE, out.width='90%', fig.cap = "Crossing decision tree"----
knitr::include_graphics("decisionTreeCrossing.png")

## -----------------------------------------------------------------------------
library(package = "SIMplyBee")
library(package = "ggplot2")

## -----------------------------------------------------------------------------
# Simulate 40 founder genomes
founderGenomes <- quickHaplo(nInd = 60, nChr = 1, segSites = 100)
# Set global population paramaters
SP <- SimParamBee$new(founderGenomes)
# Create a base population of 40 virgin queens
basePop <- createVirginQueens(founderGenomes)

# Prepare populations with a single virgin queen
virginQueen1 <- basePop[1]
# Prepare populations with multiple virgin queens
virginQueens1 <- basePop[2:4]
# Prepare virgin MultiColony objects
beekeeper1 <- createMultiColony(basePop[6:11])
beekeeper2 <- createMultiColony(basePop[12:16])
beekeeper3 <- createMultiColony(basePop[17:21])
beekeeper4 <- createMultiColony(basePop[22:31])
beekeeper5 <- createMultiColony(basePop[32:40])
beekeeper6 <- createMultiColony(basePop[41:50]) 
beekeeper7 <- createMultiColony(basePop[51:60]) 

## -----------------------------------------------------------------------------
# Create a DCA from the remaining virgin queens
DCA <- createDrones(basePop[42:50], nInd = 1000)

## -----------------------------------------------------------------------------
# Samples some drone for mating from the DCA
drones <- selectInd(DCA, nInd = 10, use = "rand")
# Mate the virgin queen with the drones
queen1 <- cross(x = virginQueen1, drones = drones, checkCross = "warning")
# Check the number of fathers for the queen
nFathers(queen1)

## -----------------------------------------------------------------------------
# Pre-select drone (father) populations from a DCA
droneGroups <- pullDroneGroupsFromDCA(DCA, n = 20, nDrones = nFathersTruncPoisson)

## -----------------------------------------------------------------------------
sapply(droneGroups, FUN = nInd)

## -----------------------------------------------------------------------------
# Pop-class with multiple queens
queens1 <- cross(x = virginQueens1, drones = droneGroups[1:3], checkCross = "warning")
nFathers(queens1)

## -----------------------------------------------------------------------------
# MultiColony-class
beekeeper1 <- cross(x = beekeeper1, drones = droneGroups[4:9], checkCross = "warning")
nFathers(beekeeper1)

## -----------------------------------------------------------------------------
crossPlan1 <- createCrossPlan(x = beekeeper2,
                              drones = DCA,
                              nDrones = nFathersPoisson)
# Inspect the cross plan
crossPlan1
sapply(crossPlan1, length)

## -----------------------------------------------------------------------------
# Cross the colonies of the beekeeper 2
beekeeper2 <- cross(x = beekeeper2, drones = DCA, crossPlan = crossPlan1, checkCross = "warning")
nFathers(beekeeper2)

## -----------------------------------------------------------------------------
# Read locations from a file
locations = read.csv("Colony_locations.csv")
# Set location to apiaries
beekeeper1_locations <- locations[locations$ColonyID %in% getId(beekeeper1),]
beekeeper1 <-  setLocation(beekeeper1, 
                           location = Map(c, beekeeper1_locations$X, beekeeper1_locations$Y))

beekeeper2_locations <- locations[locations$ColonyID %in% getId(beekeeper2),]
beekeeper2 <-  setLocation(beekeeper2, 
                           location = Map(c, beekeeper2_locations$X, beekeeper2_locations$Y))

beekeeper3_locations <- locations[locations$ColonyID %in% getId(beekeeper3),]
beekeeper3 <-  setLocation(beekeeper3, 
                           location = Map(c, beekeeper3_locations$X, beekeeper3_locations$Y))


## -----------------------------------------------------------------------------
locationsDF <- data.frame(Location = getLocation(c(beekeeper1, beekeeper2, beekeeper3), collapse = TRUE),
                          Beekeeper = c(rep("Beekeeper1", nColonies(beekeeper1)),
                                        rep("Beekeeper2", nColonies(beekeeper2)),
                                        rep("Beekeeper3", nColonies(beekeeper3))))

ggplot(data = locationsDF, aes(x = Location.1, y = Location.2, colour = Beekeeper)) + 
  geom_point()

## -----------------------------------------------------------------------------
crossPlan2 <- createCrossPlan(x = beekeeper3,
                              droneColonies = c(beekeeper1, beekeeper2),
                              spatial = TRUE,
                              radius = 3,
                              nDrones = 13)
# Inspect the cross plan
crossPlan2
sapply(crossPlan2, length)

## -----------------------------------------------------------------------------
beekeeper3 <- cross(x = beekeeper3, 
                    crossPlan = crossPlan2,
                    droneColonies = c(beekeeper1, beekeeper2),
                    nDrones = 13,
                    checkCross = "warning")
# Inspect the number of fathers
nFathers(beekeeper3)

## -----------------------------------------------------------------------------
beekeeper4_locations <- locations[locations$ColonyID %in% getId(beekeeper4),]
beekeeper4 <-  setLocation(beekeeper4, 
                           location = Map(c, beekeeper4_locations$X, beekeeper4_locations$Y))

beekeeper5_locations <- locations[locations$ColonyID %in% getId(beekeeper5),]
beekeeper5 <-  setLocation(beekeeper5, 
                           location = Map(c, beekeeper5_locations$X, beekeeper5_locations$Y))

## -----------------------------------------------------------------------------
beekeeper4 <- cross(x = beekeeper4,
                    droneColonies = c(beekeeper1, beekeeper2, beekeeper3),
                    crossPlan = "create",
                    spatial = FALSE,
                    nDrones = 12,
                    checkCross = "warning")
nFathers(beekeeper4)

beekeeper5 <- cross(x = beekeeper5,
                    droneColonies = c(beekeeper1, beekeeper2, beekeeper3),
                    crossPlan = "create",
                    spatial = TRUE,
                    radius = 3,
                    nDrones = 12,
                    checkCross = "warning")
nFathers(beekeeper5)

## -----------------------------------------------------------------------------
# Create a DCA at a mating station from colony1
(matingStationDCA <- createMatingStationDCA(beekeeper1[[3]], nDPQs = 20, nDronePerDPQ = 1000))

## -----------------------------------------------------------------------------
# Mate only an beekeeper
beekeeper6 <- cross(beekeeper6,
                    drones = matingStationDCA,
                    spatial = FALSE,
                    crossPlan = "create",
                    nDrones = 15,
                    checkCross = "warning")
nFathers(beekeeper6)

## -----------------------------------------------------------------------------
# Create a single drone for single drone insemination
singleDrone = createDrones(beekeeper1[[1]], nInd = 1)
# Create a cross plan for crossing some of the colonies in an open DCA, 
# some with single drone, and some on a mating station
crossPlanBeekeeper7 <- c(
  createCrossPlan(x = beekeeper7[1], 
                  drones = singleDrone, 
                  nDrones = 1),
  createCrossPlan(x = beekeeper7[2:6], 
                  drones = DCA, 
                  nDrones = nFathersTruncPoisson),
  createCrossPlan(x = beekeeper7[7:10], 
                  drones = matingStationDCA, 
                  nDrones = nFathersTruncPoisson)
  )

beekeeper7 <- cross(x = beekeeper7, 
                    crossPlan = crossPlanBeekeeper7, 
                    drones = c(singleDrone, DCA, matingStationDCA),
                    checkCross = "warning")
nFathers(beekeeper7)

