## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  include = TRUE
)

## ----load---------------------------------------------------------------------
library(package = "SIMplyBee")

## ----drones_not_removed-------------------------------------------------------
# Initiate simulation
founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 10)
SP <- SimParamBee$new(founderGenomes, csdChr = NULL)

# Base virgin queens
baseVirginQueen <- createVirginQueens(founderGenomes)

# Base drones
baseDrones <- createDrones(baseVirginQueen[1])

# A colony
colony <- createColony(baseVirginQueen[2])
colony <- cross(colony, baseDrones)
colony <- addDrones(colony, nInd = 100)
colony

# Crossing one of the remaining virgin queens with drones from the 
DCA <- createDCA(colony, nInd = 50)
DCA
queen <- cross(baseVirginQueen[3], DCA)
queen
getFathers(queen)

# Note that we did not by default remove drones from the colony when we 
# created the DCA, but these drones now have a caste changed to fathers, so
# they are not available anymore for further mating
colony@drones # 100 drones
table(getCaste(colony@drones)) # 50 drones 50 fathers
getDrones(colony) # 50 drones

