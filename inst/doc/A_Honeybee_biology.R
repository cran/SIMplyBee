## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  include = TRUE
)

## ----load---------------------------------------------------------------------
library(package = "SIMplyBee")

## ----founder genomes----------------------------------------------------------
founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)

## ----SimParamBee--------------------------------------------------------------
SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 32)
SP$nWorkers <- 100
SP$nDrones <- 10

## ----SP, eval = FALSE---------------------------------------------------------
#  print(SP)

## ----initialization_diagram, echo=FALSE, out.width='100%', fig.cap = "Simulation initiation"----
knitr::include_graphics("founderpop.png")

## ----base pop virgin queens---------------------------------------------------
baseQueens <- createVirginQueens(founderGenomes)
baseQueens
isVirginQueen(baseQueens)

## ----base pop drones----------------------------------------------------------
baseDrones <- createDrones(x = baseQueens[1], nInd = 15)
baseDrones

## ----colony-------------------------------------------------------------------
colony <- createColony(x = baseQueens[2])
colony

## ----cross colony-------------------------------------------------------------
colony <- cross(colony, drones = baseDrones, checkCross = "warning")
colony

## ----build up colony----------------------------------------------------------
buildUp(colony, nWorkers = 10, nDrones = 7)
buildUp(colony)

## ----buildup and save---------------------------------------------------------
colony <- buildUp(colony)
colony

## ----colony numbers 1---------------------------------------------------------
nQueens(colony)

## ----colony numbers 2---------------------------------------------------------
nFathers(colony)

## ----colony numbers 3---------------------------------------------------------
nWorkers(colony)

## ----colony numbers 4---------------------------------------------------------
nDrones(colony)

## ----colony numbers 5---------------------------------------------------------
nVirginQueens(colony)

## ----colony castes via get 1--------------------------------------------------
(queen <- getQueen(colony))

## ----colony castes via get 2--------------------------------------------------
(fathers <- getFathers(colony))

## ----colony castes via get 3--------------------------------------------------
(workers <- getWorkers(colony))

## ----colony castes via get 4--------------------------------------------------
(drones <- getDrones(colony))

## ----colony castes via get 5--------------------------------------------------
(virginQueens <- getVirginQueens(colony))

## ----remnant------------------------------------------------------------------
tmp <- pullWorkers(colony, n = 10)
colony <- tmp$remnant
colony

## ----pulled workers-----------------------------------------------------------
pulledWorkers <- tmp$pulled
pulledWorkers

## ----caste queen--------------------------------------------------------------
getCaste(queen)

## ----caste fathers------------------------------------------------------------
getCaste(fathers)

## ----caste bees---------------------------------------------------------------
bees <- c(queen, fathers[1:2], workers[1:2], drones[1:2])
getCaste(bees)

## ----misc---------------------------------------------------------------------
getMisc(getQueen(colony))

## ----csd----------------------------------------------------------------------
getCsdAlleles(queen)

## ----inbred colony------------------------------------------------------------
inbredColony <- createColony(x = createVirginQueens(x = colony, nInd = 1))
fathers <- selectInd(drones, nInd = SP$nFathers, use = "rand")
inbredColony <- cross(inbredColony, drones = fathers, checkCross = "warning")
getCsdAlleles(inbredColony)
getCsdAlleles(inbredColony, unique = TRUE)

## ----pHomBrood----------------------------------------------------------------
pHomBrood(inbredColony)

## ----hHomBrood----------------------------------------------------------------
inbredColony <- addWorkers(inbredColony, nInd = 100)
inbredColony
nHomBrood(inbredColony)

## ----hHomBrood II-------------------------------------------------------------
inbredColony <- addWorkers(inbredColony, nInd = 100)
inbredColony
nHomBrood(inbredColony)

## ----queens counters----------------------------------------------------------
getMisc(getQueen(inbredColony))

