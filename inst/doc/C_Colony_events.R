## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  include = TRUE
)

## -----------------------------------------------------------------------------
library(package = "SIMplyBee")

## ----create founder genomes and a Colony and Multicolony objects--------------
founderGenomes <- quickHaplo(nInd = 30, nChr = 1, segSites = 100)
SP <- SimParamBee$new(founderGenomes)
basePop <- createVirginQueens(founderGenomes)
drones <- createDrones(basePop[1:10], n = 1000)
fatherGroups <- pullDroneGroupsFromDCA(drones, n = 30, nDrones = 10)

# Create Colony and MultiColony class, cross them and build them up
colony <- createColony(x = basePop[11])
colony <- cross(colony, drones = fatherGroups[[1]])
colony <- buildUp(colony, nWorkers = 100, nDrones = 20)

apiary <- createMultiColony(basePop[12:17])
apiary <- cross(apiary, drones = fatherGroups[2:7])
apiary <- buildUp(apiary, nWorkers = 100, nDrones = 20, exact = TRUE)

## ----swarm_figure, echo=FALSE, out.width='100%', fig.cap="Swarm function."----
knitr::include_graphics("../man/figures/swarm.png")

## ----swarm colony-------------------------------------------------------------
tmp <- swarm(colony, p = 0.4)
tmp

## ----saving output of the function--------------------------------------------
colony <- tmp$swarm
colony1 <- tmp$remnant

## -----------------------------------------------------------------------------
colony <- buildUp(colony)
colony

## ----inspect the p swarm parameter in SP object-------------------------------
SP$swarmP

## ----set our own swarmP in SP object------------------------------------------
SP$swarmP <- 0.35
SP$swarmP

## ----swarming MultyColony-----------------------------------------------------
tmp <- swarm(apiary)
tmp

## ----inspect the output (swarm)-----------------------------------------------
tmp$swarm[[3]]

## ----inspect the output (remnant)---------------------------------------------
tmp$remnant[[3]]

## ----swarmed MultyColony object with different swarm p------------------------
tmp <- swarm(apiary, p = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8))

## -----------------------------------------------------------------------------
tmp$swarm[[1]]

## -----------------------------------------------------------------------------
tmp$swarm[[3]]

## ----assign swarm back to apiary and build up the apiary----------------------
apiary <- tmp$swarm
apiary <- buildUp(apiary)
apiary1 <- tmp$remnant

## ----split_figure, echo=FALSE, out.width='100%', fig.cap = "Split function"----
knitr::include_graphics("../man/figures/split.png")

## ----split a colony-----------------------------------------------------------
tmp <- split(colony, p = 0.3)
tmp

## -----------------------------------------------------------------------------
colony <- tmp$remnant
colony

## ----build up the colony------------------------------------------------------
colony <- buildUp(colony)

## ----split the MultiColony object and inspect the first colony in remnant-----
tmp <- split(apiary, p = 0.3)
tmp$remnant[[1]]

## ----inspect the split of the first colony------------------------------------
tmp$split[[1]]

## ----build up the colony and apaiary for further examples---------------------
apiary <- tmp$remnant
apiary <- buildUp(apiary)

## ----supersede_figure, echo=FALSE, out.width='100%', fig.cap = "Supersede function"----
knitr::include_graphics("../man/figures/supersede.png")

## ----supersede Colony object--------------------------------------------------
colony <- supersede(colony)
colony

## ----supersede Multicolony object---------------------------------------------
apiary <- supersede(apiary)
apiary

## ----collapse_figure, echo=FALSE, out.width='50%', fig.cap = "Collapse function"----
knitr::include_graphics("../man/figures/collapse.png")

## ----collapse the colony------------------------------------------------------
colony <- collapse(colony)
colony

## ----collapse the MultiColony-------------------------------------------------
apiary <- collapse(apiary)
apiary[[3]]
apiary

