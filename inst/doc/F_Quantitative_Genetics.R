## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  include = TRUE
)

## ----founderGenomes-----------------------------------------------------------
library(package = "SIMplyBee")
library(package = "ggplot2")
founderGenomes <- quickHaplo(nInd = 20, nChr = 16, segSites = 1000)

## ----SimParamBee_mean_and_varA------------------------------------------------
# Global simulation parameters
SP <- SimParamBee$new(founderGenomes)

nQtlPerChr <- 100

# Genetic parameters for queen and workers effects - each represented by a trait
mean <- c(10, 10 / SP$nWorkers)
varA <- c(1, 1 / SP$nWorkers)

## ----SimParamBee_corA_and_addTrait--------------------------------------------
corA <- matrix(data = c( 1.0, -0.5, 
                        -0.5,  1.0), nrow = 2, byrow = TRUE)
SP$addTraitA(nQtlPerChr = nQtlPerChr, mean = mean, var = varA, corA = corA,
             name = c("queenTrait", "workersTrait"))

## ----SimParamBee_varE_and_corR------------------------------------------------
varE <- c(3, 3 / SP$nWorkers)
corE <- matrix(data = c(1.0, 0.3, 
                        0.3, 1.0), nrow = 2, byrow = TRUE)
SP$setVarE(varE = varE, corE = corE)

## ----basePop_virgin_queens, echo = FALSE, fig.height = 5, fig.width = 6-------
# Base population virgin queens
basePop <- createVirginQueens(founderGenomes, n = 20)
head(basePop@gv)
head(basePop@pheno)
oldpar <- par(mfrow=c(2,2))
limQ <- range(c(basePop@gv[, "queenTrait"], basePop@pheno[, "queenTrait"]))
brkQ <- seq(from = limQ[1], to = limQ[2], length.out = 10)
limW <- range(c(basePop@gv[, "workersTrait"], basePop@pheno[, "workersTrait"]))
brkW <- seq(from = limW[1], to = limW[2], length.out = 10)
hist(basePop@gv[, "queenTrait"], xlab = "Genetic value", main = "Queen effect", xlim = limQ, breaks = brkQ)
hist(basePop@gv[, "workersTrait"], xlab = "Genetic value", main = "Workers effect", xlim = limW, breaks = brkW)
hist(basePop@pheno[, "queenTrait"], xlab = "Phenotypic value", main = "Queen effect", xlim = limQ, breaks = brkQ)
hist(basePop@pheno[, "workersTrait"], xlab = "Phenotypic value", main = "Workers effect", xlim = limW, breaks = brkW)
par(oldpar)

## ----basePop_drones, echo = FALSE, fig.height = 2.7, fig.width = 5------------
# Base population drones
drones <- createDrones(x = basePop[1:5], nInd = 3)
head(drones@gv)
oldpar <- par(mfrow=c(1,2))
hist(drones@gv[, "queenTrait"], xlab = "Genetic value", main = "Queen effect")
hist(drones@gv[, "workersTrait"], xlab = "Genetic value", main = "Workers effect")
par(oldpar)

## ----create_colony------------------------------------------------------------
colony <- createColony(x = basePop[6])
colony <- cross(x = colony, drones = drones, checkCross = "warning")
colony <- addWorkers(x = colony, nInd = 50)
colony

## ----getGv_and_getPheno-------------------------------------------------------
getGv(colony, caste = "queen")
getGv(colony, caste = "workers") |> head(n = 4)

getPheno(colony, caste = "queen")
getPheno(colony, caste = "workers") |> head(n = 4)

## ----getGv_and_getPheno_caste-------------------------------------------------
getQueenGv(colony)
getWorkersGv(colony) |> head(n = 4)

getQueenPheno(colony)
getWorkersPheno(colony) |> head(n = 4)

## ----build_up_colony----------------------------------------------------------
# Check if colony is productive
isProductive(colony)

# Build-up the colony and check the production status again
colony <- buildUp(colony)
colony
isProductive(colony)

## ----data.frame---------------------------------------------------------------
# Collate genetic and phenotypic values of workers
df <- data.frame(id = colony@workers@id,
                 mother = colony@workers@mother,
                 father = colony@workers@father,
                 gvQueenTrait = colony@workers@gv[, "queenTrait"],
                 gvWorkersTrait = colony@workers@gv[, "workersTrait"],
                 pvQueenTrait =  colony@workers@pheno[, "queenTrait"],
                 pvWorkersTrait = colony@workers@pheno[, "workersTrait"])
head(df)

## ----plot_queen_vs_worker_values----------------------------------------------
# Covariation between queen and workers effect genetic values in workers
p <- ggplot(data = df, aes(x = gvQueenTrait, y = gvWorkersTrait)) +
  xlab("Genetic value for the queen effect") +
  ylab("Genetic value for the workers effect") +
  geom_point() +
  theme_classic()
print(p)

## ----fathers_values-----------------------------------------------------------
# Variation in patriline genetic values
getFathersGv(colony)

## ----distribution_by_patriline, echo = FALSE, fig.height = 4.5, fig.width = 6----
# Variation in workers effect genetic values by patriline in workers
p <- ggplot(data = df, aes(x = gvWorkersTrait, colour = father)) +
  xlab("Genetic value for the workers effect") +
  geom_density() +
  theme_classic()
print(p)

## ----colony_pheno-------------------------------------------------------------
# Colony phenotype value
calcColonyPheno(colony, queenTrait = "queenTrait", workersTrait = "workersTrait")
help(calcColonyPheno)
help(mapCasteToColonyPheno)

## ----colony_pheno_change------------------------------------------------------
# Colony phenotype value from a reduced colony
removeWorkers(colony, p = 0.5) |>
  calcColonyPheno(queenTrait = "queenTrait", workersTrait = "workersTrait")

## ----colony_pheno_change1-----------------------------------------------------
# Colony phenotype value from a reduced colony
removeWorkers(colony, p = 0.99) |>
  calcColonyPheno(queenTrait = "queenTrait", workersTrait = "workersTrait")

## ----multicolony--------------------------------------------------------------
apiary <- createMultiColony(basePop[7:20])
drones <- createDrones(basePop[1:5], nInd = 100)
droneGroups <- pullDroneGroupsFromDCA(drones, n = nColonies(apiary), nDrones = 15)
apiary <- cross(x = apiary, drones = droneGroups, checkCross = "warning")
apiary <- buildUp(apiary)

## ----multicolony_gv-----------------------------------------------------------
getQueenGv(apiary) |> head(n = 4)
getQueenGv(apiary, collapse = TRUE) |> head(n = 4)

## ----multicolony_pheno--------------------------------------------------------
colonyGv <- calcColonyGv(apiary)
colonyPheno <- calcColonyPheno(apiary)
data.frame(colonyGv, colonyPheno)

## ----multicolony_selection----------------------------------------------------
# Select the best colony based on gv
selectColonies(apiary, n = 1, by = colonyGv)
# Select the best colony based on phenotype
selectColonies(apiary, n = 1, by = colonyPheno)

## ----SimParamBee_2------------------------------------------------------------
# Global simulation parameters
SP <- SimParamBee$new(founderGenomes)

nQtlPerChr <- 100

# Quantitative genetic parameters - for two traits, each with the queen and workers effects
meanP <- c(10, 10 / SP$nWorkers, 0, 0)
varA <- c(1, 1 / SP$nWorkers, 1, 1 / SP$nWorkers)
corA <- matrix(data = c( 1.0, -0.5,  0.0,  0.0, 
                        -0.5,  1.0,  0.0,  0.0,
                         0.0,  0.0,  1.0, -0.4, 
                         0.0,  0.0, -0.4,  1.0), nrow = 4, byrow = TRUE)
SP$addTraitA(nQtlPerChr = 100, mean = meanP, var = varA, corA = corA,
             name = c("yieldQueenTrait", "yieldWorkersTrait",
                      "calmQueenTrait", "calmWorkersTrait"))

varE <- c(3, 3 / SP$nWorkers, 3, 3 / SP$nWorkers)
corE <- matrix(data = c(1.0, 0.3, 0.0, 0.0,
                        0.3, 1.0, 0.0, 0.0,
                        0.0, 0.0, 1.0, 0.2,
                        0.0, 0.0, 0.2, 1.0), nrow = 4, byrow = TRUE)
SP$setVarE(varE = varE, corE = corE)

## ----base_pop_and_colony------------------------------------------------------
basePop <- createVirginQueens(founderGenomes)
drones <- createDrones(x = basePop[1:5], nInd = 100)
apiary <- createMultiColony(basePop[6:20])
droneGroups <- pullDroneGroupsFromDCA(drones, nColonies(apiary), nDrones = 15)
apiary <- cross(x = apiary, drones = droneGroups, checkCross = "warning")
apiary <- buildUp(apiary)
apiary

## -----------------------------------------------------------------------------
getQueenGv(apiary) |> head(n = 4)
getWorkersPheno(apiary, nInd = 3) |> head(n = 4)

## ----colony_pheno_2a----------------------------------------------------------
colonyValues <- calcColonyPheno(apiary,
                                queenTrait = c("yieldQueenTrait", "calmQueenTrait"),
                                workersTrait = c("yieldWorkersTrait", "calmWorkersTrait"),
                                traitName = c("yield", "calmness"),
                                checkProduction = c(TRUE, FALSE)) |> as.data.frame()
colonyValues

## ----colony_pheno_2b----------------------------------------------------------
myMapCasteToColonyPheno <- function(colony) {
  yield <- mapCasteToColonyPheno(colony,
                                 queenTrait = "yieldQueenTrait",
                                 workersTrait = "yieldWorkersTrait",
                                 traitName = "yield",
                                 checkProduction = TRUE)
  calmness <- mapCasteToColonyPheno(colony,
                                    queenTrait = "calmQueenTrait",
                                    workersTrait = "calmWorkersTrait",
                                    traitName = "calmness",
                                    checkProduction = FALSE)
  return(cbind(yield, calmness))
}
colonyValues <- calcColonyPheno(apiary, FUN = myMapCasteToColonyPheno) |> as.data.frame()
colonyValues

## -----------------------------------------------------------------------------
colonyValues$Index <- selIndex(Y = colonyValues, b = c(0.5, 0.5), scale = TRUE) * 10 + 100
bestColony <- selectColonies(apiary, n = 1, by = colonyValues$Index)
getId(bestColony)

## ----SimParamBee_3------------------------------------------------------------
# Global simulation parameters
SP <- SimParamBee$new(founderGenomes)

# Quantitative genetic parameters
# - the first trait has only the queen effect
# - the second trait has both the queen and workers effects
nWorkers <- 100
mean <- c(nWorkers, 10, 10 / nWorkers)
varA <- c(25, 1, 1 / nWorkers)
corA <- matrix(data = c(1.0,  0.0,  0.0,
                        0.0,  1.0, -0.5, 
                        0.0, -0.5,  1.0), nrow = 3, byrow = TRUE)
SP$addTraitA(nQtlPerChr = 100, mean = mean, var = varA, corA = corA,
             name = c("fecundityQueenTrait", "yieldQueenTrait", "yieldWorkersTrait"))

varE <- c(75, 3, 3 / nWorkers)
corE <- matrix(data = c(1.0, 0.0, 0.0,
                        0.0, 1.0, 0.3,
                        0.0, 0.3, 1.0), nrow = 3, byrow = TRUE)
SP$setVarE(varE = varE, corE = corE)

## ----base_pop_and_colony_2----------------------------------------------------
basePop <- createVirginQueens(founderGenomes)
drones <- createDrones(x = basePop[1:5], nInd = 100)
apiary <- createMultiColony(basePop[6:20])
droneGroups <- pullDroneGroupsFromDCA(drones, nColonies(apiary), nDrones = 15)
apiary <- cross(x = apiary, drones = droneGroups, checkCross = "warning")

## ----queen_values, echo = FALSE, fig.height = 5, fig.width = 6----------------
getQueenGv(apiary, collapse = TRUE)
queenPheno <- getQueenPheno(apiary, collapse = TRUE) |> as.data.frame()
cor(queenPheno)
plot(queenPheno)

## ----colony_strength----------------------------------------------------------
apiary <- buildUp(apiary, nWorkers = nWorkersColonyPhenotype,
                  queenTrait = "fecundityQueenTrait")
cbind(nWorkers = nWorkers(apiary), queenPheno)
help(nWorkersColonyPhenotype)

## ----colony_pheno_3, echo = FALSE, fig.height = 5.5, fig.width = 6.5----------
colonyValuesPheno <- calcColonyPheno(apiary,
                                     queenTrait = "yieldQueenTrait",
                                     workersTrait = "yieldWorkersTrait")
pheno <- cbind(nWorkers = nWorkers(apiary), queenPheno, yield = colonyValuesPheno)
cor(pheno)
plot(pheno)

