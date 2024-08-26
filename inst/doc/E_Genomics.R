## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  include = TRUE
)

## ----load package-------------------------------------------------------------
library(package = "SIMplyBee")
founderGenomes <- quickHaplo(nInd = 2, nChr = 3, segSites = 100)
SP <- SimParamBee$new(founderGenomes)
SP$setTrackRec(TRUE) # request recombination tracking

baseQueens <- createVirginQueens(founderGenomes)
baseDrones <- createDrones(x = baseQueens[1], nInd = 15)

colony <- createColony(x = baseQueens[2])
colony <- cross(colony, drones = baseDrones, checkCross = "warning")
colony <- buildUp(colony)

## ----queens haplo-------------------------------------------------------------
getSegSiteHaplo(colony, caste = "queen")[, 1:10]

## ----queens geno--------------------------------------------------------------
getSegSiteGeno(colony, caste = "queen")[, 1:10]

## ----fathers haplo------------------------------------------------------------
getSegSiteHaplo(colony, caste = "fathers")[, 1:10]

## ----fathers geno-------------------------------------------------------------
getSegSiteGeno(colony, caste = "fathers")[, 1:10]

## ----fathers haplo 2----------------------------------------------------------
getSegSiteHaplo(colony, caste = "fathers", 
                nInd = 1, dronesHaploid = FALSE)[, 1:10]

## ----fathers geno 2-----------------------------------------------------------
getSegSiteGeno(colony, caste = "fathers", 
               nInd = 1, dronesHaploid = FALSE)[, 1:10, drop = FALSE]

## ----workers haplo------------------------------------------------------------
getSegSiteHaplo(colony, caste = "workers", nInd = 2)[, 1:10]

## ----workers geno-------------------------------------------------------------
getSegSiteGeno(colony, caste = "workers", nInd = 2)[, 1:10]

## ----drones haplo-------------------------------------------------------------
getSegSiteHaplo(colony, caste = "drones", nInd = 4)[, 1:10]

## ----drones geno--------------------------------------------------------------
getSegSiteGeno(colony, caste = "drones", nInd = 4)[, 1:10]

## ----Colony haplo-------------------------------------------------------------
str(getSegSiteHaplo(colony, caste = "all", collapse = FALSE))

## -----------------------------------------------------------------------------
str(getSegSiteHaplo(colony, caste = "all", collapse = TRUE))

## -----------------------------------------------------------------------------
getSegSiteHaplo(colony, caste = "all", collapse = TRUE)[1:10, 1:10]

## ----all geno-----------------------------------------------------------------
getSegSiteGeno(colony, caste = "all", collapse = TRUE)[1:10, 1:10]

## ----assign genotypes of drones and queens------------------------------------
genoQ <- getSegSiteGeno(colony, caste = "queen")
genoW <- getSegSiteGeno(colony, caste = "workers")

## ----get drones sex-----------------------------------------------------------
sexW <- getCasteSex(colony, caste = "workers")

## ----pooled geno count--------------------------------------------------------
getPooledGeno(x = genoW, type = "count", sex = sexW)[, 1:10]

## ----pooled geno mean---------------------------------------------------------
(poolW <- getPooledGeno(x = genoW, type = "mean", sex = sexW))[, 1:10]

## ----plot genoQ with poolW----------------------------------------------------
plot(y = poolW, x = jitter(genoQ), ylim = c(0, 2), xlim = c(0, 2),
     ylab = "Average allele dosage in workers",
     xlab = "Allele dosage in the queen" )

## ----genotypes----------------------------------------------------------------
geno <- getSegSiteGeno(colony, collapse = TRUE)
sex <- getCasteSex(x = colony, collapse = TRUE)

## ----calcBeeGRMIbs()----------------------------------------------------------
GRM <- calcBeeGRMIbs(x = geno, sex = sex)

## ----view diagonal------------------------------------------------------------
library("Matrix")
image(as(GRM, "Matrix"))

x <- diag(GRM)
hist(x)
summary(x)

## ----view non-diagonal--------------------------------------------------------
x <- GRM[lower.tri(x = GRM, diag = FALSE)]
hist(x)
summary(x)

## ----compare caste memebers---------------------------------------------------
ids <- getCasteId(colony) 
idQueen <- ids$queen
idFathers <- ids$fathers
idWorkers <- ids$workers
idDrones <- ids$drones
idVirginQueens <- ids$virginQueens
mw <- "mw"
md <- "md"

## ----Queen vs fathers 1-------------------------------------------------------
r <- range(GRM)
hist(GRM[idQueen, idFathers], xlim = r)

## ----Queen vs workers 1-------------------------------------------------------
hist(GRM[idQueen, idWorkers], xlim = r)

## ----Queen vs drones 1--------------------------------------------------------
hist(GRM[idQueen, idDrones], xlim = r)

## ----alleleFreq---------------------------------------------------------------
hist(alleleFreq <- calcBeeAlleleFreq(x = geno, sex = sex))

## ----Set up haplotypes--------------------------------------------------------
haploQ <- getQueenIbdHaplo(colony)
haploF <- getFathersIbdHaplo(colony)
haploW <- getWorkersIbdHaplo(colony)
haploD <- getDronesIbdHaplo(colony)
haploV <- getVirginQueensIbdHaplo(colony)
 
haplo <- rbind(haploQ, haploF, haploW, haploD, haploV)

## ----calcGRMIbd---------------------------------------------------------------
GRMs <- calcBeeGRMIbd(x = haplo)

## ----view calcGRMIbd----------------------------------------------------------
image(as(GRMs$genome, "Matrix"))
image(as(GRMs$indiv, "Matrix"))

## ----view diagonal1-----------------------------------------------------------
i <- diag(GRMs$genome)
summary(x)
 
i <- diag(GRMs$indiv)
summary(i)

## ----view non-diagonal1-------------------------------------------------------
x <- GRMs$genome[lower.tri(x = GRMs$genome, diag = FALSE)]
hist(x)
summary(x)
  
i <- GRMs$indiv[lower.tri(x = GRMs$indiv, diag = FALSE)]
hist(i)
summary(i)

## -----------------------------------------------------------------------------
# Obtains caste member IDs
qI <- getQueen(colony)@id
fI <- sort(getFathers(colony)@id)
wI <- sort(getWorkers(colony)@id)
dI <- sort(getDrones(colony)@id)
r <- range(GRMs$indiv)

## ----Queen vs fathers---------------------------------------------------------
hist(GRMs$indiv[fI, qI], xlim = r)

## ----Queen vs workers---------------------------------------------------------
hist(GRMs$indiv[wI, qI], xlim = r)

## ----Queen vs drones----------------------------------------------------------
hist(GRMs$indiv[dI, qI], xlim = r)

