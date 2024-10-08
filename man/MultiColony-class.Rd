% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class-MultiColony.R
\docType{class}
\name{MultiColony-class}
\alias{MultiColony-class}
\alias{isMultiColony}
\alias{show,MultiColony-method}
\alias{c,MultiColony-method}
\alias{c,MultiColonyOrNULL-method}
\alias{[,MultiColony,integerOrNumericOrLogical-method}
\alias{[,MultiColony,character-method}
\alias{[[,MultiColony,integerOrNumericOrLogical-method}
\alias{[[,MultiColony,character-method}
\alias{[<-,MultiColony,integerOrNumericOrLogicalOrCharacter,ANY,MultiColony-method}
\alias{[[<-,MultiColony,integerOrNumericOrLogicalOrCharacter,ANY,Colony-method}
\title{Honeybee multicolony object}
\usage{
isMultiColony(x)

\S4method{show}{MultiColony}(object)

\S4method{c}{MultiColony}(x, ...)

\S4method{c}{MultiColonyOrNULL}(x, ...)

\S4method{[}{MultiColony,integerOrNumericOrLogical}(x, i, j, drop)

\S4method{[}{MultiColony,character}(x, i, j, drop)

\S4method{[[}{MultiColony,integerOrNumericOrLogical}(x, i)

\S4method{[[}{MultiColony,character}(x, i)

\S4method{[}{MultiColony,integerOrNumericOrLogicalOrCharacter,ANY,MultiColony}(x, i, j) <- value

\S4method{[[}{MultiColony,integerOrNumericOrLogicalOrCharacter,ANY,Colony}(x, i, j) <- value
}
\arguments{
\item{x}{\code{\link[SIMplyBee]{MultiColony-class}}}

\item{object}{\code{\link[SIMplyBee]{MultiColony-class}}}

\item{...}{\code{NULL}, \code{\link[SIMplyBee]{Colony-class}}, or
\code{\link[SIMplyBee]{MultiColony-class}}}

\item{i}{integer, numeric, logical, or character, index or ID to select
a colony (see examples)}

\item{j}{not used}

\item{drop}{not used}

\item{value}{\code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}} to
assign into \code{x} based on colony index or name \code{i}}
}
\value{
\code{\link[SIMplyBee]{MultiColony-class}} or \code{\link[SIMplyBee]{Colony-class}}
}
\description{
An object holding a collection of honeybee colonies. It behaves
  like a list.
}
\section{Functions}{
\itemize{
\item \code{isMultiColony()}: Test if x is a MultiColony class object

\item \code{show(MultiColony)}: Show MultiColony object

\item \code{c(MultiColony)}: Combine multiple Colony and MultiColony objects

\item \code{c(MultiColonyOrNULL)}: Combine multiple Colony and MultiColony objects

\item \code{x[i}: Extract a colony (one or more!) with an integer/numeric/logical index (position) (return \code{\link[SIMplyBee]{MultiColony-class}})

\item \code{x[i}: Extract a colony (one or more!) with a character ID (name) (return \code{\link[SIMplyBee]{MultiColony-class}})

\item \code{x[[i}: Extract a colony (just one!) with an integer/numeric/logical index (position) (return \code{\link[SIMplyBee]{Colony-class}})

\item \code{x[[i}: Extract a colony (just one!) with a character ID (name) (return \code{\link[SIMplyBee]{Colony-class}})

\item \code{`[`(x = MultiColony, i = integerOrNumericOrLogicalOrCharacter, j = ANY) <- value}: Assign colonies into MultiColony

\item \code{`[[`(x = MultiColony, i = integerOrNumericOrLogicalOrCharacter, j = ANY) <- value}: Assign Colony into MultiColony

}}
\section{Slots}{

\describe{
\item{\code{colonies}}{list, a collection of \code{\link[SIMplyBee]{Colony-class}} objects}
}}

\examples{
founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
SP <- SimParamBee$new(founderGenomes)
\dontshow{SP$nThreads = 1L}
basePop <- createVirginQueens(founderGenomes)

drones <- createDrones(x = basePop[1], nInd = 1000)
droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
apiary <- createMultiColony(basePop[1:6], n = 6)
apiary <- cross(apiary, drones = droneGroups[1:6])
apiary
show(apiary)
is(apiary)
isMultiColony(apiary)

getId(apiary)
apiary[1]
getId(apiary[1])
getId(apiary["2"])
getId(apiary[2])
getId(apiary[-1])
getId(apiary[5])

getId(apiary)
getId(apiary[c(1, 3)])
getId(apiary[c("2", "4")])
getId(apiary[c(TRUE, FALSE, TRUE, FALSE)])
getId(apiary[c(TRUE, FALSE)]) # beware of recycling!
getId(apiary[c(5, 6)])
getId(apiary[c("6", "7")])

apiary[[1]]
apiary[["2"]]
apiary[[3]]
apiary[["4"]]
try(apiary[[6]])
apiary[["7"]]

getId(c(apiary[c(1, 3)], apiary[2]))
getId(c(apiary[2], apiary[c(1, 3)]))

getId(c(apiary[2], apiary[0]))
getId(c(apiary[0], apiary[2]))

getId(c(apiary[2], NULL))
getId(c(NULL, apiary[2]))

apiary1 <- apiary[1:2]
apiary2 <- apiary[3:4]
getId(apiary1)
getId(apiary2)
apiary1[[1]] <- apiary2[[1]]
getId(apiary1)
try(apiary2[[1]] <- apiary2[[2]])

apiary1 <- apiary[1:2]
apiary2 <- apiary[3:5]
getId(apiary1)
getId(apiary2)
apiary2[1:2] <- apiary1
getId(apiary2)
try(apiary2[1] <- apiary1)
try(apiary2[1:3] <- apiary1)
try(apiary2[1:2] <- apiary1[[1]])

apiary2 <- apiary[3:5]
getId(apiary2)
try(apiary2[c("4", "5")] <- apiary1)
try(apiary2[c("4", "5")] <- apiary1)
}
\seealso{
\code{\link[SIMplyBee]{createMultiColony}}
}
