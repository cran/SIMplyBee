% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_L0_auxilary.R
\name{simulateHoneyBeeGenomes}
\alias{simulateHoneyBeeGenomes}
\title{Simulate the Honey bee genome}
\usage{
simulateHoneyBeeGenomes(
  nMelN = 0L,
  nMelS = 0L,
  nCar = 0L,
  nLig = 0L,
  Ne = 170000L,
  ploidy = 2L,
  nChr = 16L,
  nSegSites = 100L,
  nBp = 225200000/16,
  genLen = 3.199121,
  mutRate = 3.4e-09,
  recRate = 2.3e-07,
  nThreads = NULL
)
}
\arguments{
\item{nMelN}{integer, number of Apis mellifera mellifera North individuals to simulate}

\item{nMelS}{integer, number of Apis mellifera mellifera South individuals to simulate}

\item{nCar}{integer, number of Apis mellifera carnica individuals to simulate}

\item{nLig}{integer, number of Apis mellifera ligustica individuals to simulate}

\item{Ne}{integer, effective size of the simulated population. Currently set to
170,000, according to Wallberg et al., 2014. Would discourage you to change it
since it is linked to the parameters of the demographic model we use for the simulation.
However, there might be some edge cases when using a different Ne is necessary,
but proceed with caution.}

\item{ploidy}{integer, the ploidy of the individuals}

\item{nChr}{integer, number of chromosomes to simulate}

\item{nSegSites}{integer, number of segregating sites to keep per chromosome}

\item{nBp}{integer, base pair length of chromosome}

\item{genLen}{numeric, genetic length of chromosome in Morgans}

\item{mutRate}{numeric, per base pair mutation rate}

\item{recRate}{numeric, per base pair recombination rate}

\item{nThreads}{integer, if OpenMP is available, this will allow for simulating
chromosomes in parallel. If \code{NULL}, the number of threads is
automatically detected}
}
\value{
\code{\link[AlphaSimR]{MapPop-class}}
}
\description{
Level 0 function that returns simulated honeybee genomes
}
\references{
Wallberg, A., Bunikis, I., Pettersson, O.V. et al.
  A hybrid de novo genome assembly of the honeybee, Apis mellifera,
  with chromosome-length scaffolds. 2019, BMC Genomics 20:275.
  \doi{10.1186/s12864-019-5642-0}

Beye M, Gattermeier I, Hasselmann M, et al. Exceptionally high levels
  of recombination across the honey bee genome.
  2006, Genome Res 16(11):1339-1344. \doi{10.1101/gr.5680406}

Wallberg, A., Han, F., Wellhagen, G. et al. A worldwide survey of
  genome sequence variation provides insight into the evolutionary
  history of the honeybee Apis mellifera.
  2014, Nat Genet 46:1081–1088. \doi{10.1038/ng.3077}

Yang S, Wang L, Huang J, Zhang X, Yuan Y, Chen JQ, Hurst LD, Tian D.
  Parent-progeny sequencing indicates higher mutation rates in heterozygotes.
  2015, Nature 523(7561):463-7. \doi{10.1038/nature14649}.
}
\seealso{
Due to the computational time and resources required to run this function,
  we do not include an example here, but we demonstrate
  its use in the Honeybee biology vignette.
}
