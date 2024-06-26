% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SVbound.R
\name{SVbound}
\alias{SVbound}
\title{Smith and VanderWeele bound}
\usage{
SVbound(
  whichEst,
  pY1_T1_S1,
  pY1_T0_S1,
  RR_UY_T1 = NULL,
  RR_UY_T0 = NULL,
  RR_SU_T1 = NULL,
  RR_SU_T0 = NULL,
  RR_UY_S1 = NULL,
  RR_TU_S1 = NULL
)
}
\arguments{
\item{whichEst}{Input string. Defining the causal estimand of interest.
Available options are as follows. (1) Relative risk in the total
population: "RR_tot", (2) Risk difference in the total population:
"RD_tot", (3) Relative risk in the subpopulation: "RR_sub", (4) Risk
difference in the subpopulation: "RD_sub".}

\item{pY1_T1_S1}{Input value. The probability P(Y=1|T=1,I_S=1). Must be
between 0 and 1.}

\item{pY1_T0_S1}{Input value. The probability P(Y=1|T=0,I_S=1). Must be
between 0 and 1.}

\item{RR_UY_T1}{Input value. The sensitivity parameter RR_UY|T=1. Must be
greater than or equal to 1. Used in the bounds for the total population.}

\item{RR_UY_T0}{Input value. The sensitivity parameter RR_UY|T=0. Must be
greater than or equal to 1. Used in the bounds for the total population.}

\item{RR_SU_T1}{Input value. The sensitivity parameter RR_SU|T=1. Must be
greater than or equal to 1. Used in the bounds for the total population.}

\item{RR_SU_T0}{Input value. The sensitivity parameter RR_SU|T=0. Must be
greater than or equal to 1. Used in the bounds for the total population.}

\item{RR_UY_S1}{Input value. The sensitivity parameter RR_UY|S=1. Must be
greater than or equal to 1. Used in the bounds for the subpopulation.}

\item{RR_TU_S1}{Input value. The sensitivity parameter RR_TU|S=1. Must be
greater than or equal to 1. Used in the bounds for the subpopulation.}
}
\value{
A list containing the Smith and VanderWeele bound.
}
\description{
\code{SVbound()} returns a list with the SV bound. All sensitivity parameters for
the population of interest must be set to numbers, and the rest can be left
as \code{NULL}. The sensitivity parameters can be inserted directly or as output
from \code{sensitivityparametersM()}. If the causal estimand is expected to be
larger than the observational estimand, the recoding of the treatment has to
be done manually.
}
\examples{
# Example for relative risk in the total population.
SVbound(whichEst = "RR_tot", pY1_T1_S1 = 0.05, pY1_T0_S1 = 0.01,
 RR_UY_T1 = 2, RR_UY_T0 = 2, RR_SU_T1 = 1.7, RR_SU_T0 = 1.5)

# Example for risk difference in the total population.
SVbound(whichEst = "RD_tot", pY1_T1_S1 = 0.05, pY1_T0_S1 = 0.01,
 RR_UY_T1 = 2, RR_UY_T0 = 2, RR_SU_T1 = 1.7, RR_SU_T0 = 1.5)

# Example for relative risk in the subpopulation.
SVbound(whichEst = "RR_sub", pY1_T1_S1 = 0.05, pY1_T0_S1 = 0.01,
 RR_UY_S1 = 2.71, RR_TU_S1 = 2.33)

# Example for risk difference in the subpopulation.
SVbound(whichEst = "RD_sub", pY1_T1_S1 = 0.05, pY1_T0_S1 = 0.01,
 RR_UY_S1 = 2.71, RR_TU_S1 = 2.33)

}
\references{
Smith, Louisa H., and Tyler J. VanderWeele. "Bounding bias due
to selection." Epidemiology (Cambridge, Mass.) 30.4 (2019): 509.

Zetterstrom, Stina and Waernbaum, Ingeborg. "Selection bias and multiple
inclusion criteria in observational studies" Epidemiologic Methods 11, no.
1 (2022): 20220108.
}
