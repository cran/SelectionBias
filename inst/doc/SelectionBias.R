## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SelectionBias)

## ----eval = FALSE-------------------------------------------------------------
#  # Seed.
#  set.seed(158118)
#  
#  # Number of observations.
#  nObs = 5000
#  
#  # The unmeasured variable, living area (V).
#  urban = rbinom(nObs, 1, 0.85)
#  
#  # The treatment variable, zika.
#  zika_prob = arm::invlogit(-6.2 + 1.75 * urban)
#  zika = rbinom(nObs, 1, zika_prob)
#  
#  # The unmeasured variable, SES (U).
#  SES = rbinom(nObs, 1, 0.5)
#  
#  # The outcome variable, microcephaly.
#  mic_ceph_prob = arm::invlogit(-5.2 + 5 * zika - 1 * SES)
#  mic_ceph = rbinom(nObs, 1, mic_ceph_prob)
#  
#  # The first selection variable, birth.
#  birth_prob = arm::invlogit(1.2 - 4 * zika + 2 * SES)
#  birth = rbinom(nObs, 1, birth_prob)
#  
#  # The second selection variable, hospital.
#  hospital_prob = arm::invlogit(2.2 + 0.5 * urban - 2.75 * SES)
#  hospital = rbinom(nObs, 1, hospital_prob)
#  
#  # The selection indicator.
#  sel_ind = birth * hospital

## ----echo = FALSE-------------------------------------------------------------
zika_learner2 = zika_learner

zika_learner2$zika = ifelse(zika_learner2$zika==1, "Zika infected", "Not zika infected")

table1::label(zika_learner2$mic_ceph) = "Microcephaly"
table1::label(zika_learner2$urban) = "Living area"
table1::label(zika_learner2$SES) = "SES"

my.render.cont <- function(x) {
  with(table1::stats.apply.rounding(table1::stats.default(x), digits=3, rounding.fn = table1::round_pad), c("",
                                                           "Mean "=sprintf("%s", MEAN)))
}

table1::table1(~ mic_ceph + urban + SES | zika, data=zika_learner2, render.continuous = my.render.cont,
           caption = "Table 2. Proportions for the simulated dataset, by treatment status and overall.")

zika_learner2 = subset(zika_learner2,zika_learner2$birth!=0)

table1::label(zika_learner2$mic_ceph) = "Microcephaly"
table1::label(zika_learner2$urban) = "Living area"
table1::label(zika_learner2$SES) = "SES"

table1::table1(~ mic_ceph + urban + SES | zika, data=zika_learner2, render.continuous = my.render.cont,
       caption = "Table 3. Proportions for the simulated dataset, by treatment status and overall, after the first selection.")

zika_learner2 = subset(zika_learner2,zika_learner2$sel_ind!=0)

table1::label(zika_learner2$mic_ceph) = "Microcephaly"
table1::label(zika_learner2$urban) = "Living area"
table1::label(zika_learner2$SES) = "SES"

table1::table1(~ mic_ceph + urban + SES | zika, data=zika_learner2, render.continuous = my.render.cont,
       caption = "Table 4. Proportions for the simulated dataset, by treatment status and overall, after both selections.")

## ----eval = TRUE--------------------------------------------------------------
# SV bound
sensitivityparametersM(whichEst = "RR_tot",
                   whichBound = "SV",
                   Vval = matrix(c(1, 0, 0.85, 0.15), ncol = 2),
                   Uval = matrix(c(1, 0, 0.5, 0.5), ncol = 2),
                   Tcoef = c(-6.2, 1.75),
                   Ycoef = c(-5.2, 5.0, -1.0),
                   Scoef = matrix(c(1.2, 2.2, 0.0, 0.5,
                                    2.0, -2.75, -4.0, 0.0),
                                  ncol = 4),
                   Mmodel = "L",
                   pY1_T1_S1 = 0.286,
                   pY1_T0_S1 = 0.004)

# GAF bound
sensitivityparametersM(whichEst = "RR_tot",
                   whichBound = "GAF",
                   Vval = matrix(c(1, 0, 0.85, 0.15), ncol = 2),
                   Uval = matrix(c(1, 0, 0.5, 0.5), ncol = 2),
                   Tcoef = c(-6.2, 1.75),
                   Ycoef = c(-5.2, 5.0, -1.0),
                   Scoef = matrix(c(1.2, 2.2, 0.0, 0.5,
                                    2.0, -2.75, -4.0, 0.0),
                                  ncol = 4),
                   Mmodel = "L",
                   pY1_T1_S1 = 0.286,
                   pY1_T0_S1 = 0.004)

## ----eval = TRUE--------------------------------------------------------------
SVbound(whichEst = "RR_tot",
        pY1_T1_S1 = 0.004,
        pY1_T0_S1 = 0.286,
        RR_UY_T1 = 2.71,
        RR_UY_T0 = 1.94,
        RR_SU_T1 = 1.80,
        RR_SU_T0 = 2.00)

## ----eval = TRUE--------------------------------------------------------------
SVboundsharp(BF_U = 1.56,
             pY1_T0_S1 = 0.27)

## ----eval = TRUE--------------------------------------------------------------
attach(zika_learner)

AFbound(whichEst = "RR_tot",
        outcome = mic_ceph[sel_ind == 1],
        treatment = zika[sel_ind == 1],
        selection = mean(sel_ind))

## ----eval = TRUE--------------------------------------------------------------
AFbound(whichEst = "RR_tot",
        outcome = c(0.286, 0.004),
        treatment = c(0.002, 0.998),
        selection = mean(sel_ind))

## ----eval = TRUE--------------------------------------------------------------
GAFbound(whichEst = "RR_tot",
         M = 0.4502,
         m = 0.002, 
         outcome = mic_ceph[sel_ind == 1],
         treatment = zika[sel_ind == 1],
         selection = mean(sel_ind))

## ----eval = TRUE--------------------------------------------------------------
CAFbound(whichEst = "RR_tot",
         M = 0.3,
         m = 0.005, 
         outcome = c(0.286, 0.004),
         treatment = c(0.002, 0.998),
         selection = mean(sel_ind))

