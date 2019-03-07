# library(flexsurv)
library(survival)
library(tidyverse)
library(deSolve)

## Source flexsurvreg functions
lapply(list.files("R", full.names = TRUE), source)


## Add covariates to data frame
bosms3 <- flexsurv::bosms3 %>% 
  mutate(var1 = rnorm(nrow(flexsurv::bosms3 ), mean = status*0.2, sd = 0.5))

## Run model with full likelihoods
bexp <- flexsurvreg(Surv(Tstart, Tstop, status) ~ trans + var1,
                    data=bosms3, dist="exp")
# Run separate models
bexp.list <- vector(3, mode="list")
for (i in 1:3) { 
  bexp.list[[i]] <- flexsurvreg(survival::Surv(years, status) ~ var1,
                                subset= (trans ==i),
                                data = bosms3, dist="exp")
}
# Transition matrix
tmat <- rbind(c(NA,1,2),c(NA,NA,3),c(NA,NA,NA))

# Run with covariates under full likelihood and separate model setting
pmatrix.fs(bexp, t=5, trans=tmat, newdata = data.frame(var1 = c(0,1,2)))
pmatrix.fs(bexp.list, t=5, trans=tmat, newdata = data.frame(var1 = c(0,1,2)))
pmatrix.fs(bexp.list, t=5, trans=tmat, newdata = data.frame(var1 = 1))
