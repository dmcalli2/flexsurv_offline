# Demonstrate how to modify model object and original data object that model 
# is fit on 
library(tidyverse)
library(deSolve)
library(mvtnorm)

## Source flexsurvreg functions
map(list.files("R", full.names = TRUE), source)

## Create list of models to demonstrate
## Note have to rename dataframe variables so that time variable is called time, and
## status variable is called status
set.seed(1234)
bosms3 <- flexsurv::bosms3 %>% 
  mutate(var1 = rnorm(nrow(flexsurv::bosms3 ), mean = status*0.2, sd = 0.5),
         var2 = rnorm(nrow(flexsurv::bosms3 ), mean = status*0.2, sd = 0.5)) %>% 
  rename(time = years)
bexp.list <- vector(3, mode="list")

for (i in 1:3) { 
  bexp.list[[i]] <- flexsurv::flexsurvreg(survival::Surv(time, status) ~ var1 + var2,
                                subset= (trans ==i),
                                data = bosms3, dist="exp")
}
## Create transition matrix
tmat <- rbind(c(NA,1,2),c(NA,NA,3),c(NA,NA,NA))

## Write to text file and get back out again
dput(bexp.list, file = "temp.txt")
texted <- dget("temp.txt")

## add output from form.model.matrix(), very simple
texted_augment <- texted
texted_augment <- map(texted_augment, function(x){
  x$for_model_matrix_out = data.frame(var1 =1, var2 = 1)
  x
  })

## Test models work, they do.
a1 <- pmatrix.fs(bexp.list, 
                 trans = tmat, 
                 t = 1, 
                 newdata = data.frame(var1 = c(1), var2 = c(1)),
                 ci = FALSE)
a2 <- pmatrix.fs(texted_augment,
                 trans = tmat,
                 t = 1,
                 newdata = data.frame(var1 = c(1), var2 = c(1)),
                 ci = FALSE)
