#02_remove objects from model
# need to run "David/02_remove_unnecessary_elemts_model.R"

can_remove <- c(
                "call", "aux", "coefficients",
                "ncoveffs",
                # "mx", 
                "basepars", "AIC", "datameans",
                "N", "events", "trisk", "concat.formula", "res",
                "npars", "fixedpars", "loglik", "logliki", "cl"
                )
  
bexp.list2 <- bexp.list
for(model_element_single in can_remove){
    bexp.list2[] <- map(bexp.list2, function(model_object){
    model_object[model_element_single] <- NULL
    model_object})
 }

StripData <- function(model_object){
  model_object$data <- lapply(model_object$data,
    function(x){
      if(length(dim(x) == 2)) {
        x[, setdiff(c(names(x), colnames(x)), c("(Intercept)", "(weights)"))] <- 0
      } else{
        x[  setdiff(c(names(x), colnames(x)), c("(Intercept)", "(weights)"))] <- 0
        }
      if(length(dim(x) == 2)) x[1,] else  x[1]
  })
  model_object$logliki <- NA
  model_object$logliki <- model_object$logliki[1]
  model_object
}

# StripData <- function(model_object){
#   rapply(model_object, function(model_object) NA, how = "replace")
# }

bexp.list2  <- lapply(bexp.list2, StripData)

## Write to text file and get back out again
dput(bexp.list2, file = "temp.txt")
texted <- dget("temp.txt")

## add output from form.model.matrix(), very simple
texted_augment <- texted
texted_augment <- map(texted_augment, function(x){
  x$for_model_matrix_out = data.frame(var1 =1, var2 = 1)
  x
  })

a5 <- pmatrix.fs(bexp.list, t = 1, tmat, newdata = data.frame(var1 = 1, var2 = 1), ci = FALSE)
a6 <- pmatrix.fs(bexp.list2, t = 1, tmat, newdata = data.frame(var1 = 1, var2 = 1), ci = FALSE)
pmatrix.fs(texted_augment, t = 1, tmat, newdata = data.frame(var1 = 1, var2 = 1), ci = FALSE)

## Check what is common to all 3 models and drop from models 2 and 3
drop_common <- map2_lgl(bexp.list2[[1]], bexp.list2[[2]], ~ identical(.x, .y))
drop_common2 <- map2_lgl(bexp.list2[[1]], bexp.list2[[3]], ~ identical(.x, .y))
all(drop_common == drop_common2)
drop_common <- drop_common[drop_common]
drop_common <- names(drop_common)
bexp.list2[2:3] <- map(bexp.list2[2:3], ~ .x[setdiff(names(.x), drop_common)])

## Write text to one overall model, and two other tight ones
dput(bexp.list2[[1]], "Model1.txt")
dput(bexp.list2[-1], "Model_others.txt")

## Read text in for model1
model1 <- dget(file = "Model1.txt")
other_models <- dget(file = "Model_others.txt")

## Add contentin under specific label to overcome Surv problem
model1$for_model_matrix_out = data.frame(var1 =1, var2 = 1)
common_objects <- names(other_models[[1]])
other_models2 <- other_models
other_models2 <- map(other_models2, function(mymodel){
  for (common_object in common_objects){
    model1[[common_object]] <- mymodel[[common_object]]
  }
  model1
})
names(model1)
names(other_models2[[1]])

## Create final object and check it works
models_all <- vector(mode = "list", length = 3)
models_all[[1]] <- model1
for(i in 2:length(models_all)){
  models_all[[i]] <- other_models2[[i-1]]
}

## Run the final test
res_txt <- pmatrix.fs(models_all,
                 trans = tmat,
                 t = 1,
                 newdata = data.frame(var1 = c(1), var2 = c(1)),
                 ci = TRUE)
res_original <- pmatrix.fs(bexp.list,
                 trans = tmat,
                 t = 1,
                 newdata = data.frame(var1 = c(1), var2 = c(1)),
                 ci = TRUE)
