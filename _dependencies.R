# _dependencies.R --------------------------------------------------------------
# Never sourced. It exists so that renv::init() and renv::snapshot() can see every
# package this project needs (some are only loaded by name inside functions).
library(ggplot2)
library(nlme)
library(rdrobust)
library(rddensity)
library(simmer)
library(testthat)
library(knitr)
library(rmarkdown)
