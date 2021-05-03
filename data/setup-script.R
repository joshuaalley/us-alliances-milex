# Joshua Alley & Matthew Fuhrmann

# script to setup analysis of US alliances and military spending


# load packages
library(MASS)
library(tidyverse)
library(urca)
library(dynamac)
library(msm) 
library(dynlm)
library(lmtest)
library(xtable)
library(stargazer)
library(prediction)
library(coefplot)
library(car)
library(conflicted)
library(gridExtra)
library(ExtremeBounds)
library(sn)
library(countrycode)


# set seed
set.seed(12)


# define a function to check normality and residual autocorrelation
reg.check <- function(x){
  print(shapiro.test(x$residuals)) # normality
  print(bgtest(x, order = 1, type = "F")) # autocorrelation
  print(bgtest(x, order = 2, type = "F"))
}

# set ggplot2 theme
theme_set(theme_bw())

# manage conflicts
conflict_scout()
conflict_prefer("backsolve", "SparseM")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("Position", "ggplot2")
conflict_prefer("combine", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("some", "purrr")
conflict_prefer("sd", "sn")


