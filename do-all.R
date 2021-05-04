# Joshua Alley and Matthew Fuhrmann
# script to execute the full analysis of US alliances and military spending

library(here)

# set working directory for replication outside Rstudio project
setwd(here())
getwd()

# run scripts in proper order
# set up environment
source("data/setup-script.R", echo = TRUE)
# clean data
source("data/us-commitments.R", echo = TRUE) # alliance data
source("data/data-cleaning.R", echo = TRUE)
# run primary models
source("data/analysis-ts-lrm.R", echo = TRUE,
       verbose = TRUE)
source("data/counterfactual-analysis.R",
       verbose = TRUE)
# robustness checks
source("data/robustness-checks.R", echo = TRUE,
       verbose = TRUE)
source("data/analysis-ecm.R", echo = TRUE,
       verbose = TRUE) # ECM specification
source("data/analysis-pre45.R", echo = TRUE,
       verbose = TRUE) # include data before 1945
source("data/analysis-changes.R", echo = TRUE,
       verbose = TRUE) # analysis in changes
