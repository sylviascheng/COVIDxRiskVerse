# fit SL

library(here)
source(here("R/utils_sl_varimp.R"))
source(here("R/util.R"))
cpus <- 25
plan(multisession, workers = cpus, gc = TRUE)

set.seed(5929922)

# set the fit_sl_varimp args
outcome <- "CountyRelativeDay100Cases"

all_outcomes <- c(
  "CountyRelativeDay100Cases",
  "TotalCasesUpToDate",
  "CountyRelativeDay100Deaths",
  "TotalDeathsUpToDate",
  "Deathsat1year",
  "Casesat1year"
)
label <- "COVID-19 Cases at Day 100"
num_boot <- 100
var_combn <- 2

run_risk <- FALSE

start_time <- proc.time()

################################################################################
############################## LOAD DATA #######################################
################################################################################


load_data_results <- load_data(path_data = "cleaned_covid_data_final.csv",
                               path_data_dict = "Data_Dictionary.xlsx")
data <- load_data_results$data
data_dictionary <- load_data_results$data_dictionary


################################################################################
################################# FIT SL #######################################
################################################################################

# fit the SL model
sl_fit <- create_sl(data = data, outcome = outcome, all_outcomes = all_outcomes)
sl <- sl_fit$sl_fit
covars <- sl_fit$covars

fit_model_time <- proc.time()

saveRDS(sl, here(paste("Models/", outcome, ".RDS", sep = "")))

print("Finished Model Fitting")

fit_model_time - start_time


