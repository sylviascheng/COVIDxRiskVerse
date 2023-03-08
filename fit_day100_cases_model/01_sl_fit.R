source("../utils_sl_varimp.R")
source("../util.R")
cpus <- 30
plan(multisession, workers = cpus, gc = TRUE)

options(sl3.verbose = TRUE)

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

fit_model_time <- proc.time()

saveRDS(sl_fit, paste("/global/scratch/users/skyqiu/COVIDxrisk/Models/", outcome, ".RDS", sep = ""))

print("Finished Model Fitting")

print(fit_model_time - start_time)
