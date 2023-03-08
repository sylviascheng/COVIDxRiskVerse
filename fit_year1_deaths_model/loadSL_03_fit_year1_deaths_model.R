# create necessary objects
set.seed(5929922)

# set the fit_sl_varimp args
outcome <- "Deathsat1year"

all_outcomes <- c(
  "CountyRelativeDay100Cases",
  "TotalCasesUpToDate",
  "CountyRelativeDay100Deaths",
  "TotalDeathsUpToDate",
  "Deathsat1year",
  "Casesat1year"
)
label <- "COVID-19 Deaths at 1 Year"
num_boot <- 100
var_combn <- 2

run_risk <- FALSE

start_time <- proc.time()

################################################################################
################################ LOAD SL #######################################
################################################################################
sl <- sl_fit$sl_fit
covars <- sl_fit$covars

loaded_list <- load_model(
  fit = sl,
  loss = loss_squared_error,
  covars = covars,
  outcome = outcome,
  data = data,
  Data_Dictionary = data_dictionary
)

X <- loaded_list$X
Y <- loaded_list$Y

subcategories <- loaded_list$Subcategories
variable_list <- loaded_list$Variable_list
total_outcome <- loaded_list$total

risk_rescaled <- loaded_list$risk_rescaled
risk <- loaded_list$risk

load_model_time <- proc.time()

load_model_time - fit_model_time


plan(multicore, workers = cpus, gc = TRUE)
