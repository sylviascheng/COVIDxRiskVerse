# ATTENTION: EDIT THIS BASED ON YOUR DIRECTORY
sl_fit <- readRDS("../../CountyRelativeDay100Cases.RDS")

# create necessary objects
set.seed(5929922)

`%+%` <- function(a, b) paste0(a, b)

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

## LOAD DATA

load_data_results <- load_data(path_data = "cleaned_covid_data_final.csv",
                               path_data_dict = "Data_Dictionary.xlsx")
data <- load_data_results$data
data_dictionary <- load_data_results$data_dictionary

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
