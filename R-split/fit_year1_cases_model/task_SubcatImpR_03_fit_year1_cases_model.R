library(here)
source(here::here("loadSL_03_fit_year1_cases_model.R"))

################################################################################
######################### SUBCAT IMP RISK ######################################
################################################################################
gc()
subcat_imp_risk_results <- subcat_imp_risk(
  subcategories = subcategories,
  data = data, outcome = outcome,
  covars = covars,
  fit = sl,
  loss = loss_squared_error,
  Y = Y,
  num_boot = num_boot,
  variable_list = variable_list)

subcat_imp_risk_time <- proc.time()

saveRDS(subcat_imp_risk_results, here(paste("data/",
                                            outcome,
                                            "_subgroup_imp_risk.RDS",
                                            sep = "")))

