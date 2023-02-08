library(here)

source(here::here("loadSL_03_fit_day100_cases_model.R"))

################################################################################
############################ VAR IMP RISK ######################################
################################################################################
gc()
var_imp_risk_results <- var_imp_risk(X = X,
                                     data = data,
                                     outcome = outcome,
                                     covars = covars,
                                     fit = sl,
                                     loss = loss_squared_error,
                                     Y = Y,
                                     num_boot = num_boot,
                                     Data_Dictionary = data_dictionary)

variable_imp_risk_time <- proc.time()

var_imp_risk_results$Label <- data_dictionary$`Nice Label`[match(var_imp_risk_results$Variable, data_dictionary$`Variable Name`)]

saveRDS(var_imp_risk_results, here(paste("data/",
                                         outcome,
                                         "_ind_var_imp_risk.RDS",
                                         sep = "")))

print("Finished Risk Variable Importance")
