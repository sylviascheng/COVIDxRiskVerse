source("../utils_sl_varimp.R")
source("../util.R")
source("load_sl.R")

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

var_imp_risk_results$Label <- data_dictionary$`Nice Label`[match(var_imp_risk_results$Variable, data_dictionary$`Variable Name`)]

# ATTENTION: EDIT THIS BASED ON YOUR DIRECTORY
saveRDS(var_imp_risk_results, "/tmp/sky.qiu/COVIDxrisk/out/" %+% outcome %+% "_ind_var_imp_risk.RDS")

print("Finished Risk Variable Importance")
