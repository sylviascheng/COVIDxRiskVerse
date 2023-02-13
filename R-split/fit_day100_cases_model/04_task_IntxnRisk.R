# ATTENTION: MAKE SURE TASK 02 IS DONE BEFORE RUNNING THIS SCRIPT!
source("../utils_sl_varimp.R")
source("../util.R")
source("load_sl.R")

# ATTENTION: EDIT THIS BASED ON YOUR DIRECTORY
var_imp_risk_results <- readRDS("/tmp/sky.qiu/COVIDxrisk/out/" %+% outcome %+% "_ind_var_imp_risk.RDS")

################################################################################
################################## INTXN RISK ##################################
################################################################################
gc()
mips_results <- mips_imp_risk(risk_importance = var_imp_risk_results,
                              data = data,
                              outcome = outcome,
                              covars = covars,
                              fit = sl,
                              loss = loss_squared_error,
                              Y= Y,
                              num_boot = num_boot,
                              m = var_combn,
                              Data_Dictionary = data_dictionary,
                              p_val_fun = p_val_fun,
                              risk = risk)

# ATTENTION: EDIT THIS BASED ON YOUR DIRECTORY
saveRDS(mips_results, "/tmp/sky.qiu/COVIDxrisk/out/" %+% outcome %+% "_intxn_imp_risk.RDS")

print("Finished MIPS")
