source("../utils_sl_varimp.R")
source("../util.R")
source("load_sl.R")

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

# ATTENTION: EDIT THIS BASED ON YOUR DIRECTORY
saveRDS(subcat_imp_risk_results, "/tmp/sky.qiu/COVIDxrisk/out/" %+% outcome %+% "_subgroup_imp_risk.RDS")

print("Finished Subgroup Risk Variable Importance")
