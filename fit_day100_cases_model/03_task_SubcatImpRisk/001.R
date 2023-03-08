source("../../utils_sl_varimp.R")
source("../../util.R")
source("../load_sl.R")

subcategories_cur <- unique(subcategories)
gc()
subcat_imp_risk_results <- subcat_imp_risk(
  subcategories = subcategories,
  subcategories_cur = subcategories_cur,
  data = data, outcome = outcome,
  covars = covars,
  fit = sl,
  loss = loss_squared_error,
  Y = Y,
  num_boot = num_boot,
  variable_list = variable_list
)

saveRDS(subcat_imp_risk_results, "out/" %+% outcome %+% "_subgroup_imp_risk_001.RDS")

print("Finished Subgroup Risk Variable Importance")
