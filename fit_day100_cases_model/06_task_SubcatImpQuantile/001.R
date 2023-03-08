source("../../utils_sl_varimp.R")
source("../../util.R")
source("../load_sl.R")

subcategories_cur <- unique(subcategories)
gc()
subcat_imp_quantile_results <- subcat_imp_quantile(
  subcategories,
  subcategories_cur,
  data = data,
  outcome = outcome,
  covars = covars,
  fit = sl,
  Y = Y,
  num_boot = num_boot,
  variable_list = variable_list,
  total = total_outcome,
  Data_Dictionary = data_dictionary,
  p_val_fun = p_val_fun
)

saveRDS(subcat_imp_quantile_results, "out/" %+% outcome %+% "_subgroup_imp_quant_001.RDS")

print("Finished Sub-Category Quantile Importance")
