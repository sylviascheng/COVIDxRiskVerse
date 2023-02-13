source("../utils_sl_varimp.R")
source("../util.R")
source("load_sl.R")

################################################################################
############################# SUBCAT IMP QUANTILE ##############################
################################################################################
gc()
subcat_imp_quantile_results <- subcat_imp_quantile(subcategories,
                                                   data = data,
                                                   outcome = outcome,
                                                   covars = covars,
                                                   fit = sl,
                                                   Y = Y,
                                                   num_boot = num_boot,
                                                   variable_list = variable_list,
                                                   total = total_outcome,
                                                   Data_Dictionary = data_dictionary,
                                                   p_val_fun = p_val_fun)

# ATTENTION: EDIT THIS BASED ON YOUR DIRECTORY
saveRDS(subcat_imp_quantile_results, "/tmp/sky.qiu/COVIDxrisk/out/" %+% outcome %+% "_subgroup_imp_quant.RDS")

print("Finished Sub-Category Quantile Importance")
