source("../utils_sl_varimp.R")
source("../util.R")
source("load_sl.R")

################################################################################
############################### VAR IMP QUANTILE ###############################
################################################################################
gc()
var_imp_quantile_results <- var_imp_quantile(X = X,
                                             data = data,
                                             outcome = outcome,
                                             covars = covars,
                                             fit = sl,
                                             loss = loss_squared_error,
                                             Y = Y,
                                             num_boot = num_boot,
                                             total,
                                             Data_Dictionary = data_dictionary,
                                             total = total_outcome,
                                             p_val_fun = p_val_fun)

# ATTENTION: EDIT THIS BASED ON YOUR DIRECTORY
saveRDS(var_imp_quantile_results, "/tmp/sky.qiu/COVIDxrisk/out/" %+% outcome %+% "_ind_var_imp_quantile.RDS")

print("Finished Quantile-Based Variable Importance")
