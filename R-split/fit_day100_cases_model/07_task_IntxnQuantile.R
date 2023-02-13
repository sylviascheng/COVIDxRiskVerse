# ATTENTION: MAKE SURE TASK 05 IS DONE BEFORE RUNNING THIS SCRIPT!
source("../utils_sl_varimp.R")
source("../util.R")
source("load_sl.R")

# ATTENTION: EDIT THIS BASED ON YOUR DIRECTORY
var_imp_quantile_results <- readRDS("/tmp/sky.qiu/COVIDxrisk/out/" %+% outcome %+% "_ind_var_imp_quantile.RDS")

################################################################################
############################### INTXN QUANTILE #################################
################################################################################
gc()
quantile_mips_results <- mips_imp_quantile(quantile_importance = var_imp_quantile_results,
                                           data = data,
                                           outcome = outcome,
                                           covars = covars,
                                           fit = sl,
                                           loss = loss_squared_error,
                                           Y = Y,
                                           num_boot = num_boot,
                                           m = var_combn,
                                           Data_Dictionary = data_dictionary,
                                           p_val_fun = p_val_fun,
                                           total = total_outcome)

# ATTENTION: EDIT THIS BASED ON YOUR DIRECTORY
saveRDS(quantile_mips_results, "/tmp/sky.qiu/COVIDxrisk/out/" %+% outcome %+% "_intxn_imp_quantile.RDS")
