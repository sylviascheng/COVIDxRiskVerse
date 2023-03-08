source("../../utils_sl_varimp.R")
source("../../util.R")
source("../load_sl.R")

X <- X[251:300] # ATTENTION: EDIT THIS ~50 PER RUN (TODO: double check the run time of this function)
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

saveRDS(var_imp_quantile_results, "out/" %+% outcome %+% "_ind_var_imp_quantile_006.RDS")

print("Finished Quantile-Based Variable Importance")
