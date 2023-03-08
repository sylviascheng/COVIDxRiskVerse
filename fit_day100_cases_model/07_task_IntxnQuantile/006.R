# ATTENTION: MAKE SURE TASK 05 IS DONE BEFORE RUNNING THIS SCRIPT!
source("../../utils_sl_varimp.R")
source("../../util.R")
source("../load_sl.R")

var_imp_quantile_results <- readRDS("../05_task_VarImpQuantile/out/" %+% outcome %+% "_ind_var_imp_quantile_combined.RDS")
quantile_importance <- var_imp_quantile_results
quantile_importance_blip_var <- quantile_importance %>%
  filter(Condition == "Blip_Intxn")
quantile_importance_blip_var <- quantile_importance_blip_var[quantile_importance_blip_var$Variable %notin% c("CentroidLon", "CentroidLat"), ]
quantile_importance_blip_var <- quantile_importance_blip_var[quantile_importance_blip_var$Est > 0,]
quantile_importance_blip_var <- quantile_importance_blip_var[251:300,] # ATTENTION: EDIT TO ~50

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

saveRDS(quantile_mips_results, "out/" %+% outcome %+% "_intxn_imp_quantile_006.RDS")
