# ATTENTION: MAKE SURE TASK 02 IS DONE BEFORE RUNNING THIS SCRIPT!
source("../../utils_sl_varimp.R")
source("../../util.R")
source("../load_sl.R")

var_imp_risk_results <- readRDS("../02_task_VarImpRisk/out/" %+% outcome %+% "_ind_var_imp_risk_combined.RDS")

cut_off <- quantile(var_imp_risk_results$Est, 0.75)
variable_combinations <- combn(subset(var_imp_risk_results, var_imp_risk_results$Est > cut_off)$Variable, m = var_combn)
X <- as.data.frame(variable_combinations)
X <- X[, 1201:1600]

gc()
mips_results <- mips_imp_risk(risk_importance = var_imp_risk_results,
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
                              risk = risk)

saveRDS(mips_results, "out/" %+% outcome %+% "_intxn_imp_risk_004.RDS")

print("Finished MIPS")
