library(here)
source(here::here("loadSL_03_fit_day100_deaths_model.R"))

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
                                            total = total_outcome,
                                            Data_Dictionary = data_dictionary,
                                            p_val_fun = p_val_fun
                                          )

saveRDS(var_imp_quantile_results, here(paste("data/",
                                             outcome,
                                             "_ind_var_imp_quantile.RDS",
                                             sep = "")))

print("Finished Quantile-Based Variable Importance")
