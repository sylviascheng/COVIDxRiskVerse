library(here)
source(here::here("loadSL_03_fit_year1_deaths_model.R"))

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

saveRDS(quantile_mips_results, here(paste("data/",
                                          outcome,
                                          "_intxn_imp_quantile.RDS",
                                          sep = "")))

