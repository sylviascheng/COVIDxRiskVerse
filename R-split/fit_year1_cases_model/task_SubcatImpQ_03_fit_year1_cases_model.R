library(here)
source(here::here("loadSL_03_fit_year1_cases_model.R"))

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

print("Finished Sub-Category Quantile Importance")

saveRDS(subcat_imp_quantile_results, here(paste("data/",
                                                outcome,
                                                "_subgroup_imp_quant.RDS",
                                                sep = "")))

