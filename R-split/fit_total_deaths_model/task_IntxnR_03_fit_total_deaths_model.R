library(here)
source(here::here("loadSL_03_fit_total_deaths_model.R"))

################################################################################
################################## INTXN RISK ##################################
################################################################################
gc()
mips_results <- mips_imp_risk(risk_importance = var_imp_risk_results,
                              data = data,
                              outcome = outcome,
                              covars = covars,
                              fit = sl,
                              loss = loss_squared_error,
                              Y= Y,
                              num_boot = num_boot,
                              m = var_combn,
                              Data_Dictionary = data_dictionary,
                              p_val_fun = p_val_fun,
                              risk = risk)

print("Finished MIPS")

saveRDS(mips_results, here(paste("data/",
                                 outcome, "_intxn_imp_risk.RDS",
                                 sep = "")))

print("Finished Risk Sub-Category Importance")

