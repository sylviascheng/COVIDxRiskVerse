#' This script reads in all output files and combine into one data frame

library(purrr)

`%+%` <- function(a, b) paste0(a, b)

fpath <- "fit_day100_cases_model/05_task_VarImpQuantile/out/"
fnames <- list.files(fpath)

var_imp_quantile_results <- map_dfr(fpath %+% fnames, readRDS)
saveRDS(var_imp_quantile_results, fpath %+% "CountyRelativeDay100Cases_ind_var_imp_quantile_combined.RDS")
