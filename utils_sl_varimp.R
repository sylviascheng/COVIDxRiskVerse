################################################################################
################### CREATE SUPER LEARNER MODEL #################################
################################################################################

create_sl <- function(data = covid_data_processed,
                      outcome = outcome,
                      all_outcomes = all_outcomes) {
  covars <- colnames(data)[-which(names(data) %in% c(
    all_outcomes,
    "fips",
    "county_names"
  ))]
  
  task <- make_sl3_Task(
    data = data,
    covariates = covars,
    outcome = outcome,
    folds = origami::make_folds(data, fold_fun = folds_vfold, V = 10)
  )
  
  discrete_sl <- source("../utils_create_sl.R")
  discrete_sl <- discrete_sl$value()
  ## fit the sl3 object
  sl_fit <- discrete_sl$train(task)
  
  return(list("sl_fit" = sl_fit, "covars" = covars))
}

################################################################################
############################## LOAD DATA  ######################################
################################################################################

load_data <- function(path_data = "cleaned_covid_data_final_Mar_31_22.csv",
                      path_data_dict = "Data_Dictionary.xlsx") {
  ## load data
  covid_data_processed <- read.csv(PROCESSED_DATA_PATH(path_data),
                                   check.names = FALSE
  )
  
  covid_data_processed <- covid_data_processed[, -1]
  
  Data_Dictionary <- read_excel(PROCESSED_DATA_PATH(path_data_dict))
  Data_Dictionary <- Data_Dictionary[Data_Dictionary$`Variable Name` %in% colnames(covid_data_processed), ]
  
  covid_data_processed$CountyRelativeDay100Cases <- covid_data_processed$CountyRelativeDay100Cases / covid_data_processed$Population
  covid_data_processed$TotalCasesUpToDate <- covid_data_processed$TotalCasesUpToDate / covid_data_processed$Population
  covid_data_processed$CountyRelativeDay100Deaths <- covid_data_processed$CountyRelativeDay100Deaths / covid_data_processed$Population
  covid_data_processed$TotalDeathsUpToDate <- covid_data_processed$TotalDeathsUpToDate / covid_data_processed$Population
  covid_data_processed$Deathsat1year <- covid_data_processed$Deathsat1year / covid_data_processed$Population
  covid_data_processed$Casesat1year <- covid_data_processed$Casesat1year / covid_data_processed$Population
  
  return(list("data" = covid_data_processed, "data_dictionary" = Data_Dictionary))
}


################################################################################
############################## P-VALUE  ########################################
################################################################################



p_val_fun <- function(x) {
  SE <- (x[3] - x[1]) / (2 * 1.96)
  z <- x[2] / SE
  P <- exp(-0.717 * z - 0.416 * z^2)
  return(P)
}

################################################################################
##################### BLIP IN QUANTILES OF EACH W ##############################
################################################################################

find_breaks <- function(i, blip){
  result <- cut(x = i, breaks = unique(quantile(i)))
  result <-  as.data.frame(cbind(result, blip)) %>%
    group_by(result) %>%
    summarise_at(vars(blip), list(name = mean))
  
  blip_var <- var(result$name)
  return(blip_var)
  
}

################################################################################
############################# SET QUANTILE VALUES ##############################
################################################################################

set_quantiles <- function(data, X, target, target_q, nontarget_q, subcategory_flag = FALSE) {
  if (subcategory_flag == FALSE) {
    if (is.null(nontarget_q)) {
      data[[target]] <- quantile(data[[target]], target_q)
    } else {
      for (i in X) {
        if (i == target) {
          data[[i]] <- quantile(data[[i]], target_q)
        } else {
          data[[i]] <- quantile(data[[i]], nontarget_q)
        }
      }
    }
  } else {
    for (i in colnames(data)) {
      if (i %in% target) {
        data[[i]] <- quantile(data[[i]], target_q)
      }
    }
  }
  
  return(data)
}

################################################################################
############################# LOAD INFO FROM MODEL #############################
################################################################################

load_model <- function(fit,
                       loss,
                       covars,
                       outcome,
                       data = covid_data_processed,
                       Data_Dictionary = Data_Dictionary) {
  
  task <- fit$training_task
  dat <- task$data
  X <- task$nodes$covariates
  Y <- task$Y
  preds <- fit$predict_fold(task, fold_number = "full")
  risk <- mean(loss(preds, Y))
  risk_rescaled <- mean(sqrt((Y - preds)^2) * dat$Population)
  total <- sum(dat[[outcome]] * dat$Population)
  
  
  Data_Dictionary_Used <- Data_Dictionary %>%
    filter(Keep == "Yes") %>%
    select(`Variable Name`, `Label`)
  Data_Dictionary_Used <- Data_Dictionary_Used[Data_Dictionary_Used$`Variable Name` %in% covars, ]
  subcategories <- Data_Dictionary_Used$Label
  variable_list <- Data_Dictionary_Used$`Variable Name`
  
  return_list <- list(
    "Task" = task,
    "Data" = data,
    "X" = X,
    "Y" = Y,
    "risk" = risk,
    "risk_rescaled" = risk_rescaled,
    "total" = total,
    "Data_Dictionary_Used" = Data_Dictionary_Used,
    "Subcategories" = subcategories,
    "Variable_list" = variable_list
  )
}

################################################################################
####################### VARIABLE IMPORTANCE RISK ###############################
################################################################################

var_imp_risk <- function(X,
                         data,
                         outcome,
                         covars,
                         fit,
                         loss,
                         Y,
                         num_boot,
                         Data_Dictionary) {
  
  tmp_fun <- function(i) {
    boot_results_list <- list()
    
    for (boot in seq(num_boot)) {
      print(boot)
      nr <- nrow(data)
      
      resampled_data <- data[sample(1:nr, size = nr, replace = TRUE), ]
      resampled_data_perm <- resampled_data
      resampled_data_perm[[i]] <- sample(resampled_data[[i]], size = nr)
      
      task_no_perm <- make_sl3_Task(
        data = resampled_data,
        outcome = outcome,
        covariates = covars
      )
      
      task_perm <- make_sl3_Task(
        data = resampled_data_perm,
        outcome = outcome,
        covariates = covars
      )
      
      resampled_sl_preds <- fit$predict_fold(task_no_perm, fold_number = "full")
      resampled_perm_sl_preds <- fit$predict_fold(task_perm, fold_number = "full")
      
      varimp_metric <- mean(loss(resampled_perm_sl_preds, Y)) / mean(loss(resampled_sl_preds, Y))
      
      boot_results_list[[boot]] <- varimp_metric
    }
    
    pval <- (1 + sum(unlist(boot_results_list) <= 1)) / (num_boot + 1)
    quantiles <- quantile(unlist(boot_results_list), probs <- c(0.025, 0.50, 0.975))
    
    results_list <- list(
      "Variable" = i,
      "Lower_CI" = quantiles[[1]],
      "Est" = quantiles[[2]],
      "Upper_CI" = quantiles[[3]],
      "P_Value" = pval
    )
    
    return(results_list)
  }
  
  risk_importance <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(risk_importance) <- c("Variable", "Lower_CI", "Est", "Upper_CI", "P_Value")
  for (i in X) {
    print(i)
    tictoc::tic()
    risk_importance <- rbind(risk_importance, as.data.frame(tmp_fun(i)))
    tictoc::toc()
  }
  
  return(risk_importance)
}

################################################################################
################### SUBGROUP IMPORTANCE BASED ON RISK ##########################
################################################################################

subcat_imp_risk <- function(subcategories,
                            subcategories_cur,
                            data,
                            outcome,
                            covars,
                            fit,
                            loss,
                            Y,
                            num_boot,
                            variable_list) {
  
  X <- subcategories_cur
  X <- X[X != "outcome"]
  
  tmp_fun <- function(i) {
    boot_results_list <- list()
    for (boot in seq(num_boot)) {
      print(boot)
      nr <- nrow(data)
      subcat_vars <- variable_list[which(subcategories %in% i)]
      
      resampled_data_perm <- as.data.frame(data[sample(1:nr, size = nr, replace = TRUE), ])
      resampled_data_no_perm <- as.data.frame(data[sample(1:nr, size = nr, replace = TRUE), ])
      resampled_data <- as.data.frame(data[sample(1:nr, size = nr, replace = TRUE), ])
      
      resampled_data_perm[, subcat_vars] <- resampled_data_no_perm[, subcat_vars]
      
      task_no_perm <- make_sl3_Task(
        data = resampled_data,
        outcome = outcome,
        covariates = covars
      )
      
      task_perm <- make_sl3_Task(
        data = resampled_data_perm,
        outcome = outcome,
        covariates = covars
      )
      
      resampled_sl_preds <- fit$predict_fold(task_no_perm, fold_number = "full")
      resampled_perm_sl_preds <- fit$predict_fold(task_perm, fold_number = "full")
      
      varimp_metric <- mean(loss(resampled_perm_sl_preds, Y)) / mean(loss(resampled_sl_preds, Y))
      
      boot_results_list[[boot]] <- varimp_metric
    }
    
    pval <- (1 + sum(unlist(boot_results_list) <= 1)) / (num_boot + 1)
    quantiles <- quantile(unlist(boot_results_list), probs <- c(0.025, 0.50, 0.975))
    
    results_list <- list("Variable" = i, 
                         "Lower_CI" = quantiles[[1]], 
                         "Est" = quantiles[[2]], 
                         "Upper_CI" = quantiles[[3]], 
                         "P_Value" = pval)
    
    return(results_list)
  }
  
  subgroup_risk_importance <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(subgroup_risk_importance) <- c("Variable", "Lower_CI", "Est", "Upper_CI", "P_Value")
  for (i in X) {
    print(i)
    tictoc::tic()
    subgroup_risk_importance <- rbind(subgroup_risk_importance, as.data.frame(tmp_fun(i)))
    tictoc::toc()
  }
  
  return(subgroup_risk_importance)
}


################################################################################
################### VARIABLE IMPORTANCE QUANTILE ###############################
################################################################################

var_imp_quantile <- function(X,
                             data,
                             outcome,
                             covars,
                             fit,
                             loss,
                             Y,
                             num_boot,
                             variable_list,
                             total,
                             Data_Dictionary,
                             p_val_fun) {
  
  tmp_fun <- function(i) {
    quantile_boot_results_list <- list()
    blip_var_x_W <- list()
    
    for (boot in seq(num_boot)) {
      print(boot)
      nr <- nrow(data)
      
      resampled_data <- as.data.frame(data[sample(1:nr, size = nr, replace = TRUE), ])
      
      quantiles <- quantile(resampled_data[[i]])
      
      resampled_data_Q1 <- resampled_data_Q4 <- resampled_data
      
      resampled_data_Q1[[i]] <- quantiles[2]
      resampled_data_Q4[[i]] <- quantiles[4]
      
      Q1_task <- make_sl3_Task(
        data = resampled_data_Q1,
        covariates = covars,
        outcome = outcome
      )
      
      Q4_task <- make_sl3_Task(
        data = resampled_data_Q4,
        covariates = covars,
        outcome = outcome
      )
      
      Q1_predictions <- fit$predict_fold(task = Q1_task, fold_number = "full") * total
      Q4_predictions <- fit$predict_fold(task = Q4_task, fold_number = "full") * total
      
      blip <- Q4_predictions - Q1_predictions
      ATE <- mean(blip)
      VTE <- var(blip)
      blip_min <- range(blip)[1]
      blip_max <- range(blip)[2]
      blip_range <- blip_max - blip_min
      
      results_list <- list("ATE" = ATE,
                           "VTE" = VTE,
                           "Blip_min" = blip_min,
                           "Blip_max" = blip_max,
                           "Blip_range" = blip_range)
      
      quantile_boot_results_list[[boot]] <- results_list
      
      blip_var_W <- apply(resampled_data[, -c(1:8)], 2,  FUN = find_breaks, blip = blip)
      
      blip_var_x_W[[boot]] <- blip_var_W
    }
    
    blip_intxn_results <- bind_rows(blip_var_x_W)
    blip_intxn_medians <- apply(blip_intxn_results, 2, median)
    max_variance <- which.max(blip_intxn_medians)
    
    quantiles_max_blip_var <- t(as.data.frame(quantile(blip_intxn_results[[max_variance]],
                                                       probs <- c(0.025, 0.50, 0.975))))
    
    pval_blip_var <- apply(quantiles_max_blip_var, 1, p_val_fun)
    
    blip_var_results <- bind_cols(i, quantiles_max_blip_var, pval_blip_var, names(max_variance))
    blip_var_results$Condition <- "Blip_Intxn"
    rownames(blip_var_results) <- NULL
    colnames(blip_var_results) <- c("Variable", "Lower_CI", "Est", "Upper_CI", "P_Value", "Intxn_Var", "Condition")
    
    quantile_boot_results <- bind_rows(quantile_boot_results_list)
    
    quantile_boot_results_CIs <- t(sapply(quantile_boot_results, quantile,
                                          probs <- c(0.025, 0.50, 0.975)))
    
    pvals <- as.data.frame(apply(quantile_boot_results_CIs, 1, p_val_fun))
    result <- bind_cols(i, quantile_boot_results_CIs, pvals, "None")
    result$Condition <- rownames(result)
    
    rownames(result) <- NULL
    
    colnames(result) <- c("Variable", "Lower_CI", "Est", "Upper_CI", "P_Value", "Intxn_Var", "Condition")
    
    result <- bind_rows(result, blip_var_results)
    
    return(result)
  }
  
  quantile_importance <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(quantile_importance) <- c("Variable", "Lower_CI", "Est", "Upper_CI", "P_Value", "Intxn_Var", "Condition")
  for (i in X) {
    print(i)
    tictoc::tic()
    quantile_importance <- rbind(quantile_importance, as.data.frame(tmp_fun(i)))
    tictoc::toc()
  }
  
  quantile_importance$Label <- Data_Dictionary$`Nice Label`[match(quantile_importance$Variable, Data_Dictionary$`Variable Name`)]
  
  return(quantile_importance)
}

################################################################################
#################### SUBGROUP QUANTILE IMPORTANCE ##############################
################################################################################

subcat_imp_quantile <- function(subcategories,
                                subcategories_cur,
                                data,
                                outcome,
                                covars,
                                fit,
                                loss,
                                Y,
                                num_boot,
                                variable_list,
                                total,
                                Data_Dictionary,
                                p_val_fun) {
  
  X <- subcategories_cur
  X <- X[X != "outcome"]
  
  tmp_fun <- function(i) {
    quantile_subcat_boot_results_list <- list()
    for (boot in seq(num_boot)) {
      print(boot)
      nr <- nrow(data)
      
      resampled_data <- as.data.frame(data[sample(1:nr,
                                                  size = nr,
                                                  replace = TRUE), ])
      
      subcat_vars <- variable_list[which(subcategories %in% i)]
      
      subcat_25_nontarget_obs <- set_quantiles(data = resampled_data,
                                               X,
                                               target = subcat_vars,
                                               target_q = 0.25,
                                               nontarget_q = NULL,
                                               subcategory_flag = TRUE)
      
      subcat_75_nontarget_obs <- set_quantiles(data = resampled_data,
                                               X,
                                               target = subcat_vars,
                                               target_q = 0.75,
                                               nontarget_q = NULL,
                                               subcategory_flag = TRUE)
      
      task_sub_cat_75_nontarget_obs <- make_sl3_Task(
        data = subcat_75_nontarget_obs,
        covariates = covars,
        outcome = outcome
      )
      
      task_sub_cat_25_nontarget_obs <- make_sl3_Task(
        data = subcat_25_nontarget_obs,
        covariates = covars,
        outcome = outcome
      )
      
      subcat_75_obs_predictions <- fit$predict_fold(task = task_sub_cat_75_nontarget_obs,
                                                    fold_number = "full")
      
      subcat_25_obs_predictions <- fit$predict_fold(task = task_sub_cat_25_nontarget_obs,
                                                    fold_number = "full")
      
      varimp_metric <- mean(subcat_75_obs_predictions - subcat_25_obs_predictions)
      
      quantile_subcat_boot_results_list[[boot]] <- varimp_metric
    }
    
    quantiles <- quantile(unlist(quantile_subcat_boot_results_list),
                          probs <- c(0.025, 0.50, 0.975))
    p_val <- p_val_fun(quantiles)
    
    results_list <- list("Variable" = i, "Lower_CI" = quantiles[[1]],
                         "Est" = quantiles[[2]],
                         "Upper_CI" = quantiles[[3]],
                         "P_Value" = p_val)
    
    return(results_list)
  }
  
  subgroup_quantile_importance <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(subgroup_quantile_importance) <- c("Variable", "Lower_CI", "Est", "Upper_CI", "P_Value")
  for (i in X) {
    print(i)
    tictoc::tic()
    subgroup_quantile_importance <- rbind(subgroup_quantile_importance, as.data.frame(tmp_fun(i)))
    tictoc::toc()
  }
  
  subgroup_quantile_importance[, c(2:4)] <- as.data.frame(sapply(subgroup_quantile_importance[, c(2:4)], as.numeric) * total)
  
  return(subgroup_quantile_importance)
}

################################################################################
######################### MIPS INTERACTIONS RISK ###############################
################################################################################


mips_imp_risk <- function(risk_importance,
                          data,
                          outcome,
                          covars,
                          fit,
                          loss,
                          Y,
                          num_boot,
                          m,
                          Data_Dictionary,
                          p_val_fun,
                          risk) {
  
  tmp_fun <- function(i) {
    target_vars <- X[, i]
    mips_boot_results_list <- list()
    for (boot in seq(num_boot)) {
      print(boot)
      nr <- nrow(data)
      resampled_data_perm <- as.data.frame(data[sample(1:nr, size = nr, replace = TRUE), ])
      resampled_data_no_perm <- as.data.frame(data[sample(1:nr, size = nr, replace = TRUE), ])
      
      resampled_data_perm[, target_vars] <- resampled_data_no_perm[, target_vars]
      
      ## compute the additive risk for this set of variables
      additives <- risk_importance %>%
        filter(Variable %in% target_vars) %>%
        select(Est)
      
      additive_risk <- sum(unlist(additives) - 1) + 1
      
      task_perm <- make_sl3_Task(
        data = resampled_data_perm,
        outcome = outcome,
        covariates = covars
      )
      
      resampled_perm_sl_preds <- fit$predict_fold(task_perm, fold_number = "full")
      
      risk_scrambled <- mean(loss(resampled_perm_sl_preds, Y))
      varimp_metric <- risk_scrambled / risk
      
      target_vars_nice <- Data_Dictionary$`Nice Label`[match(target_vars, Data_Dictionary$`Variable Name`)]
      
      results_list <- list(
        "Variable_Combo" = paste(target_vars_nice, collapse = " & "),
        "Joint_Risk" = varimp_metric,
        "Additive_Risk" = additive_risk
      )
      
      mips_boot_results_list[[boot]] <- results_list
    }
    
    mips_boot_results <- bind_rows(mips_boot_results_list)
    mips_boot_results$MIP <- mips_boot_results$Joint_Risk - mips_boot_results$Additive_Risk
    
    MIPS_CI <- as.data.frame(t(quantile(mips_boot_results$MIP, probs <- c(0.025, 0.50, 0.975))))
    MIPS_pval <- apply(MIPS_CI, 1, p_val_fun)
    
    mips_result <- list(
      "Variable_Combo" = paste(target_vars_nice, collapse = " & "),
      "Lower_CI" = MIPS_CI[[1]],
      "Est" = MIPS_CI[[2]],
      "Upper_CI" = MIPS_CI[[3]],
      "P_Val" = MIPS_pval
    )
    
    return(mips_result)
  }
  
  permuted_importance <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(permuted_importance) <- c("Variable_Combo", "Lower_CI", "Est", "Upper_CI", "P_Val")
  for (i in 1:dim(X)[2]) {
    print(X[,i])
    tictoc::tic()
    permuted_importance <- rbind(permuted_importance, as.data.frame(tmp_fun(i)))
    tictoc::toc()
  }
  
  return(permuted_importance)
}

################################################################################
####################### MIPS INTERACTIONS QUANTILE #############################
################################################################################

mips_imp_quantile <- function(quantile_importance,
                              data,
                              outcome,
                              covars,
                              fit,
                              loss,
                              Y,
                              num_boot,
                              m,
                              Data_Dictionary,
                              p_val_fun,
                              total) {
  
  tmp_fun <- function(i) {
    A <- quantile_importance_blip_var[i,]$Variable
    W <- quantile_importance_blip_var[i,]$Intxn_Var
    
    boot_result_list <- list()
    
    for (boot in seq(num_boot)) {
      print(boot)
      nr <- nrow(data)
      
      resampled_data <- as.data.frame(data[sample(1:nr, size = nr,
                                                  replace = TRUE), ])
      
      quantiles_A <- quantile(as.numeric(resampled_data[[A]]))
      quantiles_W <- quantile(as.numeric(resampled_data[[W]]))
      
      resampled_data_Q1 <- resampled_data_Q4 <- resampled_data
      
      resampled_data_Q1[[A]] <- quantiles_A[2]
      resampled_data_Q4[[A]] <- quantiles_A[4]
      
      Q1_task <- make_sl3_Task(
        data = resampled_data_Q1,
        covariates = covars,
        outcome = outcome
      )
      
      Q4_task <- make_sl3_Task(
        data = resampled_data_Q4,
        covariates = covars,
        outcome = outcome
      )
      
      Q1_predictions <- fit$predict_fold(task = Q1_task,
                                         fold_number = "full") * total
      Q4_predictions <- fit$predict_fold(task = Q4_task,
                                         fold_number = "full") * total
      
      blip <- Q4_predictions - Q1_predictions
      
      W_quant <- cut(resampled_data[[W]],
                     breaks = as.numeric(unique(quantile(as.numeric(resampled_data[[W]])))),
                     include.lowest = TRUE)
      
      blip_by_quant <- as.data.frame(cbind(blip, W_quant)) %>%
        group_by(W_quant) %>%
        summarise_at(vars(blip), list(name = mean))
      
      blip_by_quant <- t(blip_by_quant)
      blip_by_quant <- blip_by_quant[2,]
      
      Q4_by_quant <- as.data.frame(cbind(Q4_predictions, W_quant)) %>%
        group_by(W_quant) %>%
        summarise_at(vars(Q4_predictions), list(name = mean))
      
      Q4_by_quant <- t(Q4_by_quant)
      Q4_by_quant <- Q4_by_quant[2,]
      
      Q1_by_quant <- as.data.frame(cbind(Q1_predictions, W_quant)) %>%
        group_by(W_quant) %>%
        summarise_at(vars(Q1_predictions), list(name = mean))
      
      Q1_by_quant <- t(Q1_by_quant)
      Q1_by_quant <- Q1_by_quant[2,]
      
      boot_result <- as.data.frame(rbind(Q4_by_quant, Q1_by_quant, blip_by_quant))
      
      boot_result$Condition <- rownames(boot_result)
      
      boot_result_list[[boot]] <- boot_result
      
    }
    
    boot_df <- bind_rows(boot_result_list)
    
    Q1_results <- subset(boot_df, Condition == "Q1_by_quant")
    Q4_results <- subset(boot_df, Condition == "Q4_by_quant")
    Blip_results <- subset(boot_df, Condition == "blip_by_quant")
    
    quantiles_length <- dim(Q1_results)[2]-1
    labels <- paste("Q",1:quantiles_length, sep = "")
    
    Q1_quantiles <- as.data.frame(t(as.data.frame(apply(Q1_results[1:quantiles_length], 2, quantile, probs = c(0.025, 0.50, 0.975)))))
    Q1_quantiles$Condition <- "Q1_preds"
    Q1_quantiles$EM_quantile <- labels
    
    Q4_quantiles <- as.data.frame(t(as.data.frame(apply(Q4_results[1:quantiles_length], 2, quantile, probs = c(0.025, 0.50, 0.975)))))
    Q4_quantiles$Condition <- "Q4_preds"
    Q4_quantiles$EM_quantile <- labels
    
    Blip_quantiles <- as.data.frame(t(as.data.frame(apply(Blip_results[1:quantiles_length], 2, quantile, probs = c(0.025, 0.50, 0.975)))))
    Blip_quantiles$Condition <- "Blip_preds"
    Blip_quantiles$EM_quantile <- labels
    
    result <- bind_rows(Q4_quantiles, Q1_quantiles, Blip_quantiles)
    
    result$Target_var <- A
    result$EM_var <- W
    
    rownames(result) <- NULL
    
    return(result)
  }
  
  mips_quantile_importance_results <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(mips_quantile_importance_results) <- c("2.5%", "50%", "97.5%", "Condition", "EM_quantile", "Target_var", "EM_var")
  for (i in 1:nrow(quantile_importance_blip_var)) {
    print(quantile_importance_blip_var[i,]$Variable)
    tictoc::tic()
    mips_quantile_importance_results <- rbind(mips_quantile_importance_results, as.data.frame(tmp_fun(i)))
    tictoc::toc()
  }
  
  return(mips_quantile_importance_results)
}
