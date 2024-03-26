library(corrplot)
library(car)
library(MASS)
library(olsrr) # for stepwise features
library(dplyr)
library(jtools)
library(memisc)

current_dir <- "analysis/temporal_country_models"
result_dir <- "analysis/temporal_country_models/feature_selection"
source(file.path(current_dir,"helper_function.R"))

### LOAD DATA
# import data file
data_path <- file.path(current_dir,"coal_data_deletion.csv")
deletion_data <- read.csv(data_path)
# set the first column as row name then delete
rownames(deletion_data) <- deletion_data$X
deletion_data <- deletion_data[,-1:-2]

### FEATURE SELECTION
response_vector <- c("Coal_Share_Ret","Coal_Share_EarlyRet","Coal_Share_Tran","Coal_Share_PhasedOut")
# independent variables
model_results <- list()  # To store model results
step_for_predictors <- list() # To store the step forward feature selection predictors 
step_back_predictors <- list() # To store the step forward feature selection predictors 
indep_data <- deletion_data[, !colnames(deletion_data) %in% response_vector]
indep_data_std <- f_standardize_df(indep_data)
# loop through the 4 response variables
# investigate the full model with all predictors
# use step forward p to find the most important unique predictors
for (var_name in response_vector){
  run_path <- file.path(result_dir,paste0("coal_features_deletion_",var_name,".txt"))
  sink(run_path)
  print(paste("Dependent Variable",var_name))
  # full model
  formula_str <- paste(var_name, "~ .")
  coal_model_data <- cbind(deletion_data[, var_name],indep_data_std)
  colnames(coal_model_data)[1] <- var_name
  model <- lm(formula_str, data = coal_model_data)
  model_results[[var_name]] <- model
  print("Start FULL MODEL")
  print(summary(model))
  # step forward p regression
  step_for <- ols_step_forward_aic(model,penter=0.05,detail=T)
  print("RESULTS OF STEP FORWARD")
  print(step_for)
  # step backward p regression 
  step_back <- ols_step_backward_aic(model,penter=0.05,detail=T)
  print("RESULTS OF STEP BACKWARD")
  print(step_back)
  if (var_name != response_vector[1]){
    step_for_predictors_intersect <- intersect(this_step_for,step_for$predictors)
    step_back_predictors_intersect <- intersect(this_step_for,step_back$predictors)
  }
  this_step_for <- step_for$predictors
  this_step_back <- step_back$predictors
  step_for_predictors <- c(step_for_predictors,this_step_for)
  step_back_predictors <- c(step_back_predictors,this_step_back)
  sink()
}
step_for_unique <- unique(step_for_predictors)
step_back_unique <- unique(step_back_predictors)
stepwise_intersect <- unique(c(step_for_predictors_intersect,step_back_predictors_intersect))

### MODEL FROM FEATURE SELECTION
# intersection of features that existed in both forward and backward stepwise feature selection
indep_std_intersect <- indep_data_std[, stepwise_intersect]
model_results_intereset <- list()  # To store model results
sink(file.path(result_dir,paste0("coal_features_deletion_stepwise_feature.txt")))
for (var_name in response_vector){
  print(var_name)
  formula_str <- paste(var_name, "~ .")
  coal_model_intersect <- cbind(deletion_data[, var_name],indep_std_intersect)
  colnames(coal_model_intersect)[1] <- var_name
  model_intersect <- lm(formula_str, data = coal_model_intersect)
  model_results_intereset[[var_name]] <- model_intersect
  print(summary(model_intersect))
  # export VIF values
  vif_results <- vif(model_intersect)
  print(vif_results)
}
sink()

### SIMPLE MODEL
small_response <- c("Coal_Share_Ret","Coal_Share_Tran","Coal_Share_PhasedOut")
small_var <- c("Coal_duration_year","Coal_logCurrent_MW", "Value_LNG_Import","R_WholeSale","sanction_LNG_Import")
indep_std_small <- indep_data_std[, small_var]
model_results_small <- list()  # To store model results
sink(file.path(result_dir,paste0("coal_features_deletion_small.txt")))
for (var_name in small_response){
  print(var_name)
  formula_str <- paste(var_name, "~ .")
  coal_model_small <- cbind(deletion_data[, var_name],indep_std_small)
  colnames(coal_model_small)[1] <- var_name
  model_small <- lm(formula_str, data = coal_model_small)
  model_results_small[[var_name]] <- model_small
  print(summary(model_small))
  # export VIF values
  vif_results <- vif(model_small)
  print(vif_results)
}
sink()


### VISUALIZATIONS
# full model 
png(file = file.path(result_dir,"coal_deletion_full_models.png"), width = 200, height = 200,
    units = "mm", res = 600, bg = "transparent")
plot_summs(model_results[["Coal_Share_Ret"]],model_results[["Coal_Share_EarlyRet"]],
           model_results[["Coal_Share_Tran"]],model_results[["Coal_Share_PhasedOut"]],
           model.names=c("Retirement","Early Retirement","Transition","Phase Out"))
dev.off()

variable_mapping <- f_map_variables()

# intersection model 
png(file = file.path(result_dir,"coal_deletion_intersection_models.png"), width = 200, height = 100,
    units = "mm", res = 600, bg = "transparent")
plot_summs(model_results_intereset[["Coal_Share_Ret"]],model_results_intereset[["Coal_Share_EarlyRet"]],
           model_results_intereset[["Coal_Share_Tran"]],model_results_intereset[["Coal_Share_PhasedOut"]],
           coefs = variable_mapping,
           model.names=c("Retirement","Early Retirement","Transition","Phase Out")) 
dev.off()


# small model 
png(file = file.path(result_dir,"coal_deletion_small_models.png"), width = 200, height = 50,
    units = "mm", res = 600, bg = "transparent")
plot_summs(model_results_small[["Coal_Share_Ret"]],
           model_results_small[["Coal_Share_Tran"]],model_results_small[["Coal_Share_PhasedOut"]],
           coefs = variable_mapping,
           model.names=c("Retirement","Transition","Phase Out")) 
dev.off()
