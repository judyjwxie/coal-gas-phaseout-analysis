library(corrplot)
library(car)
library(MASS)
library(olsrr)
library(rio)
library(dplyr)
library(haven)
library(tidyverse)
library(jtools)
library(memisc)
library(stargazer)

setwd("C:/Users/jx920/OneDrive - Imperial College London/1 - Projects/social_political_institutional")
current_dir <- "analysis/temporal_country_models"
result_dir <- "analysis/temporal_country_models/imputation"
model_dir <- "analysis/temporal_country_models/model_results"
source(file.path(current_dir,"helper_function.R"))

########## 
#LOAD DATA
# import data file
amelia <- read.csv(file.path(result_dir,paste0("coal_data_amelia_impAll.csv")))
# set the first column as row name then delete
rownames(amelia) <- amelia$X
amelia <- amelia[,-1:-2]

response_vector <- c("Coal_Share_Ret","Coal_Share_Tran","Coal_Share_PhasedOut",
                     "Coal_Gen_Decline_Perc","Gas_Gen_Decline_Perc")
indep_data <- amelia[, !colnames(amelia) %in% response_vector]
indep_data_std <- f_standardize_df(indep_data)
full_data <- cbind(amelia[, response_vector],indep_data_std)
write.csv(full_data, file.path(model_dir,"coal_data_amelia_std.csv"))

# survey data 
survey_vector <- c("Survey_Worry_.","Survey_Belief_.","Survey_FutureGen_.",
                   "Survey_Personal_.","Survey_FossilMore_.","Survey_FossilLess_.")
survey_result <- read.csv(file.path(current_dir,paste0("_all_temporal_indep_var.csv")))
survey_result <- survey_result %>%#
  distinct()  %>% 
  filter(year >= 2021) %>% 
  unite("cy", Country.Code:year,sep = "-") %>%
  column_to_rownames(var = "cy") %>% 
  dplyr::select(all_of(survey_vector))  %>%
  filter_all(any_vars(!is.na(.)))
survey_result <- f_standardize_df(survey_result)
full_data_survey <- merge(survey_result, full_data,by = "row.names")

# decline rate data
rate_vector <- c("rate_coal_gen_5y","rate_coal_share_5y","rate_coal_cap_5y",
                 "rate_gas_gen_5y","rate_gas_share_5y")
rate_result <- read.csv(file.path(current_dir,paste0("_all_temporal_coalgas_var.csv")))
rate_result <- rate_result %>%#
  distinct()  %>% 
  unite("cy", Country.Code:year,sep = "-") %>%
  column_to_rownames(var = "cy") %>% 
  dplyr::select(all_of(rate_vector))  %>%
  filter_all(any_vars(!is.na(.)))
full_data_rate <- merge(rate_result, full_data,by = "row.names")


##############
#Individual model
linear_model <- function(independent_vars, dep_var, data, covariate_labels, depvar_labels, ext_name){
  model_list <- list()
  
  for (i in 1:length(independent_vars)) {
    formula <- paste(dep_var, "~", paste(independent_vars[[i]], collapse = " + "), sep = " ")
    model <- lm(formula, data = data)
    model$AIC <- AIC(model)
    model_list[[i]] <- model
  }
  stargazer(model_list,type="html",
            dep.var.labels=depvar_labels, 
            covariate.labels=covariate_labels, 
            keep.stat=c("aic", "rsq", "adj.rsq","n"),
            out=file.path(model_dir,paste(dep_var, ext_name,".html",sep = "")))
  return(model_list)
}

########## 
# Coal and gas model
selected_indep <- c("R_Choice", "num_supply_policy", "WDI_Coal_El_.","WDI_NG_El_.", "Share_VRE_Gen",#"Gas_duration_year", #"Coal_duration_year", 
                    "Coal_logCurrent_MW","Gas_logCurrent_MW","LogBP_GasR2P_yr",
                    "HHI_Coal_Import","HHI_Gas_Import","NetValue_Coal_Import","NetValue_Gas_Import")
indep_temp_coal <- list(
  "logGDPpc",
  "annex_one",
  "R_Choice",
  "num_supply_policy",
  selected_indep,
  c("R_Choice", "num_supply_policy", "WDI_Coal_El_.","WDI_NG_El_.", "Share_VRE_Gen","LogBP_GasR2P_yr",
    "HHI_Coal_Import","HHI_Gas_Import","NetValue_Coal_Import","NetValue_Gas_Import"),
  c("R_Choice", "num_supply_policy", "Coal_logCurrent_MW","Gas_logCurrent_MW", "Share_VRE_Gen","LogBP_GasR2P_yr",
    "HHI_Coal_Import","HHI_Gas_Import","NetValue_Coal_Import","NetValue_Gas_Import")
)
cvar_temp_coal <- c("Log GDP per capita",
                    "Annex I",
                    "Choice of supplier",
                    "Number of energy supply policies",
                    "Coal share in electricity",
                    "Gas share in electricity",
                    "VRE share in electricity",
                    "Log current coal capacity",
                    "Log current gas capacity",
                    "Log gas reserve production ratio",
                    "Coal import trade diversity",
                    "Gas import trade diversity",
                    "Coal net import value",
                    "Gas net import value")

model_coal_ret <- linear_model(indep_temp_coal, "Coal_Share_Ret", 
                                      full_data,cvar_temp_coal,
                                      "Share of coal retired in peak capacity","")
model_coal_tran <- linear_model(indep_temp_coal, "Coal_Share_Tran", 
                                       full_data,cvar_temp_coal,
                                       "Share of coal transition in peak capacity","")
model_coal_phaseout <- linear_model(indep_temp_coal, "Coal_Share_PhasedOut", 
                                           full_data,cvar_temp_coal,
                                           "Share of coal phase out in peak capacity","")
model_coal_gen <- linear_model(indep_temp_coal, "Coal_Gen_Decline_Perc", 
                                    full_data,cvar_temp_coal,
                                    "Share of coal decline in peak generation","")
model_gas_gen <- linear_model(indep_temp_coal, "Gas_Gen_Decline_Perc", 
                               full_data,cvar_temp_coal,
                               "Share of gas decline in peak generation","")
# correlation
model_indep_selected <- full_data %>%
  distinct()  %>% 
  dplyr::select(all_of(selected_indep))  
model_corr <- f_corr(model_indep_selected,model_dir,"coalgas_model_indep_correlation")

# rates
model_coal_rate_gen <- linear_model(indep_temp_coal, "rate_coal_gen_5y", 
                                full_data_rate,cvar_temp_coal,
                                "5-year rolling rate of coal generation decline (% per year)","")
model_coal_rate_cap <- linear_model(indep_temp_coal, "rate_coal_cap_5y", 
                                    full_data_rate,cvar_temp_coal,
                                    "5-year rolling rate of coal capacity decline (% per year)","")
model_coal_rate_share <- linear_model(indep_temp_coal, "rate_coal_share_5y", 
                                    full_data_rate,cvar_temp_coal,
                                    "5-year rolling rate of coal share decline (% per year)","")
model_gas_rate_gen <- linear_model(indep_temp_coal, "rate_gas_gen_5y", 
                                    full_data_rate,cvar_temp_coal,
                                    "5-year rolling rate of gas generation decline (% per year)","")
model_gas_rate_share <- linear_model(indep_temp_coal, "rate_gas_share_5y", 
                                      full_data_rate,cvar_temp_coal,
                                      "5-year rolling rate of gas share decline (% per year)","")


##########
#investigate trade relationships
# gas trade  
indep_trade_gas <- list("NetValue_Gas_Import",
                    "NetQuantity_Gas_Import",
                    "HHI_Gas_Import", 
                    "agree_Gas_Import", 
                    "sanction_Gas_Import",
                    "common_language_Gas_Import",
                    "colony_Gas_Import", 
                    "HHI_Gas_Export",
                    "agree_Gas_Export", 
                    "sanction_Gas_Export",
                    "common_language_Gas_Export",
                    "colony_Gas_Export")
cvar_trade_gas <- c("Gas net import value" ,
                  "Gas net import quantity" ,
                "Gas import trade diversity",
                   "Gas import share of trade agreements" , 
                   "Gas import share of trade sanctions" ,
                   "Gas import share of common language",
                   "Gas import share from colonizers", 
                   "Gas export trade diversity",
                "Gas export share of trade agreements" , 
                "Gas export share of trade sanctions" ,
                "Gas export share of common language",
                "Gas export share from colonizers")
model_gas_gen_trade <- linear_model(indep_trade_gas, "Gas_Gen_Decline_Perc", 
                              full_data,cvar_trade_gas,
                              "Share of gas decline in peak generation","_trade")

# coal trade 
indep_trade_coal <- list("NetValue_Coal_Import",
                        "NetQuantity_Coal_Import",
                        "HHI_Coal_Import", 
                        "agree_Coal_Import", 
                        "sanction_Coal_Import",
                        "common_language_Coal_Import",
                        "colony_Coal_Import", 
                        "HHI_Coal_Export",
                        "agree_Coal_Export", 
                        "sanction_Coal_Export",
                        "common_language_Coal_Export",
                        "colony_Coal_Export")
cvar_trade_coal <- c("Coal net import value" ,
                    "Coal net import quantity" ,
                    "Coal import trade diversity",
                    "Coal import share of trade agreements" , 
                    "Coal import share of trade sanctions" ,
                    "Coal import share of common language",
                    "Coal import share from colonizers", 
                    "Coal export trade diversity",
                    "Coal export share of trade agreements" , 
                    "Coal export share of trade sanctions" ,
                    "Coal export share of common language",
                    "Coal export share from colonizers")
model_coal_gen_trade <- linear_model(indep_trade_coal, "Coal_Gen_Decline_Perc", 
                                    full_data,cvar_trade_coal,
                                    "Share of coal decline in peak generation","_trade")

# RESOURCES
cvar_resource <- c("Coal Rents",
                       "Oil Rent",
                       "NG Rents",
                       "Natural gas production",
                       "Natural gas reserve",
                       "Coal production" ,
                       "Coal reserve",
                       "Log Natural gas RPR",
                       "Coal RPR")
indep_resource <- list("WDI_CoalRents_.",
                   "WDI_OilRents_.",
                   "WDI_NGRents_.",
                   "BP_GasProduction_bM3",
                   "BP_GasReserve_tM3",
                   "BP_CoalProduction_mTon", 
                   "BP_CoalReserve_mTon", 
                   "LogBP_GasR2P_yr",
                   "BP_CoalR2P_yr")
model_coal_gen_resource <- linear_model(indep_resource, "Coal_Gen_Decline_Perc", 
                                     full_data,cvar_resource,
                                     "Share of coal decline in peak generation","_resource")
model_gas_gen_resource <- linear_model(indep_resource, "Gas_Gen_Decline_Perc", 
                                        full_data,cvar_resource,
                                        "Share of gas decline in peak generation","_resource")

# POLICY
indep_policy <- list("num_supply_policy","binary_airqual","ghg_ambition_n",	"cw_2030",
                     "annex_one", "annex_two","R_Choice")
cvar_policy <- c("Number of energy supply policies",
                 "Air quality standards","NDC ambition","2030 ambition update",
                 "Annex I","Annex II", "Choice of supplier")
model_coal_gen_policy <- linear_model(indep_policy, "Coal_Gen_Decline_Perc", 
                                        full_data,cvar_policy,
                                        "Share of coal decline in peak generation","_policy")
model_gas_gen_policy <- linear_model(indep_policy, "Gas_Gen_Decline_Perc", 
                                       full_data,cvar_policy,
                                       "Share of gas decline in peak generation","_policy")

#######
#  SURVEY RESULTS
indep_temp_coal_survey <- list(
  "logGDPpc",
  "R_Choice",
  c("logGDPpc", "R_Choice", "WDI_Coal_El_.","WDI_NG_El_.", "Coal_duration_year",
    "Gas_duration_year", "Coal_logCurrent_MW","Gas_logCurrent_MW",#"BP_GasR2P_yr",
    "HHI_Coal_Import","HHI_Gas_Import","NetValue_Coal_Import","NetValue_Gas_Import",
    "num_supply_policy"),
  c("logGDPpc", "R_Choice", "WDI_Coal_El_.","WDI_NG_El_.", "Coal_duration_year",
    "Gas_duration_year", "Coal_logCurrent_MW","Gas_logCurrent_MW",#"BP_GasR2P_yr",
    "HHI_Coal_Import","HHI_Gas_Import","NetValue_Coal_Import","NetValue_Gas_Import",
    "num_supply_policy","Survey_Belief_."),
  "Survey_Belief_."
)
cvar_temp_coal_survey <- c("Log GDP per capita",
                           "Choice of supplier",
                           "Coal share in electricity",
                           "Gas share in electricity",
                           "Years since first coal plant",
                           "Years since first gas plant",
                           "Log current coal capacity",
                           "Log current gas capacity",
                           #"Gas reserve production ratio",
                           "Coal import trade diversity",
                           "Gas import trade diversity",
                           "Coal net import value",
                           "Gas net import value",
                           "Number of energy supply policies",
                           "Climate change belief")

model_coal_phaseout_survey <- linear_model(indep_temp_coal_survey, "Coal_Share_PhasedOut", 
                                           full_data_survey, cvar_temp_coal_survey,
                                           "Share of coal phase out in peak capacity (2021-2022)","_survey")
model_coal_gen_survey <- linear_model(indep_temp_coal_survey, "Coal_Gen_Decline_Perc", 
                                      full_data_survey,cvar_temp_coal_survey,
                                        "Share of coal decline in peak generation (2021-2022)","_survey")
model_gas_gen_survey <- linear_model(indep_temp_coal_survey, "Gas_Gen_Decline_Perc", 
                                     full_data_survey,cvar_temp_coal_survey,
                                       "Share of gas decline in peak generation (2021-2022)","_survey")


# VISUALIZATION 
variable_mapping <- f_map_variables()
png(file = file.path(model_dir,"coalgas_temporal_models_plusgen.png"), width = 200, height = 100,
    units = "mm", res = 600, bg = "transparent")
plot_summs(model_coal_phaseout[[7]],model_coal_gen[[7]], model_gas_gen[[7]],
           coefs = variable_mapping,
           model.names=c("Coal capacity phase out","Coal generation decline","Gas generation decline"))
dev.off()

#png(file = file.path(current_dir,"coalgas_temporal_models_recent.png"), width = 200, height = 80,
#    units = "mm", res = 600, bg = "transparent")
#plot_summs(model_coal_ret_survey[[5]], model_coal_tran_survey[[5]],
#           model_coal_phaseout_survey[[5]],model_gas_ret_survey[[5]], 
#           coefs = variable_mapping,
#           model.names=c("Coal Retirement","Coal Transition","Coal Phase Out","Gas Retirement"))
#dev.off()

# EXPORT MODEL ENSEMBLES

m1 <- model_coal_phaseout[[1]]
m2 <- model_coal_phaseout[[2]]
m3 <- model_coal_phaseout[[3]]
m4 <- model_coal_phaseout[[4]]
m5 <- model_coal_phaseout_survey[[5]]
m6 <- model_coal_gen[[1]]
m7 <- model_coal_gen[[2]]
m8 <- model_coal_gen[[3]]
m9 <- model_coal_gen[[4]]
m10 <- model_coal_gen_survey[[5]]
m11 <- model_gas_gen[[1]]
m12 <- model_gas_gen[[2]]
m13 <- model_gas_gen[[3]]
m14 <- model_gas_gen[[4]]
m15 <- model_gas_gen_survey[[5]]
stargazer(m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,
          type="html",
          align=TRUE,
          dep.var.labels=c(#"Share of coal phase out in peak capacity",
                           "Share of coal decline in peak generation",
                           "Share of gas decline in peak generation"), 
          covariate.labels=c("Log GDP per capita",
                             "Annex I",
                             "Choice of supplier",
                             "Number of energy supply policies",
                             "Climate change belief"),
          keep.stat=c("rsq", "adj.rsq","n"),
          out=file.path(model_dir,"test.html"))


##########
# side correlations
indep_temp_belief <- list(
  "logGDPpc",
  "R_Choice",
  "WDI_Coal_El_.",
  c("Coal_duration_year", "Coal_logCurrent_MW"),
  c("logGDPpc", "R_Choice", "WDI_Coal_El_.","Coal_duration_year", "Coal_logCurrent_MW")
)
cvar_temp_belief <- c("Log GDP per capita","Choice of supplier",
                          "Coal share in electricity",
                          "Years since first coal plant",
                          "Log current coal capacity")
model_belief <- linear_model(indep_temp_belief, "Survey_Belief_.", 
                                     full_data_survey, cvar_temp_belief,
                                     "Climate change belief","_recent")

