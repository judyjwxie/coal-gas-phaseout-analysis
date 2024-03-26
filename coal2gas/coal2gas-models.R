library(corrplot)
library(car)
library(tidyverse)
library(jtools)
library(memisc)

##### Prepare dataset
# load independent variables or predictors file
indep_data <- read.csv("analysis/indep_var_compiled.csv")
indep_data <- indep_data %>% column_to_rownames(., var = "Country.Code")
indep_data_noname <- subset(indep_data, select = -c(R_8,
                                                    Region, IncomeGroup, Hanson_Name, BP_Name,
                                                    Meta_Name, WEF_Name, Henisz_Name, Reform_Name,
                                                    TableName, GEM_Name, StateCapacity_sd))
region_data <- read.csv("data/MESSAGE/region_binary.csv")
region_data <- region_data %>% column_to_rownames(., var = "Country.Code")
# load dependent variables or response variables file
depvar_data <- read.csv("data/global_GEM/coal2gas_indicators.csv")
depvar_data <- depvar_data %>% column_to_rownames(., var = "Country.Code")
depvar_data_select <- subset(depvar_data, select = c(IND_Perc_Transition_MW, 
                                                     IND_Perc_RetOnly_MW, IND_Perc_Transition_vs_Ret))
# complete dataset
dep_compile_1 <- cbind(indep_data_noname, depvar_data_select[rownames(indep_data_noname),])
dep_compile <- cbind(dep_compile_1, region_data[rownames(dep_compile_1),])
dep_compile <- mutate(dep_compile, across(starts_with("R11"), ~ifelse(is.na(.x),0,.x)))

##### Test regression model 
# Transition MW on peak capacity
indep_m1 <- na.omit(subset(dep_compile, select = c(IND_Perc_Transition_MW, R_Choice)))
dep_m1 <- lm(IND_Perc_Transition_MW ~ ., data = indep_m1)
indep_m2 <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(StateCapacity)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_MW))))
dep_m2 <- lm(IND_Perc_Transition_MW ~ ., data = indep_m2)
indep_m3 <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(GCI4)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_MW))))
dep_m3 <- lm(IND_Perc_Transition_MW ~ ., data = indep_m3)
indep_m4 <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(GDPpc)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_MW))))
dep_m4 <- lm(IND_Perc_Transition_MW ~ ., data = indep_m4)
indep_m5 <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(WDI_Coal_El_Perc)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_MW,R_Choice))))
dep_m5 <- lm(IND_Perc_Transition_MW ~ ., data = indep_m5)
indep_m6 <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(WDI_Coal_El_Perc,StateCapacity)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_MW))))
dep_m6 <- lm(IND_Perc_Transition_MW ~ ., data = indep_m6)
indep_m7 <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(WDI_Coal_El_Perc,WDI_NG_El_Perc,Survey_Worry)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_MW,R_Choice))))
dep_m7 <- lm(IND_Perc_Transition_MW ~ ., data = indep_m7)
indep_m7b <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(WDI_Coal_El_Perc,WDI_NG_El_Perc,Survey_Worry,WG_Voice)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_MW,R_Choice))))
dep_m7b <- lm(IND_Perc_Transition_MW ~ ., data = indep_m7b)

export_summs(dep_m1, dep_m2,dep_m3,dep_m4,dep_m5,dep_m6,dep_m7,dep_m7b,
             statistics = c(N = "nobs", R2 = "r.squared", "AIC"))


# Transition over total phase out
indep_n1 <- na.omit(subset(dep_compile, select = c(IND_Perc_Transition_vs_Ret, R_Choice)))
dep_n1 <- lm(IND_Perc_Transition_vs_Ret ~ ., data = indep_n1)
indep_n1a <- na.omit(subset(dep_compile, select = c(IND_Perc_Transition_vs_Ret, R_Liberalization)))
dep_n1a <- lm(IND_Perc_Transition_vs_Ret ~ ., data = indep_n1a)
indep_n1b <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(StateCapacity)))),
                           subset(dep_compile, select = c(IND_Perc_Transition_vs_Ret))))
dep_n1b <- lm(IND_Perc_Transition_vs_Ret ~ ., data = indep_n1b)
indep_n1c <- na.omit(subset(dep_compile, select = c(IND_Perc_Transition_vs_Ret, R_Choice,R_Liberalization)))
dep_n1c <- lm(IND_Perc_Transition_vs_Ret ~ ., data = indep_n1c)

indep_n2 <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(WDI_NG_El_Perc,WDI_Coal_El_Perc,WDI_NGRents,Survey_Worry)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_vs_Ret,R_Choice,R_Liberalization))))
dep_n2 <- lm(IND_Perc_Transition_vs_Ret ~ ., data = indep_n2)
indep_n2a <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(WDI_NG_El_Perc,WDI_NGRents,Survey_Worry)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_vs_Ret,R_Liberalization))))
dep_n2a <- lm(IND_Perc_Transition_vs_Ret ~ ., data = indep_n2a)
indep_n2b <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(WDI_NG_El_Perc,WDI_NGRents,Survey_Worry,WG_Voice)))),
                           subset(dep_compile, select = c(IND_Perc_Transition_vs_Ret,R_Choice,R_Liberalization))))
dep_n2b <- lm(IND_Perc_Transition_vs_Ret ~ ., data = indep_n2b)
print(summary(dep_n2))

export_summs(dep_n1, dep_n1a,dep_n1b,dep_n1c,dep_n2,dep_n2a,dep_n2b,
             statistics = c(N = "nobs", R2 = "r.squared", "AIC"))

# retirement over peak capacity
indep_r1 <- na.omit(subset(dep_compile, select = c(IND_Perc_RetOnly_MW, R_Choice)))
dep_r1 <- lm(IND_Perc_RetOnly_MW ~ ., data = indep_r1)
indep_r2 <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(StateCapacity)))),
                          subset(dep_compile, select = c(IND_Perc_RetOnly_MW))))
dep_r2 <- lm(IND_Perc_RetOnly_MW ~ ., data = indep_r2)
indep_r2a <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(WDI_Coal_El_Perc,WDI_NG_El_Perc,Survey_Worry)))),
                          subset(dep_compile, select = c(IND_Perc_RetOnly_MW,R_Choice))))
dep_r2a <- lm(IND_Perc_RetOnly_MW ~ ., data = indep_r2a)
indep_r2b <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(WDI_Coal_El_Perc,WDI_NG_El_Perc,Survey_Worry,StateCapacity)))),
                           subset(dep_compile, select = c(IND_Perc_RetOnly_MW))))
dep_r2b <- lm(IND_Perc_RetOnly_MW ~ ., data = indep_r2b)
print(summary(dep_r2b))

export_summs(dep_r1, dep_r2,dep_r2a,dep_r2b,
             statistics = c(N = "nobs", R2 = "r.squared", "AIC"))

plot_summs(dep_m7,dep_r2a,model.names=c("Transition","Retirement"))


## Liberalization index
indep_l1 <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(R_Choice)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_MW))))
dep_l1 <- lm(IND_Perc_Transition_MW ~ ., data = indep_l1)
indep_l2 <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(StateCapacity)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_MW))))
dep_l2 <- lm(IND_Perc_Transition_MW ~ ., data = indep_l2)
indep_l3 <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(GCI_Innovation)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_MW))))
dep_l3 <- lm(IND_Perc_Transition_MW ~ ., data = indep_l3)
indep_l4 <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(WG_Voice)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_MW))))
dep_l4 <- lm(IND_Perc_Transition_MW ~ ., data = indep_l4)
indep_l4a <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(WG_Voice)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_MW))))
dep_l4a <- lm(IND_Perc_Transition_MW ~ ., data = indep_l4a)
indep_l5 <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(R_Choice)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_MW,R11_SAS,R11_AFR,R11_EEU,R11_WEU,
                                                         R11_MEA,R11_LAM,R11_FSU,R11_PAS,R11_PAO,
                                                         R11_NAM,R11_CPA))))
dep_l5 <- lm(IND_Perc_Transition_MW ~ ., data = indep_l5)
indep_l6<- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(StateCapacity)))),
                          subset(dep_compile, select = c(IND_Perc_Transition_MW,R11_SAS,R11_AFR,R11_EEU,R11_WEU,
                                                         R11_MEA,R11_LAM,R11_FSU,R11_PAS,R11_PAO,
                                                         R11_NAM,R11_CPA))))
dep_l6 <- lm(IND_Perc_Transition_MW ~ ., data = indep_l6)
indep_l7<- na.omit(subset(dep_compile, select = c(IND_Perc_Transition_MW,R11_SAS,R11_AFR,R11_EEU,R11_WEU,
                                                        R11_MEA,R11_LAM,R11_FSU,R11_PAS,R11_PAO,
                                                        R11_NAM,R11_CPA)))
dep_l7 <- lm(IND_Perc_Transition_MW ~ ., data = indep_l7)

export_summs(dep_l1, dep_l2,dep_l3,dep_l4,dep_l5,dep_l6,dep_l7,
             statistics = c(N = "nobs", R2 = "r.squared", "AIC"))


## Liberalization index
indep_l1b <- na.omit(subset(dep_compile, select = c(IND_Perc_RetOnly_MW,R_Liberalization,R_Choice)))
dep_l1b <- lm(IND_Perc_RetOnly_MW ~ ., data = indep_l1b)
indep_l2b <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(StateCapacity)))),
                          subset(dep_compile, select = c(IND_Perc_RetOnly_MW))))
dep_l2b <- lm(IND_Perc_RetOnly_MW ~ ., data = indep_l2b)
indep_l3b <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(GCI_Innovation)))),
                          subset(dep_compile, select = c(IND_Perc_RetOnly_MW))))
dep_l3b <- lm(IND_Perc_RetOnly_MW ~ ., data = indep_l3b)
indep_l4b <- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(WG_Voice)))),
                          subset(dep_compile, select = c(IND_Perc_RetOnly_MW))))
dep_l4b <- lm(IND_Perc_RetOnly_MW ~ ., data = indep_l4b)

indep_l5b <- na.omit(subset(dep_compile, select = c(IND_Perc_RetOnly_MW,R11_SAS,R11_AFR,R11_EEU,R11_WEU,
                                                         R11_MEA,R11_LAM,R11_FSU,R11_PAS,R11_PAO,
                                                         R11_NAM,R11_CPA,R_Choice,R_Liberalization)))
dep_l5b <- lm(IND_Perc_RetOnly_MW ~ ., data = indep_l5b)
indep_l6b<- na.omit(cbind(data.frame(scale(subset(dep_compile, select = c(StateCapacity)))),
                         subset(dep_compile, select = c(IND_Perc_RetOnly_MW,R11_SAS,R11_AFR,R11_EEU,R11_WEU,
                                                        R11_MEA,R11_LAM,R11_FSU,R11_PAS,R11_PAO,
                                                        R11_NAM,R11_CPA))))
dep_l6b <- lm(IND_Perc_RetOnly_MW ~ ., data = indep_l6b)
indep_l7b<- na.omit(subset(dep_compile, select = c(IND_Perc_RetOnly_MW,R11_SAS,R11_AFR,R11_EEU,R11_WEU,
                                                  R11_MEA,R11_LAM,R11_FSU,R11_PAS,R11_PAO,
                                                  R11_NAM,R11_CPA)))
dep_l7b <- lm(IND_Perc_RetOnly_MW ~ ., data = indep_l7b)

export_summs(dep_l1b,dep_l2b,dep_l5b,dep_l6b,dep_l7b,
             statistics = c(N = "nobs", R2 = "r.squared", "AIC"))

plot_summs(dep_l7,dep_l7b,model.names=c("Transition","Retirement"))
