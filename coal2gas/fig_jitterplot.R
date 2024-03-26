library(ggplot2)
library(ggrepel)
library(scales)
library(car)
library(tidyverse)

##### Prepare dataset
depvar_data <- read.csv("data/global_GEM/coal2gas_indicators.csv")
depvar_data <- depvar_data %>% column_to_rownames(., var = "Country.Code")
depvar_data_select <- subset(depvar_data, select = c(IND_Perc_Transition_MW, IND_Perc_RetOnly_MW, Max_Cap_MW,IND_Perc_Transition_vs_Ret))
indep_data <- read.csv("analysis/indep_var_compiled.csv")
indep_data <- indep_data %>% column_to_rownames(., var = "Country.Code")
indep_data_noname <- subset(indep_data, select = -c(R_8,
                                                    Region, IncomeGroup, Hanson_Name, BP_Name,
                                                    Meta_Name, WEF_Name, Henisz_Name, Reform_Name,
                                                    TableName, StateCapacity_sd))
data_compile <- cbind(indep_data_noname, depvar_data_select[rownames(indep_data_noname),])
data_compile$Max_Cap_TW <- data_compile$Max_Cap_MW/1000000

select_data <- na.omit(subset(data_compile, select = c(IND_Perc_Transition_MW,IND_Perc_RetOnly_MW, R_Choice,GEM_Name,Max_Cap_TW,IND_Perc_Transition_vs_Ret )))

select_data_ret <- subset(select_data, IND_Perc_RetOnly_MW!=0)
select_data_ret_zeros <- subset(select_data, R_Choice==0 & IND_Perc_RetOnly_MW==0)
ret_zero <- paste(unlist(list(select_data_ret_zeros$GEM_Name)), collapse="\n")
p1 <- ggplot(select_data_ret, aes(x=R_Choice,y=IND_Perc_Transition_vs_Ret, label = GEM_Name,size = Max_Cap_TW)) + 
  geom_jitter(position = position_jitter(seed = 0.5)) + 
  geom_text_repel(position = position_jitter(seed = 0.5),size = 3) +
  xlab("Choice of power suppliers") + ylab("Share of transitioned coal capacity in phase-out capacity (%)") +
  scale_x_continuous(breaks= c(0,1)) +
  ylim(-2,102) +
  guides(size = guide_legend("Peak Capacity (TW)"))
p1 


select_data_tran <- subset(select_data, R_Choice!=0 | IND_Perc_Transition_MW!=0)
select_data_tran_zeros <- subset(select_data, R_Choice==0 & IND_Perc_Transition_MW==0)
p2 <- ggplot(select_data_tran, aes(x=R_Choice,y=IND_Perc_Transition_MW, label = GEM_Name,size = Max_Cap_TW)) + 
  geom_jitter(position = position_jitter(seed = 0.5)) + 
  geom_text_repel(position = position_jitter(seed = 0.5),size = 3) +
  xlab("Choice of power suppliers") + ylab("Share of transitioned coal capacity in peak capacity (%)") +
  scale_x_continuous(breaks= c(0,1)) +
  ylim(-2,102) +
  guides(size = guide_legend("Peak Capacity (TW)"))
p2 
