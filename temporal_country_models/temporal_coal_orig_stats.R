library(corrplot)
library(tidyverse)
library(car)
library(MASS)
library(olsrr)
library(dplyr)
library(jtools)
library(memisc)
library(stargazer)
library(skimr) # for summary statistics of data set 
library(extracat) # for visualizing missing data 

setwd("C:/Users/jx920/OneDrive - Imperial College London/1 - Projects/social_political_institutional")
current_dir <-"analysis/temporal_country_models"
source(file.path(current_dir,"helper_function.R"))

# LOAD DATASET
### full dataset
dep_data_path <- file.path(current_dir,"_all_temporal_coalgas_var.csv")
dep_data <- read.csv(dep_data_path)
indep_data_path <- file.path(current_dir,"_all_temporal_indep_var.csv")
indep_data <- read.csv(indep_data_path)
merged_data <- merge(dep_data, indep_data, by = c("Country.Code", "year"), all.x = TRUE)
merged_data <- merged_data[!duplicated(merged_data), ]

rownames(merged_data) <- do.call(paste,c(merged_data[c("Country.Code","year")],sep="-"))
f_vis_missing(merged_data,current_dir,"coal_data_full")
full_data_filter <- merged_data %>% filter(year > 1994)
sink(file.path(current_dir,"coalgas_table_summarystats.txt"))
skim(full_data_filter)
sink()
# visualize missing data
f_vis_missing(full_data_filter,current_dir,"coalgas_data_filter")

#### drop some incomplete variables
# this is thru qualitatively evaluating the the coal_visna_full.png file
# some obvious ones with multi-collinearity 
# the dropped variables are completely not used in further analysis 
# can re-evaluate whether they should be completely dropped
full_data_truncate <- subset(full_data_filter, select = -c(Survey_Worry_.,Survey_Belief_.,	Survey_FutureGen_.,	Survey_Personal_.,
                                                           Survey_FossilMore_.,	Survey_FossilLess_.,
                                                   l1,l2,j,f,aligne_l1,aligne_l2,alignl1_l2,aligne_j,WDI_Fossil_El_.,
                                                   WG_GovEff, WG_Reg, WG_Voice, tax_inc_tax,tax_trade_tax,taxrev_gdp,
                                                   # above are ones with many missing data 
                                                   # above are correlated with other governance or capacity variables
                                                   GDPpc_2017.,Coal_Peak_MW,Coal_Current_MW, 
                                                   Coal_Ret_MW, Coal_EarlyRet_MW, Coal_Tran_MW, StateCapacity_sd,
                                                   Gas_Peak_MW,Gas_Ret_MW,Gas_EarlyRet_MW,Gas_Share_PhasedOut,
                                                   Gas_Share_EarlyRet,Gas_Share_Ret,BP_GasR2P_yr,
                                                  # not used 
                                                  Share_Solar_Gen,Share_Wind_Gen,Share_Nuclear_Gen,Share_Coal_Gen,Share_Gas_Gen,
                                                  # these are not used in the imputation, but added later
                                                  rate_coal_gen_5y,rate_coal_share_5y,rate_coal_cap_5y,rate_gas_gen_5y,rate_gas_share_5y
                                                  
                                                  )) 
sink(file.path(current_dir,"coalgas_table_summarystats_truncate.txt"))
skim(full_data_truncate)
sink()
write.csv(full_data_truncate, file.path(current_dir,"coalgas_data_truncate.csv"))
# visualize missing data
f_vis_missing(full_data_truncate,current_dir,"coalgas_data_truncate")

# STANDARDIZE DATASET
# LIST-WISE DELETION
# when there are NA in the data set, list-wise deletion was used
lw_deletion <- f_listwise_delete(full_data_truncate)
truncate_data_filter <- lw_deletion$data1
truncate_data_out <- lw_deletion$data2
write.csv(truncate_data_filter, file.path(current_dir,"coal_data_deletion.csv"))
truncate_data_std <- f_standardize_df(subset(truncate_data_filter, select = -c(Country.Code)))
write.csv(truncate_data_std, file.path(current_dir,"coal_data_deletion_std.csv"))
# correlation
indep_corr <- f_corr(truncate_data_std,current_dir,"coal_data_deletion_correlation")

