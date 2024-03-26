library(corrplot)
library(car)
library(MASS)
library(olsrr)
library(dplyr)
library(Amelia)

setwd("C:/Users/jx920/OneDrive - Imperial College London/1 - Projects/social_political_institutional")
current_dir <-"analysis/temporal_country_models"
imp_dir <-"analysis/temporal_country_models/imputation"
source(file.path(current_dir,"helper_function.R"))

#=================== import data file
data_path <- file.path(current_dir,"coalgas_data_truncate.csv")
truncated_data <- read.csv(data_path)
# set the first column as row name then delete
rownames(truncated_data) <- truncated_data$X
truncated_data <- truncated_data[,-1]
# listwise deletion data
listwise_data <- read.csv(file.path(current_dir,"coal_data_deletion.csv"))
rownames(listwise_data) <- listwise_data$X
listwise_data <- listwise_data[,-1]

#=================== prepare Amelia run
### binary variables
binary_col <- sapply(truncated_data, is_binary)
binary_col_names <- names(truncated_data)[binary_col]
### proportion variables 
# the ones within [0,1]
#proportion_col_names <- c("HHI_LNG_Import", "contiguity_LNG_Import", "agree_LNG_Import",
#                          "sanction_LNG_Import", "common_language_LNG_Import", "colony_LNG_Import",
#                         "HHI_LNG_Export", "contiguity_LNG_Export",
#                          "agree_LNG_Export", "sanction_LNG_Export", "common_language_LNG_Export",
#                          "colony_LNG_Export")
proportion_col_names <- c("HHI_Gas_Import","contiguity_Gas_Import","agree_Gas_Import",
                          "sanction_Gas_Import","common_language_Gas_Import","colony_Gas_Import",
                          "HHI_Gas_Export","contiguity_Gas_Export","agree_Gas_Export",
                          "sanction_Gas_Export","common_language_Gas_Export",
                          "colony_Gas_Export","HHI_Coal_Import",
                          "contiguity_Coal_Import","agree_Coal_Import",
                          "sanction_Coal_Import","common_language_Coal_Import",
                          "colony_Coal_Import","HHI_Coal_Export","contiguity_Coal_Export",
                          "agree_Coal_Export","sanction_Coal_Export",
                          "common_language_Coal_Export","colony_Coal_Export",
                          "Share_VRE_Gen")


num_cols <- length(proportion_col_names)
column_indices <- which(colnames(truncated_data) %in% proportion_col_names)
# the ones within [0,100]
percentage_col_name <- c('WDI_Manu_GDP_.', 'WDI_CoalRents_.',
                         'WDI_OilRents_.', 'WDI_NGRents_.', 'WDI_Coal_El_.', 'WDI_NG_El_.')
num_cols2 <- length(percentage_col_name)
column_indices2 <- which(colnames(truncated_data) %in% percentage_col_name)
# set the bounds 
bds <- matrix(c(column_indices, column_indices2, rep(0, each = (num_cols+num_cols2)), 
                rep(1, each = num_cols),rep(100, each = num_cols2)), nrow = (num_cols+num_cols2), ncol = 3)
# right skewed variables 
# these require log-linear transformations
log_col_names <- c('BP_GasReserve_tM3', 'BP_GasProduction_bM3',
              'BP_CoalProduction_mTon', 'BP_CoalReserve_mTon', #'BP_GasR2P_yr',
              'BP_CoalR2P_yr')

#=================== use Amelia for multiple imputation
amelia_attempt <- amelia(truncated_data, m = 5, p2s = 1, ts = "year",cs="Country.Code",
                         noms = binary_col_names, 
                         logs = log_col_names,
                         bounds = bds)
save(amelia_attempt, file = file.path(imp_dir,"amelia_attempt.RData"))
write.csv(amelia_attempt$imputations$imp1, file.path(imp_dir,"coal_data_amelia_imp1.csv"))
write.csv(amelia_attempt$imputations$imp2, file.path(imp_dir,"coal_data_amelia_imp2.csv"))
write.csv(amelia_attempt$imputations$imp3, file.path(imp_dir,"coal_data_amelia_imp3.csv"))
write.csv(amelia_attempt$imputations$imp4, file.path(imp_dir,"coal_data_amelia_imp4.csv"))
write.csv(amelia_attempt$imputations$imp5, file.path(imp_dir,"coal_data_amelia_imp5.csv"))


all_amelia_data <- data.frame()
for (num in 1:5) {
  amelia_imp_data <- amelia_attempt$imputations[[paste0("imp", num)]]
  if (num == 1) {
    all_amelia_data <- amelia_imp_data
  } else {
    all_amelia_data <- bind_rows(all_amelia_data, amelia_imp_data)
  }
}
all_amelia_data <- all_amelia_data %>%
  group_by(Country.Code, year) %>%
  summarise_all(mean) %>%
  unite("cy", Country.Code:year,sep = "-") %>%
  column_to_rownames(var = "cy")
write.csv(all_amelia_data, file.path(imp_dir,"coal_data_amelia_impAll.csv"))

#=================== visualize imputed data
# plot missingness
png(file = file.path(imp_dir,"coal_data_amelia_missingness.png"), width = 400, height = 400,
    units = "mm", res = 600, bg = "transparent")
missmap(amelia_attempt)
dev.off()

# correlation
indep_corr <- f_corr(all_amelia_data,current_dir,"coalgas_data_imputation_correlation")