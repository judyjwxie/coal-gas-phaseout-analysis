library(lavaan)
library(lavaanPlot)

current_dir <- "analysis/temporal_country_models"
#sem_dir <- "analysis/temporal_country_models/feature_selection"
source(file.path(current_dir,"helper_function.R"))

### LOAD DATA
# import data file
data_path <- file.path(current_dir,"/coal_data_deletion.csv")
coal_data <- read.csv(data_path)
# set the first column as row name then delete
rownames(coal_data) <- coal_data$X
coal_data <- coal_data[,-1:-2]
coal_data_std <- f_standardize_df(coal_data)

m1 <- '
  Coal_Share_Ret ~ 1 + Coal_duration_year + Coal_logCurrent_MW + R_WholeSale + R_Choice  + Value_LNG_Import 
  Coal_Share_PhasedOut ~ 1 + Coal_duration_year + Coal_logCurrent_MW + R_WholeSale + R_Choice  + Value_LNG_Import 
  R_WholeSale ~~ R_Choice

  '
fitm1 <- sem(m1, data=coal_data_std)
summary(fitm1)
lavaanPlot(model = fitm1, node_options = list(fontname = "Helvetica"), edge_options = list(color = "grey"), 
           coefs = TRUE, covs = TRUE, stars = "regress",digits = 2)
