library(corrplot)
library(car)
library(MASS)
library(dplyr)
library(ggplot2)
library(gridExtra)

# CHECK FOR BINARY COLUMNS
is_binary <- function(x) all(unique(x) %in% c(0, 1, NA))

# LIST-WISE DELETION
# input: complete data set
# output: remaining and deleted data set after list-wise deletion
f_listwise_delete <- function(full_data){
  # drop values that don't have the full set of data
  full_data_clean <- na.omit(full_data)
  # show the values that were dropped
  full_data2 <- full_data %>% mutate(RowName = rownames(.))
  full_data_clean2 <- full_data_clean %>% mutate(RowName = rownames(.))
  rows_not_in <- anti_join(full_data2, full_data_clean2,by="RowName")
  return(list(data1 = full_data_clean, data2 = rows_not_in))
}

# STANDARDIZE DATASET
# input: selected data to standardize
# output: the standardized data set 
f_standardize_df <- function(selected_data){
  # check for binary data
  # separate them out, as they don't need to be standardized
  binary_col <- sapply(selected_data, is_binary)
  indep_data_binary <- selected_data[, binary_col]
  indep_data_continuous <- selected_data[, !binary_col]
  # standardize the dataset
  indep_data_std <- cbind(data.frame(scale(indep_data_continuous)), indep_data_binary)
  return(indep_data_std)
}

# CORRELATION OF VARIABLES
# input: standardized data set and dropped na, directory name, export data file names
# output: correlation matrix
# export: the correlation plot and matrix data 
f_corr <- function(std_data,dir_path,fname){
  indep_corr <- cor(std_data)
  png_fname <- paste0(fname,".png")
  csv_fname <- paste0(fname,".csv")
  png(file = file.path(dir_path,png_fname),width = 300, height = 300,
      units = "mm", res = 600, bg = "transparent")
  corrplot(indep_corr, method = "color", type = "lower",
           diag = FALSE, tl.col = "black")
  dev.off()
  write.csv(indep_corr, file.path(dir_path,csv_fname))
  return(indep_corr)
}

# VISUALIZE MISSING DATA
# code from https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html 
f_vis_missing <- function(df,dir_path,fname){
  png_fname <- paste0(fname,"_missing_data.png")
  missing.values <- df %>%
    gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    group_by(key) %>%
    mutate(total = n()) %>%
    group_by(key, total, isna) %>%
    summarise(num.isna = n()) %>%
    mutate(pct = num.isna / total * 100)
  
  levels <-
    (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
  
  percentage.plot <- missing.values %>%
    ggplot() +
    geom_bar(aes(x = reorder(key, desc(pct)), 
                 y = pct, fill=isna), 
             stat = 'identity', alpha=0.8) +
    scale_x_discrete(limits = levels) +
    scale_fill_manual(name = "", 
                      values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
    coord_flip() +
    labs(title = "Percentage of missing values", x =
           'Variable', y = "% of missing values") +
    theme(legend.position = "none")
    
  
  row.plot <- df %>%
    mutate(id = row_number()) %>%
    gather(-id, key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    ggplot(aes(key, id, fill = isna)) +
    geom_raster(alpha=0.8) +
    scale_fill_manual(name = "",
                      values = c('steelblue', 'tomato3'),
                      labels = c("Present", "Missing")) +
    scale_x_discrete(limits = levels) +
    labs(x = "Variable",
         y = "Row Number", title = "Missing values in rows") +
    coord_flip() +
    theme(axis.title.y = element_blank(),  # Remove y-axis labels
          axis.text.y = element_blank())  # Remove y-axis tick labels
  
  png(file = file.path(dir_path,png_fname), width = 200, height = 200,
      units = "mm", res = 600, bg = "transparent")
  grid.arrange(percentage.plot, row.plot, ncol = 2)
  dev.off()
}

# VARIABLE MAPPING
f_map_variables <- function(){
  variable_mapping <- c(
    "Current Year" = "year", 
    "Year of first coal plant" = "Coal_start_year", 
    "Year of first gas plant" = "Gas_start_year", 
    "Coal Annual Emissions" = "Coal_AnnualMtCO2", 
    "Gas Annual Emissions" = "Gas_AnnualMtCO2", 
    "Years since first coal plant" = "Coal_duration_year", 
    "Years since first gas plant" = "Gas_duration_year", 
    "Current coal capacity (log)" = "Coal_logCurrent_MW", 
    "Current gas capacity (log)" = "Gas_logCurrent_MW", 
    "Coal Rents" = "WDI_CoalRents_.",
    "Oil Rent" = "WDI_OilRents_.",
    "NG Rents" = "WDI_NGRents_.", 
    "% Coal in Electricity" = "WDI_Coal_El_.",
    "% NG in Electricity" = "WDI_NG_El_.",
    "% VRE in Electricity" = "Share_VRE_Gen",
    "% Manufacturing in GDP" = "WDI_Manu_GDP_.",
    "GDP per capita (log)" = "logGDPpc", 
    "Natural gas production" = "BP_GasProduction_bM3", 
    "Natural gas reserve" = "BP_GasReserve_tM3", 
    "Coal production" = "BP_CoalProduction_mTon", 
    "Coal reserve" = "BP_CoalReserve_mTon", 
    "Natural gas RPR" = "BP_GasR2P_yr",
    "Log natural gas RPR" = "LogBP_GasR2P_yr",
    "Coal RPR" = "BP_CoalR2P_yr", 
    "LNG import trade diversity" = "HHI_LNG_Import", 
    "LNG import value" = "Value_LNG_Import",
    "LNG import share of neighbors" = "contiguity_LNG_Import", 
    "LNG import share of trade agreements" = "agree_LNG_Import", 
    "LNG import share of trade sanctions" = "sanction_LNG_Import",
    "LNG import share of common language" = "common_language_LNG_Import",
    "LNG import share from colonizers" = "colony_LNG_Import", 
    "LNG export trade diversity" = "HHI_LNG_Export",
    "LNG export value" = "Value_LNG_Export",
    "LNG export share of neighbors" = "contiguity_LNG_Export", 
    "LNG export share of trade agreements" = "agree_LNG_Export",
    "LNG export share of trade sanctions" = "sanction_LNG_Export", 
    "LNG export share of common language" = "common_language_LNG_Export", 
    "LNG export share from colonizers" = "colony_LNG_Export",
    "Coal import trade diversity" = "HHI_Coal_Import",
    "Coal net import quantity" = "NetQuantity_Coal_Import",
    "Coal net import value" = "NetValue_Coal_Import",
    "Gas import trade diversity" = "HHI_Gas_Import",
    "Gas import share of trade sanctions" = "sanction_Gas_Import",
    "Gas net import value" = "NetValue_Gas_Import",
    "Gas net import quantity" = "NetQuantity_Gas_Import",
    "Independent power producers" = "R_IndepProducer", 
    "Power market privitization" = "R_Private",
    "Power market unbundling" = "R_Unbundle", 
    "Wholesale power markets" = "R_WholeSale",
    "Independent power regulator" = "R_IndepReg", 
    "Power market consumer choice" = "R_Choice",
    "Power market liberalization" = "R_Liberalization", 
    "Power market corporatization" = "R_Corp",
    "Number of energy supply policies" = "num_supply_policy",
    "Air quality standards" = "binary_airqual",
    "Annex I" = "annex_one",
    "Annex II" = "annex_two",
    "Climate change worry" = "Survey_Worry_.",
    "Climate change belief" = "Survey_Belief_.",
    "Climate change affect on future generation" = "Survey_FutureGen_.",
    "Climate change personal effect" = "Survey_Personal_.",
    "Support for more fossil fuel" = "Survey_FossilMore_.",
    "Support for less fossil fuel" = "Survey_FossilLess_.")
  return(variable_mapping)
}