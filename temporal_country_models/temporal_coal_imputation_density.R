library(corrplot)
library(car)
library(MASS)
library(olsrr)
library(dplyr)
library(Amelia)

setwd("C:/Users/jx920/OneDrive - Imperial College London/1 - Projects/social_political_institutional")
current_dir <-"analysis/temporal_country_models"
imp_dir <-"analysis/temporal_country_models/imputation"

#========== import data
# listwise deletion data
listwise_data <- read.csv(file.path(current_dir,"coal_data_deletion.csv"))
rownames(listwise_data) <- listwise_data$X
listwise_data <- listwise_data[,-1]
# truncated data
data_path <- file.path(current_dir,"coal_data_truncate.csv")
truncated_data <- read.csv(data_path)
rownames(truncated_data) <- truncated_data$X
truncated_data <- truncated_data[,-1]
# amelia data
load(file.path(imp_dir,"amelia_attempt.RData")) 
#plot(amelia_attempt, which.vars = 10:14)


#========== visualization 
# Amalia's built in 
# for each cell that is missing in the variable, the diagnostic will find the 
# mean of that cell across each of the m datasets and use that value for 
# the density plot

# make the bandwidth of all density plots 0.1

# plot density
num_imputations <- 5
variable_names <- colnames(truncated_data)[10:length(colnames(truncated_data))]

# Split the variable names into two groups
variable_names_group1 <- variable_names[1:15]
variable_names_group2 <- variable_names[16:35] # Assuming you have more than 35 variables

# function to plot the density
plot_density <- function(variable_names_group,fname, h){
  png(file = file.path(imp_dir,fname), width = 200, height = h,
      units = "mm", res = 600, bg = "transparent")
  # Set up the layout for the subplots
  # Loop through each variable in Group 1
  num_rows <- ceiling(length(variable_names_group) / 5)
  num_cols <- min(length(variable_names_group), 5)
  par(mfrow = c(num_rows, num_cols))
  #par(mar = c(3, 4, 0.5, 2))
  amelia_i <- read.csv(file.path(imp_dir,paste0("coal_data_amelia_imp1.csv")))
  all_data <- data.frame(matrix(NA, nrow = nrow(amelia_i), ncol = 0))
  for (variable_name in variable_names_group) {
    # Plot the density for the current variable
    plot(density(truncated_data[[variable_name]], na.rm = TRUE, bw=0.1),main = "",col = "black",lwd=1)
    # Loop through each imputation and add the density lines
    for (i in 1:num_imputations) {
      amelia_i <- read.csv(file.path(imp_dir,paste0("coal_data_amelia_imp", as.character(i), ".csv")))
      new_col_name <- paste("col", i, sep = "_")
      new_col_data <- amelia_i[[variable_name]]
      all_data[[new_col_name]] <- new_col_data
      print(all_data)
      #lines(density(amelia_i[[variable_name]]), col = "blue", lty = 3, lwd = 2)
    }
    overall_mean <- rowMeans(all_data, na.rm = TRUE)
    density_vector <- density(overall_mean,bw=0.1)
    lines(density_vector, col = "blue", lty = 3, lwd = 1)
    # Adding the listwise data density and reference density
    lines(density(listwise_data[[variable_name]],bw=0.1), col = "red", lty = 3, lwd = 1)
    # Add a title to the plot
    title(main = variable_name)
    #if (which(variable_names_group == variable_name)==5){
    #  legend(x = "topright",   inset = c(- 0.8, 0),   xpd=TRUE,    # Position
    #       legend = c("Raw", "Imputed","Deletion"),  # Legend texts
    #       lty = c(1, 3, 3),           # Line types
    #       col = c("black","blue","red"),           # Line colors
    #       lwd = c(2, 1, 1))   
    #}
  }
  dev.off()
}


plot_density(variable_names_group1,"coal_data_amelia_density1_mean.png",150)
#plot_density(variable_names_group2,"coal_data_amelia_density2_mean.png",200)

# Reset the layout
par(mfrow = c(1, 1))
