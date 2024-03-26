library(corrplot)
library(car)
library(tidyverse)
library(jtools)
library(memisc)
library(ggplot2)
library(effects)
library(stargazer)

##### Prepare dataset

# load plant-wise data
plantwise_data <- read.csv("analysis/coal2gas/coal2gas_plantwise.csv")
model_list <- list()

##### Test regression model 
data_m1 <- na.omit(cbind(data.frame(scale(subset(plantwise_data, select = c(GDPpc_2017.)))),
                           subset(plantwise_data, select = c(Transition))))
model_list[[1]] <- glm(Transition ~ ., data = data_m1)

data_m2c <- na.omit(subset(plantwise_data, select = c(Transition, R_Choice)))
model_list[[2]] <- glm(Transition ~ ., data = data_m2c)

data_m3 <- na.omit(cbind(data.frame(scale(subset(plantwise_data, select = c(LNG_Import_km,LNG_Export_km)))),#c(Coal_Price_Var,NG_Price_Var,LNG_km)))),
                         subset(plantwise_data, select = c(Transition))))
model_list[[3]] <- glm(Transition ~ ., data = data_m3)

data_m4 <- na.omit(cbind(data.frame(scale(subset(plantwise_data, select = c(Coal_Price_Var,NG_Price_Var)))),
                         subset(plantwise_data, select = c(Transition))))
model_list[[4]] <- glm(Transition ~ ., data = data_m4)

data_m5 <- na.omit(cbind(data.frame(scale(subset(plantwise_data, select = c(WDI_CoalRents_.,WDI_NGRents_.)))),
                         subset(plantwise_data, select = c(Transition))))
model_list[[5]] <- glm(Transition ~ ., data = data_m5)

data_m6 <- na.omit(cbind(data.frame(scale(subset(plantwise_data, select = c(Coal_MW)))),
                         subset(plantwise_data, select = c(Transition))))
model_list[[6]] <- glm(Transition ~ ., data = data_m6)

data_bigmodel <- na.omit(cbind(data.frame(scale(subset(plantwise_data, select = c(GDPpc_2017.,LNG_Export_km,NG_Price_Var,
                                                                            WDI_CoalRents_.,Coal_MW)))),
                         subset(plantwise_data, select = c(Transition,R_Choice))))
model_list[[7]] <- glm(Transition ~ ., data = data_bigmodel)

stargazer(model_list,type="html",
          dep.var.labels= "Coal to gas transition", 
          covariate.labels= c("GDP per capita",
                              "Choice of supplier",
                              "Distance from LNG import terminal",
                              "Distance from LNG export terminal",
                              "Coal price",
                              "Gas price",
                              "Coal rent",
                              "Gas rent",
                              "Coal plant capacity"), 
          keep.stat=c("aic","rsq", "adj.rsq","n"),
          out=file.path("analysis/coal2gas/coal2gas_plantwise_model.html"))




par(mfrow = c(1, 2))
effect_plot(model_m6, pred = Coal_MW, interval = TRUE, plot.points = TRUE,
            y.label = "% Transion",
            x.label = "SD of coal capacity (MW)")
effect_plot(model_m7, pred = BP_CoalR2P_yr, interval = TRUE, plot.points = TRUE,
            y.label = "% Transion",
            x.label = "SD of coal reserve to production")
effect_plot(model_m7, pred = BP_GasR2P_yr, interval = TRUE, plot.points = TRUE,
            y.label = "% Transion",
            x.label = "SD of gas reserve to production")
effect_plot(model_m1, pred = GDPpc_2017., interval = TRUE, plot.points = TRUE,
            y.label = "% Transion",
            x.label = "SD of GDP per capita")
effect_plot(model_m2c, pred = R_Choice, interval = TRUE, plot.points = TRUE,
            y.label = "% Transion",
            x.label = "Choice of supplier")
effect_plot(model_m4, pred = NG_Price_Var, interval = TRUE, plot.points = TRUE,
            y.label = "% Transion",
            x.label = "SD of natural gas price")
effect_plot(model_m5, pred = WDI_CoalRents_., interval = TRUE, plot.points = TRUE, 
            y.label = "% Transion",
            x.label = "SD of coal rent")


eff <- effect("Coal_MW", model_m6)
eff2 <- effect("BP_CoalR2P_yr", model_m7)
plot(eff, style="stacked",rug=F, key.args=list(space="right"),
     row = 1,col = 1,nrow = 1,ncol = 2,more = TRUE)
plot(eff2, style="stacked",rug=F, key.args=list(space="right"),
     row = 1,col = 2,nrow = 1,ncol = 2)

plot(allEffects(model_m7))