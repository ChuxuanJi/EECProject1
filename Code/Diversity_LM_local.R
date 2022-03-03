rm(list = ls())
#loading package

library(car)

#loading files
local_1_all <- read.delim('C:/Users/lenovo/Desktop/Linear Model/local_925_all.txt',header = T)
local_2_all <- read.delim('C:/Users/lenovo/Desktop/Linear Model/local_945_all.txt',header = T)
local_3_all <- read.delim('C:/Users/lenovo/Desktop/Linear Model/local_1884_all.txt',header = T)


#Chao1 Index Linear model of local samples
##1.All local samples' linear model
#local 1 linear model T*P(***: )
#R2=0.4719, VIF( < 10), AIC=1733.53, MSE=7635.454
lm_chao_local_1_all_Tp <- lm(Chao1 ~ Temperature*pH, data = local_1_all)
summary(lm_chao_local_1_all_Tp)
vif(lm_chao_local_1_all_Tp, merge_coef=fause)
lm_chao_local_1_all_Tp.step <- step(lm_chao_local_1_all_Tp, direction = "backward")
mean(lm_chao_local_1_all_Tp$residuals^2)

#local 2 linear model T*P(***: )
#R2=0.001491, VIF(pH < 10), AIC=12575.88(AIC=12575.04,Chao1 ~ Temperature), MSE=177305.6
lm_chao_local_2_all_Tp <- lm(Chao1 ~ Temperature*pH, data = local_2_all)
summary(lm_chao_local_2_all_Tp)
vif(lm_chao_local_2_all_Tp, merge_coef=fause)
lm_chao_local_2_all_Tp.step <- step(lm_chao_local_2_all_Tp, direction = "backward")
mean(lm_chao_local_2_all_Tp$residuals^2)

#local 3 linear model T*P(***: )
#R2=0.05701, VIF(pH < 10), AIC=19957.57, MSE=1801304
lm_chao_local_3_all_Tp <- lm(Chao1 ~ Temperature*pH, data = local_3_all)
summary(lm_chao_local_3_all_Tp)
vif(lm_chao_local_3_all_Tp, merge_coef=fause)
lm_chao_local_3_all_Tp.step <- step(lm_chao_local_3_all_Tp, direction = "backward")
mean(lm_chao_local_3_all_Tp$residuals^2)

#local 1 linear model T(***: )
#R2=0.2438, VIF( < 10), AIC=1800.86, MSE=11049.61
lm_chao_local_1_all_T <- lm(Chao1 ~ Temperature, data = local_1_all)
summary(lm_chao_local_1_all_T)
lm_chao_local_1_all_T.step <- step(lm_chao_local_1_all_T, direction = "backward")
mean(lm_chao_local_1_all_T$residuals^2)

#local 2 linear model T(***: )
#R2=0.001509, VIF( < 10), AIC=12575.04, MSE=177644.7
lm_chao_local_2_all_T <- lm(Chao1 ~ Temperature, data = local_2_all)
summary(lm_chao_local_2_all_T)
lm_chao_local_2_all_T.step <- step(lm_chao_local_2_all_T, direction = "backward")
mean(lm_chao_local_2_all_T$residuals^2)

#local 3 linear model T(***: )
#R2=0.01907, VIF( < 10), AIC=20010.21, MSE=1876496
lm_chao_local_3_all_T <- lm(Chao1 ~ Temperature, data = local_3_all)
summary(lm_chao_local_3_all_T)
lm_chao_local_3_all_T.step <- step(lm_chao_local_3_all_T, direction = "backward")
mean(lm_chao_local_3_all_T$residuals^2)

#local 1 linear model p(***: )
#R2=0.2981, VIF( < 10), AIC=1786.47, MSE=10255.59
lm_chao_local_1_all_p <- lm(Chao1 ~ pH, data = local_1_all)
summary(lm_chao_local_1_all_p)
lm_chao_local_1_all_p.step <- step(lm_chao_local_1_all_p, direcpion = "backward")
mean(lm_chao_local_1_all_p$residuals^2)

#local 2 linear model p(***: )
#R2=0.0002836, VIF( < 10), AIC=, MSE=177862.7
lm_chao_local_2_all_p <- lm(Chao1 ~ pH, data = local_2_all)
summary(lm_chao_local_2_all_p)
lm_chao_local_2_all_p.step <- step(lm_chao_local_2_all_p, direcpion = "backward")
mean(lm_chao_local_2_all_p$residuals^2)

#local 3 linear model p(***: )
#R2=-0.0007, VIF( < 10), AIC=, MSE=1914306
lm_chao_local_3_all_p <- lm(Chao1 ~ pH, data = local_3_all)
summary(lm_chao_local_3_all_p)
lm_chao_local_3_all_p.step <- step(lm_chao_local_3_all_p, direcpion = "backward")
mean(lm_chao_local_3_all_p$residuals^2)

#Observed OTUS Linear model of local samples
##1.All local samples' linear model
#local 1 linear model T*P(***: )
#R2=0.412, VIF( < 10), AIC=1682.99, MSE=5876.543
lm_OO_local_1_all_Tp <- lm(OO ~ Temperature*pH, data = local_1_all)
summary(lm_OO_local_1_all_Tp)
vif(lm_OO_local_1_all_Tp, merge_coef=fause)
lm_OO_local_1_all_Tp.step <- step(lm_OO_local_1_all_Tp, direction = "backward")
mean(lm_OO_local_1_all_Tp$residuals^2)

#local 2 linear model T*P(***: )
#R2=0.009332, VIF(p < 10), AIC=11344.71(AIC=11344.04, OO ~ Temperature + pH), MSE=54213.27
lm_OO_local_2_all_Tp <- lm(OO ~ Temperature*pH, data = local_2_all)
summary(lm_OO_local_2_all_Tp)
vif(lm_OO_local_2_all_Tp, merge_coef=fause)
lm_OO_local_2_all_Tp.step <- step(lm_OO_local_2_all_Tp, direction = "backward")
mean(lm_OO_local_2_all_Tp$residuals^2)

#local 3 linear model T*P(***: )
#R2=0.0594, VIF(p < 10), AIC=17680.87, MSE=348082.4
lm_OO_local_3_all_Tp <- lm(OO ~ Temperature*pH, data = local_3_all)
summary(lm_OO_local_3_all_Tp)
vif(lm_OO_local_3_all_Tp, merge_coef=fause)
lm_OO_local_3_all_Tp.step <- step(lm_OO_local_3_all_Tp, direction = "backward")
mean(lm_OO_local_3_all_Tp$residuals^2)


#local 1 linear model T(***: )
#R2=0.2093, VIF( < 10), AIC=1738.2, MSE=7986.412
lm_OO_local_1_all_T <- lm(OO ~ Temperature, data = local_1_all)
summary(lm_OO_local_1_all_T)
lm_OO_local_1_all_T.step <- step(lm_OO_local_1_all_T, direction = "backward")
mean(lm_OO_local_1_all_T$residuals^2)

#local 2 linear model T(***: )
#R2=0.001895, VIF( < 10), AIC=11350.49, MSE=54725.73
lm_OO_local_2_all_T <- lm(OO ~ Temperature, data = local_2_all)
summary(lm_OO_local_2_all_T)
lm_OO_local_2_all_T.step <- step(lm_OO_local_2_all_T, direction = "backward")
mean(lm_OO_local_2_all_T$residuals^2)

#local 3 linear model T(***: )
#R2=0.0247, VIF( < 10), AIC=17729.04, MSE=361445.1
lm_OO_local_3_all_T <- lm(OO ~ Temperature, data = local_3_all)
summary(lm_OO_local_3_all_T)
lm_OO_local_3_all_T.step <- step(lm_OO_local_3_all_T, direction = "backward")
mean(lm_OO_local_3_all_T$residuals^2)


#local 1 linear model p(***: )
#R2=0.2529, VIF( < 10), AIC=1727.25, MSE=7545.958
lm_OO_local_1_all_p <- lm(OO ~ pH, data = local_1_all)
summary(lm_OO_local_1_all_p)
lm_OO_local_1_all_p.step <- step(lm_OO_local_1_all_p, direcpion = "backward")
mean(lm_OO_local_1_all_p$residuals^2)

#local 2 linear model p(***: )
#R2=0.007482, VIF( < 10), AIC=11344.65, MSE=54419.35
lm_OO_local_2_all_p <- lm(OO ~ pH, data = local_2_all)
summary(lm_OO_local_2_all_p)
lm_OO_local_2_all_p.step <- step(lm_OO_local_2_all_p, direcpion = "backward")
mean(lm_OO_local_2_all_p$residuals^2)

#local 3 linear model p(***: )
#R2=-0.0006107, VIF( < 10), AIC=, MSE=370825.7
lm_OO_local_3_all_p <- lm(OO ~ pH, data = local_3_all)
summary(lm_OO_local_3_all_p)
lm_OO_local_3_all_p.step <- step(lm_OO_local_3_all_p, direcpion = "backward")
mean(lm_OO_local_3_all_p$residuals^2)


#Shannon Index Linear model of local samples
##1.All local samples' linear model
#local 1 linear model T*P(***: )
#R2=0.2912, VIF( < 10), AIC=-57.82, MSE=0.7110322
lm_Shan_local_1_all_Tp <- lm(Shannon ~ Temperature*pH, data = local_1_all)
summary(lm_Shan_local_1_all_Tp)
vif(lm_Shan_local_1_all_Tp, merge_coef=fause)
lm_Shan_local_1_all_Tp.step <- step(lm_Shan_local_1_all_Tp, direction = "backward")
mean(lm_Shan_local_1_all_Tp$residuals^2)

#local 2 linear model T*P(***: )
#R2=0.04069, VIF( < 10), AIC=769.5, MSE=2.079669
lm_Shan_local_2_all_Tp <- lm(Shannon ~ Temperature*pH, data = local_2_all)
summary(lm_Shan_local_2_all_Tp)
vif(lm_Shan_local_2_all_Tp, merge_coef=fause)
lm_Shan_local_2_all_Tp.step <- step(lm_Shan_local_2_all_Tp, direction = "backward")
mean(lm_Shan_local_2_all_Tp$residuals^2)

#local 3 linear model T*P(***: )
#R2=0.04255, VIF( < 10), AIC=1069.52, MSE=2.152098
lm_Shan_local_3_all_Tp <- lm(Shannon ~ Temperature*pH, data = local_3_all)
summary(lm_Shan_local_3_all_Tp)
vif(lm_Shan_local_3_all_Tp, merge_coef=fause)
lm_Shan_local_3_all_Tp.step <- step(lm_Shan_local_3_all_Tp, direction = "backward")
mean(lm_Shan_local_3_all_Tp$residuals^2)


#local 1 linear model T(***: )
#R2=0.1433, VIF( < 10), AIC=-23.21, MSE=0.8685166
lm_Shan_local_1_all_T <- lm(Shannon ~ Temperature, data = local_1_all)
summary(lm_Shan_local_1_all_T)
lm_Shan_local_1_all_T.step <- step(lm_Shan_local_1_all_T, direction = "backward")
mean(lm_Shan_local_1_all_T$residuals^2)

#local 2 linear model T(***: )
#R2=0.009679, VIF( < 10), AIC=800.59, MSE=2.151039
lm_Shan_local_2_all_T <- lm(Shannon ~ Temperature, data = local_2_all)
summary(lm_Shan_local_2_all_T)
lm_Shan_local_2_all_T.step <- step(lm_Shan_local_2_all_T, direction = "backward")
mean(lm_Shan_local_2_all_T$residuals^2)

#local 3 linear model T(***: )
#R2=0.03076, VIF( < 10), AIC=1084.49, MSE=2.181765
lm_Shan_local_3_all_T <- lm(Shannon ~ Temperature, data = local_3_all)
summary(lm_Shan_local_3_all_T)
lm_Shan_local_3_all_T.step <- step(lm_Shan_local_3_all_T, direction = "backward")
mean(lm_Shan_local_3_all_T$residuals^2)


#local 1 linear model p(***: )
#R2=0.1678, VIF( < 10), AIC=-28.8, MSE=0.8436986
lm_Shan_local_1_all_p <- lm(Shannon ~ pH, data = local_1_all)
summary(lm_Shan_local_1_all_p)
lm_Shan_local_1_all_p.step <- step(lm_Shan_local_1_all_p, direcpion = "backward")
mean(lm_Shan_local_1_all_p$residuals^2)

#local 2 linear model p(***: )
#R2=0.03141, VIF( < 10), AIC=777.52, MSE=2.103845
lm_Shan_local_2_all_p <- lm(Shannon ~ pH, data = local_2_all)
summary(lm_Shan_local_2_all_p)
lm_Shan_local_2_all_p.step <- step(lm_Shan_local_2_all_p, direcpion = "backward")
mean(lm_Shan_local_2_all_p$residuals^2)

#local 3 linear model p(***: )
#R2=-0.0007119, VIF( < 10), AIC=, MSE=2.252602
lm_Shan_local_3_all_p <- lm(Shannon ~ pH, data = local_3_all)
summary(lm_Shan_local_3_all_p)
lm_Shan_local_3_all_p.step <- step(lm_Shan_local_3_all_p, direcpion = "backward")
mean(lm_Shan_local_3_all_p$residuals^2)

