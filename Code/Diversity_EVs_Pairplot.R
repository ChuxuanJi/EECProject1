rm(list = ls())
#loading package

library(car)

#Chao1 Pairs Plot
Chaofunc <- read.csv("C:/Users/lenovo/Desktop/Chao_envfactors/Func_Chao_T_pH_lati.csv", stringsAsFactors = T)
Chaofunc_soil <- Chaofunc[which(Chaofunc$Location == 'Soil (ns)'), ]
Chaofunc_ns_sedi <- Chaofunc[which(Chaofunc$Location == 'Sediment (ns)'), ]
Chaofunc_ns_surf <- Chaofunc[which(Chaofunc$Location == 'Surface (ns)'), ]
Chaofunc_ns_water <- Chaofunc[which(Chaofunc$Location == 'Water (ns)'), ]
Chaofunc_s_sedi <- Chaofunc[which(Chaofunc$Location == 'Sediment (s)'), ]
Chaofunc_s_surf <- Chaofunc[which(Chaofunc$Location == 'Surface (s)'), ]
Chaofunc_s_water <- Chaofunc[which(Chaofunc$Location == 'Water (s)'), ]
str(Chaofunc)

#raw lm
lm1 <- lm(Chao1~ ., data = Chaofunc)
summary(lm1)
#VIF value
vif(lm1)
#linear model without location
lm2 <- lm(Chao1~ Temperature*pH*Latitude, data = Chaofunc)
summary(lm2)
vif(lm2)
#linear model without latitude
lm3 <- lm(Chao1 ~ Temperature +pH, data = Chaofunc)
summary(lm3)
vif(lm3, merge_coef=fause)
cor(Chaofunc[,3:5])
lm2.step <- step(lm2, direction = "backward")
lm1.step <- step(lm1, direction = "backward")
lm3.step <- step(lm3, direction = "backward")
plot(Chaofunc[,3:5])



pairs(Chaofunc[,2:5])
pairs(Chaofunc_soil[,2:5])
pairs(Chaofunc_ns_sedi[,2:5])
pairs(Chaofunc_ns_surf[,2:5])
pairs(Chaofunc_ns_water[,2:5])
pairs(Chaofunc_s_sedi[,2:5])
pairs(Chaofunc_s_surf[,2:5])
pairs(Chaofunc_s_water[,2:5])

#Observed OTUs Pairs Plot
OOfunc <- read.csv("C:/Users/lenovo/Desktop/OO_envfactors/Func_OO_T_pH_lati.csv", stringsAsFactors = T)
OOfunc_soil <- OOfunc[which(OOfunc$Location == 'Soil (ns)'), ]
OOfunc_ns_sedi <- OOfunc[which(OOfunc$Location == 'Sediment (ns)'), ]
OOfunc_ns_surf <- OOfunc[which(OOfunc$Location == 'Surface (ns)'), ]
OOfunc_ns_water <- OOfunc[which(OOfunc$Location == 'Water (ns)'), ]
OOfunc_s_sedi <- OOfunc[which(OOfunc$Location == 'Sediment (s)'), ]
OOfunc_s_surf <- OOfunc[which(OOfunc$Location == 'Surface (s)'), ]
OOfunc_s_water <- OOfunc[which(OOfunc$Location == 'Water (s)'), ]
str(OOfunc)

#raw lm
lm1 <- lm(OO~ ., data = OOfunc)
summary(lm1)
#VIF value
vif(lm1)
#linear model without location
lm2 <- lm(OO~ Temperature*pH*Latitude, data = OOfunc)
summary(lm2)
vif(lm2)
#linear model without latitude
lm3 <- lm(OO ~ Temperature +pH, data = OOfunc)
summary(lm3)
vif(lm3, merge_coef=fause)
cor(OOfunc[,3:5])
lm2.step <- step(lm2, direction = "backward")
lm1.step <- step(lm1, direction = "backward")
lm3.step <- step(lm3, direction = "backward")
plot(OOfunc[,3:5])



pairs(OOfunc[,2:5])
pairs(OOfunc_soil[,2:5])
pairs(OOfunc_ns_sedi[,2:5])
pairs(OOfunc_ns_surf[,2:5])
pairs(OOfunc_ns_water[,2:5])
pairs(OOfunc_s_sedi[,2:5])
pairs(OOfunc_s_surf[,2:5])
pairs(OOfunc_s_water[,2:5])


#Shannon Index Pairs Plot
funcdata <- read.csv("C:/Users/lenovo/Desktop/Shan_envfactors/Func_Shan_T_pH_lati.csv", stringsAsFactors = T)
funcdata_soil <- funcdata[which(funcdata$Location == 'Soil (ns)'), ]
funcdata_ns_sedi <- funcdata[which(funcdata$Location == 'Sediment (ns)'), ]
funcdata_ns_surf <- funcdata[which(funcdata$Location == 'Surface (ns)'), ]
funcdata_ns_water <- funcdata[which(funcdata$Location == 'Water (ns)'), ]
funcdata_s_sedi <- funcdata[which(funcdata$Location == 'Sediment (s)'), ]
funcdata_s_surf <- funcdata[which(funcdata$Location == 'Surface (s)'), ]
funcdata_s_water <- funcdata[which(funcdata$Location == 'Water (s)'), ]
str(funcdata)

#raw lm
lm1 <- lm(Shannon~ ., data = funcdata)
summary(lm1)
#VIF value
vif(lm1)
#linear model without location
lm2 <- lm(Shannon~ Temperature + pH + Latitude, data = funcdata)
summary(lm2)
vif(lm2)
#linear model without latitude
lm3 <- lm(Shannon ~ Temperature +pH, data = funcdata)
summary(lm3)
vif(lm3, merge_coef=fause)
cor(funcdata[,3:5])
lm2.step <- step(lm2, direction = "backward")
lm1.step <- step(lm1, direction = "backward")
lm3.step <- step(lm3, direction = "backward")
plot(funcdata[,3:5])


layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2, byrow = TRUE))
pairs(funcdata[,2:5])
pairs(funcdata_soil[,2:5])
pairs(funcdata_ns_sedi[,2:5])
pairs(funcdata_ns_surf[,2:5])
pairs(funcdata_ns_water[,2:5])
pairs(funcdata_s_sedi[,2:5])
pairs(funcdata_s_surf[,2:5])
pairs(funcdata_s_water[,2:5])
