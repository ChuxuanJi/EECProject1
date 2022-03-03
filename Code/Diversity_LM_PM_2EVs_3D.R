rm(list = ls())
#loading package
library(car)
library(rockchalk)

#Chao1 Linear model and Polynomial regression model 2 Environmental Variables 3D Plot
Chaofunc <- read.csv("C:/Users/lenovo/Desktop/Chao_envfactors/Func_Chao_T_pH_lati.csv", stringsAsFactors = T)

#lm and pm
lm_chao_all_Tp <- lm(Chao1 ~ Temperature*pH, data = Chaofunc)
lm_chao_all_TL <- lm(Chao1 ~ Temperature*Latitude, data = Chaofunc)
lm_chao_all_pL <- lm(Chao1 ~ pH*Latitude, data = Chaofunc)

pm_chao_all_Tp <- lm(Chao1~ poly(Temperature, 3) + poly(pH, 3), data = Chaofunc)
pm_chao_all_TL <- lm(Chao1~ poly(Temperature, 3) + poly(Latitude, 3) + Temperature:Latitude, data = Chaofunc)
pm_chao_all_pL <- lm(Chao1~ poly(pH, 3) + poly(Latitude, 3), data = Chaofunc)

#plot
old.par = par(mfrow=c(2,3), mar=c(1,1,1,1))

plotPlane(lm_chao_all_Tp, "Temperature", "pH", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='Black', lcol = '#2E5C6E',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'Temperature (C°)', x2lab = 'pH')

plotPlane(lm_chao_all_TL, "Temperature", "Latitude", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='black', lcol = '#7D6C46',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'Temperature (C°)', x2lab = 'Latitude')

plotPlane(lm_chao_all_pL, "pH", "Latitude", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='Black', lcol = '#261E47',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'pH', x2lab = 'Latitude')

plotPlane(pm_chao_all_Tp, "Temperature", "pH", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='Black', lcol = '#2E5C6E',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'Temperature (C°)', x2lab = 'pH')

plotPlane(pm_chao_all_TL, "Temperature", "Latitude", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='black', lcol = '#7D6C46',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'Temperature (C°)', x2lab = 'Latitude')

plotPlane(pm_chao_all_pL, "pH", "Latitude", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='Black', lcol = '#261E47',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'pH', x2lab = 'Latitude')

#Observed OTUs Linear model and Polynomial regression model 2 Environmental Variables 3D Plot
OOfunc <- read.csv("C:/Users/lenovo/Desktop/OO_envfactors/Func_OO_T_pH_lati.csv", stringsAsFactors = T)

#lm and pm
lm_OO_all_Tp <- lm(OO ~ Temperature*pH, data = OOfunc)
lm_OO_all_TL <- lm(OO ~ Temperature*Latitude, data = OOfunc)
lm_OO_all_pL <- lm(OO ~ pH*Latitude, data = OOfunc)

pm_OO_all_Tp <- lm(OO~ poly(Temperature, 3) + poly(pH, 3), data = OOfunc)
pm_OO_all_TL <- lm(OO~ poly(Temperature, 3) + poly(Latitude, 3) + Temperature:Latitude, data = OOfunc)
pm_OO_all_pL <- lm(OO~ poly(pH, 3) + poly(Latitude, 3), data = OOfunc)

#plot
old.par = par(mfrow=c(2,3), mar=c(1,1,1,1))

plotPlane(lm_OO_all_Tp, "Temperature", "pH", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='Black', lcol = '#2E5C6E',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'Temperature (C°)', x2lab = 'pH',ylab = 'Observed OTUs')

plotPlane(lm_OO_all_TL, "Temperature", "Latitude", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='black', lcol = '#7D6C46',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'Temperature (C°)', x2lab = 'Latitude',ylab = 'Observed OTUs')

plotPlane(lm_OO_all_pL, "pH", "Latitude", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='Black', lcol = '#261E47',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'pH', x2lab = 'Latitude',ylab = 'Observed OTUs')

plotPlane(pm_OO_all_Tp, "Temperature", "pH", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='Black', lcol = '#2E5C6E',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'Temperature (C°)', x2lab = 'pH',ylab = 'Observed OTUs')

plotPlane(pm_OO_all_TL, "Temperature", "Latitude", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='black', lcol = '#7D6C46',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'Temperature (C°)', x2lab = 'Latitude',ylab = 'Observed OTUs')

plotPlane(pm_OO_all_pL, "pH", "Latitude", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='Black', lcol = '#261E47',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'pH', x2lab = 'Latitude',ylab = 'Observed OTUs')


#Shannon Index Linear model and Polynomial regression model 2 Environmental Variables 3D Plot
Shanfunc <- read.csv("C:/Users/lenovo/Desktop/Shan_envfactors/Func_Shan_T_pH_lati.csv", stringsAsFactors = T)

#lm and pm
lm_Shan_all_Tp <- lm(Shannon ~ Temperature*pH, data = Shanfunc)
lm_Shan_all_TL <- lm(Shannon ~ Temperature*Latitude, data = Shanfunc)
lm_Shan_all_pL <- lm(Shannon ~ pH*Latitude, data = Shanfunc)

pm_Shan_all_Tp <- lm(Shannon~ poly(Temperature, 3) + poly(pH, 3)+Temperature:pH, data = Shanfunc)
pm_Shan_all_TL <- lm(Shannon~ poly(Temperature, 3) + poly(Latitude, 3) , data = Shanfunc)
pm_Shan_all_pL <- lm(Shannon~ poly(pH, 2) + poly(Latitude, 3), data = Shanfunc)

#plot
old.par = par(mfrow=c(2,3), mar=c(1,1,1,1))

plotPlane(lm_Shan_all_Tp, "Temperature", "pH", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='Black', lcol = '#2E5C6E',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'Temperature (C°)', x2lab = 'pH')

plotPlane(lm_Shan_all_TL, "Temperature", "Latitude", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='black', lcol = '#7D6C46',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'Temperature (C°)', x2lab = 'Latitude')

plotPlane(lm_Shan_all_pL, "pH", "Latitude", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='Black', lcol = '#261E47',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'pH', x2lab = 'Latitude')

plotPlane(pm_Shan_all_Tp, "Temperature", "pH", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='Black', lcol = '#2E5C6E',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'Temperature (C°)', x2lab = 'pH')

plotPlane(pm_Shan_all_TL, "Temperature", "Latitude", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='black', lcol = '#7D6C46',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'Temperature (C°)', x2lab = 'Latitude')

plotPlane(pm_Shan_all_pL, "pH", "Latitude", pch=20, col=rgb(0,0,0,0.1), drawArrows=TRUE, 
          alength=0, acol="red", pcol ='Black', lcol = '#261E47',alty=1,alwd=1, theta=30, phi=20, 
          x1lab = 'pH', x2lab = 'Latitude')
