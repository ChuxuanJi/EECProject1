rm(list = ls())
#loading package
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(car)

#Chao1 Linear model and Polynomial regression model single Environmental Variable Plot
Chaofunc <- read.csv("C:/Users/lenovo/Desktop/Chao_envfactors/Func_Chao_T_pH_lati.csv", stringsAsFactors = T)

#linear model only Temperature
lm_chao_all_T_plot <- ggplot(Chaofunc, aes(Temperature, Chao1)) +
  geom_point(size=1, shape=21) +
  geom_smooth(method = "lm", color = c("#CB4042"))+
  labs(x = 'Temperature (°C)', y = "Chao1 Index", title = 'Linear Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
lm_chao_all_T_plot

#linear model only pH
lm_chao_all_p_plot <- ggplot(Chaofunc, aes(pH, Chao1)) +
  geom_point(size=1, shape=21) +
  geom_smooth(method = "lm", color = c("#3A8FB7"))+
  labs(x = 'pH', y = "Chao1 Index", title = 'Linear Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
lm_chao_all_p_plot

#linear model only Latitude
lm_chao_all_L_plot <- ggplot(Chaofunc, aes(Latitude, Chao1)) +
  geom_point(size=1, shape=21) +
  geom_smooth(method = "lm", color = c("#FFB11B"))+
  labs(x = 'Latitude', y = "Chao1 Index", title = 'Linear Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
lm_chao_all_L_plot

#Polynomial Regression model only Temperature
pm_chao_all_T <- lm(Chao1~ poly(Temperature, 3), data = Chaofunc)

new_T <- seq(min(Chaofunc$Temperature), max(Chaofunc$Temperature), 0.01)
pred_pm_chaofunc_all_T <- data.frame(predict(pm_chao_all_T, newdata = data.frame(Temperature = new_T),
                                             interval = "confidence"), new_T = new_T)
head(pred_pm_chaofunc_all_T)
pm_chao_all_T_plot <- ggplot() +
  geom_point(data = Chaofunc, mapping = aes(x = Temperature, y = Chao1), size=1, shape=21) +
  geom_ribbon(data = pred_pm_chaofunc_all_T, mapping = aes(x = new_T, ymin = lwr, ymax = upr), 
              fill = "grey", alpha = 0.5)+
  geom_line(data = pred_pm_chaofunc_all_T, mapping = aes(x = new_T, y = fit), 
            color = "#CB4042", size = 1, alpha = 0.5) +
  labs(x = 'Temperature (°C)', y = "Chao1 Index", title = 'Polynomial Regression Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
pm_chao_all_T_plot

#Polynomial Regression model only pH
pm_chao_all_p <- lm(Chao1~ poly(pH, 3), data = Chaofunc)

new_p <- seq(min(Chaofunc$pH), max(Chaofunc$pH), 0.01)
pred_pm_chaofunc_all_p <- data.frame(predict(pm_chao_all_p, newdata = data.frame(pH = new_p),
                                             interval = "confidence"), new_p = new_p)

pm_chao_all_p_plot <- ggplot() +
  geom_point(data = Chaofunc, mapping = aes(x = pH, y = Chao1), size=1, shape=21) +
  geom_ribbon(data = pred_pm_chaofunc_all_p, mapping = aes(x = new_p, ymin = lwr, ymax = upr), 
              fill = "grey", alpha = 0.5)+
  geom_line(data = pred_pm_chaofunc_all_p, mapping = aes(x = new_p, y = fit), 
            color = "#3A8FB7", size = 1, alpha = 0.5) +
  labs(x = 'pH', y = "Chao1 Index", title = 'Polynomial Regression Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
pm_chao_all_p_plot

#Polynomial Regression model only Latitude
pm_chao_all_L <- lm(Chao1~ poly(Latitude, 3), data = Chaofunc)

new_L <- seq(min(Chaofunc$Latitude), max(Chaofunc$Latitude), 0.01)
pred_pm_chaofunc_all_L <- data.frame(predict(pm_chao_all_L, newdata = data.frame(Latitude = new_L),
                                             interval = "confidence"), new_L = new_L)

pm_chao_all_L_plot <- ggplot() +
  geom_point(data = Chaofunc, mapping = aes(x = Latitude, y = Chao1), size=1, shape=21) +
  geom_ribbon(data = pred_pm_chaofunc_all_L, mapping = aes(x = new_L, ymin = lwr, ymax = upr), 
              fill = "grey", alpha = 0.5)+
  geom_line(data = pred_pm_chaofunc_all_L, mapping = aes(x = new_L, y = fit), 
            color = "#FFB11B", size = 1, alpha = 0.5) +
  labs(x = 'Latitude', y = "Chao1 Index", title = 'Polynomial Regression Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
pm_chao_all_L_plot

ggarrange(lm_chao_all_T_plot, lm_chao_all_p_plot,lm_chao_all_L_plot,pm_chao_all_T_plot, pm_chao_all_p_plot,pm_chao_all_L_plot,ncol = 3, nrow = 2,
          labels = c("A","B","C","D","E","F"), # 添加标签
          font.label = list(size = 14))

#Observed OTUs Linear model and Polynomial regression model single Environmental Variable Plot
OOfunc <- read.csv("C:/Users/lenovo/Desktop/OO_envfactors/Func_OO_T_pH_lati.csv", stringsAsFactors = T)

#linear model only Temperature
lm_OO_all_T_plot <- ggplot(OOfunc, aes(Temperature, OO)) +
  geom_point(size=1, shape=21) +
  geom_smooth(method = "lm", color = c("#CB4042"))+
  labs(x = 'Temperature (°C)', y = "Observed OTUs", title = 'Linear Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
lm_OO_all_T_plot

#linear model only pH
lm_OO_all_p_plot <- ggplot(OOfunc, aes(pH, OO)) +
  geom_point(size=1, shape=21) +
  geom_smooth(method = "lm", color = c("#3A8FB7"))+
  labs(x = 'pH', y = "Observed OTUs", title = 'Linear Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
lm_OO_all_p_plot

#linear model only Latitude
lm_OO_all_L_plot <- ggplot(OOfunc, aes(Latitude, OO)) +
  geom_point(size=1, shape=21) +
  geom_smooth(method = "lm", color = c("#FFB11B"))+
  labs(x = 'Latitude', y = "Observed OTUs", title = 'Linear Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
lm_OO_all_L_plot

#Polynomial Regression model only Temperature
pm_OO_all_T <- lm(OO~ poly(Temperature, 3), data = OOfunc)

new_T <- seq(min(OOfunc$Temperature), max(OOfunc$Temperature), 0.01)
pred_pm_OOfunc_all_T <- data.frame(predict(pm_OO_all_T, newdata = data.frame(Temperature = new_T),
                                           interval = "confidence"), new_T = new_T)
head(pred_pm_OOfunc_all_T)
pm_OO_all_T_plot <- ggplot() +
  geom_point(data = OOfunc, mapping = aes(x = Temperature, y = OO), size=1, shape=21) +
  geom_ribbon(data = pred_pm_OOfunc_all_T, mapping = aes(x = new_T, ymin = lwr, ymax = upr), 
              fill = "grey", alpha = 0.5)+
  geom_line(data = pred_pm_OOfunc_all_T, mapping = aes(x = new_T, y = fit), 
            color = "#CB4042", size = 1, alpha = 0.5) +
  labs(x = 'Temperature (°C)', y = "Observed OTUs", title = 'Polynomial Regression Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
pm_OO_all_T_plot

#Polynomial Regression model only pH
pm_OO_all_p <- lm(OO~ poly(pH, 3), data = OOfunc)

new_p <- seq(min(OOfunc$pH), max(OOfunc$pH), 0.01)
pred_pm_OOfunc_all_p <- data.frame(predict(pm_OO_all_p, newdata = data.frame(pH = new_p),
                                           interval = "confidence"), new_p = new_p)

pm_OO_all_p_plot <- ggplot() +
  geom_point(data = OOfunc, mapping = aes(x = pH, y = OO), size=1, shape=21) +
  geom_ribbon(data = pred_pm_OOfunc_all_p, mapping = aes(x = new_p, ymin = lwr, ymax = upr), 
              fill = "grey", alpha = 0.5)+
  geom_line(data = pred_pm_OOfunc_all_p, mapping = aes(x = new_p, y = fit), 
            color = "#3A8FB7", size = 1, alpha = 0.5) +
  labs(x = 'pH', y = "Observed OTUs", title = 'Polynomial Regression Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
pm_OO_all_p_plot

#Polynomial Regression model only Latitude
pm_OO_all_L <- lm(OO~ poly(Latitude, 3), data = OOfunc)

new_L <- seq(min(OOfunc$Latitude), max(OOfunc$Latitude), 0.01)
pred_pm_OOfunc_all_L <- data.frame(predict(pm_OO_all_L, newdata = data.frame(Latitude = new_L),
                                           interval = "confidence"), new_L = new_L)

pm_OO_all_L_plot <- ggplot() +
  geom_point(data = OOfunc, mapping = aes(x = Latitude, y = OO), size=1, shape=21) +
  geom_ribbon(data = pred_pm_OOfunc_all_L, mapping = aes(x = new_L, ymin = lwr, ymax = upr), 
              fill = "grey", alpha = 0.5)+
  geom_line(data = pred_pm_OOfunc_all_L, mapping = aes(x = new_L, y = fit), 
            color = "#FFB11B", size = 1, alpha = 0.5) +
  labs(x = 'Latitude', y = "Observed OTUs", title = 'Polynomial Regression Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
pm_OO_all_L_plot


ggarrange(lm_OO_all_T_plot, lm_OO_all_p_plot,lm_OO_all_L_plot, pm_OO_all_T_plot, pm_OO_all_p_plot,pm_OO_all_L_plot,ncol = 3, nrow = 2,
          labels = c("A","B","C","D","E","F"), # 添加标签
          font.label = list(size = 14))


#Shannon Index Linear model and Polynomial regression model single Environmental Variable Plot
Shanfunc <- read.csv("C:/Users/lenovo/Desktop/Shan_envfactors/Func_Shan_T_pH_lati.csv", stringsAsFactors = T)

#linear model only Temperature
lm_Shan_all_T_plot <- ggplot(Shanfunc, aes(Temperature, Shannon)) +
  geom_point(size=1, shape=21) +
  geom_smooth(method = "lm", color = c("#CB4042"))+
  labs(x = 'Temperature (°C)', y = "Shannon Index", title = 'Linear Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
lm_Shan_all_T_plot

#linear model only pH
lm_Shan_all_p_plot <- ggplot(Shanfunc, aes(pH, Shannon)) +
  geom_point(size=1, shape=21) +
  geom_smooth(method = "lm", color = c("#3A8FB7"))+
  labs(x = 'pH', y = "Shannon Index", title = 'Linear Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
lm_Shan_all_p_plot

#linear model only Latitude
lm_Shan_all_L_plot <- ggplot(Shanfunc, aes(Latitude, Shannon)) +
  geom_point(size=1, shape=21) +
  geom_smooth(method = "lm", color = c("#FFB11B"))+
  labs(x = 'Latitude', y = "Shannon Index", title = 'Linear Model')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
lm_Shan_all_L_plot

#Polynomial Regression model only Temperature
pm_Shan_all_T <- lm(Shannon~ poly(Temperature, 3), data = Shanfunc)

new_T <- seq(min(Shanfunc$Temperature), max(Shanfunc$Temperature), 0.01)
pred_pm_Shanfunc_all_T <- data.frame(predict(pm_Shan_all_T, newdata = data.frame(Temperature = new_T),
                                             interval = "confidence"), new_T = new_T)
head(pred_pm_Shanfunc_all_T)
pm_Shan_all_T_plot <- ggplot() +
  geom_point(data = Shanfunc, mapping = aes(x = Temperature, y = Shannon), size=1, shape=21) +
  geom_ribbon(data = pred_pm_Shanfunc_all_T, mapping = aes(x = new_T, ymin = lwr, ymax = upr), 
              fill = "grey", alpha = 0.5)+
  geom_line(data = pred_pm_Shanfunc_all_T, mapping = aes(x = new_T, y = fit), 
            color = "#CB4042", size = 1, alpha = 0.5) +
  labs(x = 'Temperature (°C)', y = "Shannon Index", title = )+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
pm_Shan_all_T_plot

#Polynomial Regression model only pH
pm_Shan_all_p <- lm(Shannon~ poly(pH, 3), data = Shanfunc)

new_p <- seq(min(Shanfunc$pH), max(Shanfunc$pH), 0.01)
pred_pm_Shanfunc_all_p <- data.frame(predict(pm_Shan_all_p, newdata = data.frame(pH = new_p),
                                             interval = "confidence"), new_p = new_p)

pm_Shan_all_p_plot <- ggplot() +
  geom_point(data = Shanfunc, mapping = aes(x = pH, y = Shannon), size=1, shape=21) +
  geom_ribbon(data = pred_pm_Shanfunc_all_p, mapping = aes(x = new_p, ymin = lwr, ymax = upr), 
              fill = "grey", alpha = 0.5)+
  geom_line(data = pred_pm_Shanfunc_all_p, mapping = aes(x = new_p, y = fit), 
            color = "#3A8FB7", size = 1, alpha = 0.5) +
  labs(x = 'pH', y = "Shannon Index")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
pm_Shan_all_p_plot

#Polynomial Regression model only Latitude
pm_Shan_all_L <- lm(Shannon~ poly(Latitude, 3), data = Shanfunc)

new_L <- seq(min(Shanfunc$Latitude), max(Shanfunc$Latitude), 0.01)
pred_pm_Shanfunc_all_L <- data.frame(predict(pm_Shan_all_L, newdata = data.frame(Latitude = new_L),
                                             interval = "confidence"), new_L = new_L)

pm_Shan_all_L_plot <- ggplot() +
  geom_point(data = Shanfunc, mapping = aes(x = Latitude, y = Shannon), size=1, shape=21) +
  geom_ribbon(data = pred_pm_Shanfunc_all_L, mapping = aes(x = new_L, ymin = lwr, ymax = upr), 
              fill = "grey", alpha = 0.5)+
  geom_line(data = pred_pm_Shanfunc_all_L, mapping = aes(x = new_L, y = fit), 
            color = "#FFB11B", size = 1, alpha = 0.5) +
  labs(x = 'Latitude', y = "Shannon Index")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme_bw()
pm_Shan_all_L_plot


ggarrange(lm_Shan_all_T_plot, lm_Shan_all_p_plot,lm_Shan_all_L_plot,pm_Shan_all_T_plot, pm_Shan_all_p_plot,pm_Shan_all_L_plot,ncol = 3, nrow = 2,
          labels = c("A","B","C","D","E","F"), # 添加标签
          font.label = list(size = 14))

ggarrange(pm_Shan_all_T_plot, pm_Shan_all_p_plot,pm_Shan_all_L_plot,ncol = 3, nrow = 1,
          labels = c("A","B","C"), # 添加标签
          font.label = list(size = 14))
