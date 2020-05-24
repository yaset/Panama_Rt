rm(list = ls())
source("function/libraries.R") #### Libraries  
source("function/data_covid.R") #### Load data and functions to fix data
source("function/functions.R") 


central_america <- c("Panama", "Mexico", "Costa Rica", "Colombia", "Ecuador", "Brazil")

central <- list()
for(i in 1:length(central_america)){
  central[[i]] <- countries(central_america[i])
}
names(central) <- central_america 

for(i in 1:length(central)){
  central[[i]] <- filter(central[[i]], I_a >= 1)
}

for(i in 1:length(central)){
  central[[i]][,1] <- as.Date(central[[i]][,1], format = "%Y-%m-%d")
}






ggplot(data = central$Panama, aes(x = dates, y = I_a))+
  geom_point()+
  geom_point(data = central$Mexico, aes(dates, y = I_a), inherit.aes = F, color = "Red")+
  geom_point(data = central$`Costa Rica`, aes(dates, y = I_a), inherit.aes = F, color = "Green")


####### se carga la base del pais
data <- central$Brazil


data2 <- data[1:45,] ### los primeros 45 dias
ggplot(data = data2, aes(x = dates, y = I_a))+ ### grafica de I_a en los primeros 30 dias
  geom_point()

AS <- lm(log(I_a+0.5) ~ dates, data = data2) ### Modelo Exponential Growth Rate by linear regression

summary(AS)


### Exponential growth rate
exp(AS$coefficients) ### coeficcientes
exp(confint(AS)) ### CI


##### Armar una base de datos
dates = seq(1,length(data$I_a),1) ### fechas de las bases de datos de los paises
### Lineal model
fits =   exp(AS$coefficients[2]*dates) ### fitted values and predictive values del modelo lineal

gr <- confint(AS)[2,] ### intervalos
fit_l = exp(gr[1]*dates) #### fit upper
fit_u = exp(gr[2]*dates) #### fit lower

###### Database to manage of data
data$fits <- fits
data$fit_u <- fit_u
data$fit_l <- fit_l



model1 <- data.frame(dates, fits, fit_l, fit_u, I_a = data$I_a)
### residual analysis
model1 <- model1 %>%
  mutate(resi =   log(I_a) - log(fits),
         RSS = (log(fits) - log(I_a))^2)


mean_y <- mean(model1$I_a) ### mean observerd

#### Estimations in a Logaritm correction


SS_res <- sum(model1$RSS) ### SS- residuals
SS_t <- sum((log(model1$I_a) - log(mean_y))^2) ### total
MSPE <- mean((log(model1$I_a) - log(model1$fits))^2)
r2 <- 1 - (SS_res / SS_t) ### Pseudo-R2 .61 of variance
cor(log(model1$I_a), log(model1$fits))^2


#### Visual inspection
ggplot(data = model1, aes(x = resi))+
  geom_histogram(stat = "density", fill = "SteelBlue")+
  geom_vline(xintercept = 0)

hist(model1$resi)
plot(model1$dates, model1$resi)
plot(log(model1$I_a), model1$resi)

summary(model1$resi)

r2


data2 <- data[1:45,] 
##### hasta 45 dias
ggplot(data = data2, aes( x = dates, y = fits))+
  geom_line(color = "Red")+
  geom_ribbon(aes(x = dates, ymin = fit_l, ymax = fit_u), fill = "Red", alpha = 0.3)+
  geom_ribbon(data = data[1:45,],aes(x = dates, ymin = fit_l, ymax = fit_u), fill = "Red", alpha = 0.3, inherit.aes = F)+
  geom_point(aes(x = dates, y = I_a), shape = 17, inherit.aes = F)+
  scale_x_date(limits = as.Date(c(data2$dates[1], max(data2$dates))), date_breaks = "2 weeks",
               date_labels = "%m-%d")+
  theme_cowplot()


#### proyecion hasta el 19 de Mayo
ggplot(data = data, aes( x = dates, y = log(fits)))+
  geom_line(color = "Red")+
  geom_ribbon(aes(x = dates, ymin = log(fit_l), ymax = log(fit_u)), fill = "Red", alpha = 0.3)+
  geom_ribbon(data = data[1:45,],aes(x = dates, ymin = log(fit_l), ymax = log(fit_u)), fill = "Blue", alpha = 0.3, inherit.aes = F)+
  geom_point(aes(x = dates, y = log(I_a)), shape = 17, inherit.aes = F)+
  geom_vline(xintercept = data$dates[45])+
  scale_x_date(limits = as.Date(c(data$dates[1], max(data$dates))), date_breaks = "2 weeks",
               date_labels = "%m-%d")+
  theme_cowplot()



