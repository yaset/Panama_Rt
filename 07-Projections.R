############# EFECTIVE REPRODUCTIVE NUMBER
rm(list = ls())
#### load data
source("functions/libraries.R")
data <- read.csv("data/Panama_data.csv")
colors.plot <- pal_lancet(palette = "lanonc")(9)


#######################################################################
### Selecciona solo los locales y excluye los importados
data <- data %>%
  filter(Type_of_case == "local" )


data$fis <- as.Date(data$fis, format = "%Y-%m-%d")

#### Curva Epidemica
data <- data %>%
  filter(fis >= "2020-02-10" & fis <= "2020-04-20")
dates_fis <- as.Date(data$fis, format = "%Y-%m-%d")
fis <- incidence(dates_fis)

#####################################################################
########### se quitan los ultimos 15 dias de la curva
####################################################################

fis <- fis[1:45] ##### ESTA LINEA ES VARIABLE, QU

database <- data.frame(dates = fis$dates[1:45], num = fis$counts[1:45])

scala2 <- max(database$num)/5
incidencia <- ggplot(data = database, aes(x = dates, y = num))+
  geom_point(stat = "identity", color = colors.plot[1],  size = 2)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1)

incidencia
fis

#### tasa de crecimeinto
pan30 <- fis$counts[19:26] 
projects <- data.frame(dates = fis$dates[19:26], counts = pan30)
model1 <- lm(log(counts+0.5) ~ dates, data = projects)

model1$call[[2]]
quan <- confint(model1)[,2]


### school for 7 days
school.case.l <- exp((seq(27,33,1) * quan[1]) )
school.case.u <- exp((seq(27,33,1) * quan[2]) )
school.case <- exp(seq(27,33,1) * model1$coefficients[2])
school <- data.frame(dates = fis$dates[27:33], fits.l = school.case.l,
                     fits.u = school.case.u, fit = school.case)


school2 <- incidencia +
  geom_line(data = school, aes(x = dates, y = fit), linetype = 3)+
  geom_ribbon(data = school, aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.3, fill = colors.plot[7])+
  xlab("Date")+
  ylab("Daily Incidence")+
  labs(title = "A")+
  theme_cowplot()


ggsave("figures/close_schools.png",width = 10, height = 7)

### Toque de queda x 7 dias

### model
### 17-marzo
pan30 <- fis$counts[1:33] 
projects <- data.frame(dates = fis$dates[1:33], counts = pan30)
model1tda <- lm(log(counts+0.5) ~ dates, data = projects)

quan <- confint(model1tda)[,2]


tda.case.l <- exp((seq(33,40,1) * quan[1]) )
tda.case.u <- exp((seq(33,40,1) * quan[2]) )
tda.case <- exp(seq(33,40,1)*model1tda$coefficients[2])
tda <- data.frame(dates = fis$dates[33:40], fits.l = tda.case.l,
                     fits.u = tda.case.u, fit = tda.case)


case <- incidencia +
  geom_line(data = tda, aes(x = dates, y = fit), linetype = 3)+
    geom_ribbon(data = tda, aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.3, fill = colors.plot[7])+
  xlab("Date")+
  ylab("Daily Incidence")+
  labs(title = "B")+
  theme_cowplot()




## Toque de queda x 7 dias

### model
### Cuarentena
pan30 <- fis$counts[32:42] 
projects <- data.frame(dates = fis$dates[32:42], counts = pan30)
model1tda <- lm(log(counts+0.5) ~ dates, data = projects)

quan <- confint(model1tda)[,2]


tda.case.l <- exp((seq(42,45,1) * quan[1]) )
tda.case.u <- exp((seq(42,45,1) * quan[2]) )
tda.case <- exp(seq(42,45,1)*model1tda$coefficients[2])
tda <- data.frame(dates = fis$dates[42:45], fits.l = tda.case.l,
                  fits.u = tda.case.u, fit = tda.case)


cuaren <- incidencia +
  geom_line(data = tda, aes(x = dates, y = fit), linetype = 3)+
  geom_ribbon(data = tda, aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.3, fill = colors.plot[7])+
  xlab("Date")+
  ylab("Daily Incidence")+
  labs(title = "C")+
  theme_cowplot()

png("figures/projections.png", width = 750, height = 800)
grid.arrange(school2,case,cuaren, ncol = 1)
dev.off()


#### casosos

casos <- ts(fis$counts[1:26])
plot(casos)
casos.p <- HoltWinters(casos, beta = F, gamma = F)

library(forecast)
pro <- forecast(casos.p,15)
plot(forecast(casos.p,15))
pro$lower
school.l <- pro$lower[,1]
school.u <- pro$upper[,1]
fis$dates[27:41]

projec <- data.frame(dates = fis$dates[27:41], low = school.l, up = school.u)

incidencia+
  geom_ribbon(data = projec, aes (x = dates, ymin = low, ymax = up ), 
              inherit.aes = FALSE, alpha = 0.4, fill = "Red" )






#### casosos

casos <- ts(fis$counts[20:33])
plot(casos)
casos.p <- HoltWinters(casos, beta = F, gamma = F)

library(forecast)
pro <- forecast(casos.p,13)
plot(forecast(casos.p,13))
pro$lower
school.l <- pro$lower[,1]
school.u <- pro$upper[,1]
fis$dates[33:45]

projec <- data.frame(dates = fis$dates[33:45], low = school.l, up = school.u)

incidencia+
  geom_ribbon(data = projec, aes (x = dates, ymin = low, ymax = up ), 
              inherit.aes = FALSE, alpha = 0.4, fill = "Red" )
