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

fis <- fis[1:45] ##### ESTA LINEA ES VARIABLE

database <- data.frame(dates = fis$dates, num = fis$counts)


write.xlsx(database,"salidas/cases_final.xlsx")

scala2 <- max(database$num)/5
incidencia <- ggplot(data = database, aes(x = dates, y = num))+
  geom_point(stat = "identity", color = colors.plot[1],  size = 2)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1.2)

incidencia
fis
##################################################
#### Tasa de Crecimiento
pan30 <- fis$counts[1:23] 
projects <- data.frame(dates = fis$dates[1:23], counts = pan30)
model1 <- lm(log(counts+0.5) ~ dates, data = projects)

model1$call[[2]]
quan <- confint(model1)[,2]


#### School for 7 days


start.date <- fis$dates[1]
end1.date <- max(fis$dates)
end.date <- as.Date("2020-03-10") + 30

school.case.l <- exp((seq(1,53,1) * quan[1]) )
school.case.u <- exp((seq(1,53,1) * quan[2]) )
school.case <- exp(seq(1,53,1) * model1$coefficients[2])
school <- data.frame(dates = seq(start.date,end.date, by = "day"), fits.l = school.case.l,
                     fits.u = school.case.u, fit = school.case)
school_closure_21 <- school[23:44,]
write.csv(school_closure_21, "salidas/school_closure21_proj.csv")


w1 <- ggplot(data = school, aes (x = dates, y = fit))+
  geom_line(data = school[1:34,], aes(x = dates, y = fit),linetype = 3)+
  geom_point(data = database, aes(x = dates, y = num), stat = "identity", color = colors.plot[1],  size = 2)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1.2)+
  geom_ribbon(data = school[1:23,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.3, fill = colors.plot[3])+
  geom_ribbon(data = school[1:34,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.2, fill = colors.plot[3])+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,500,by = 100), limits = c(0,500))+
  xlab("Date")+
  ylab("Daily Incidence")+
  labs(title = "7 days")+
  theme_cowplot()




ggsave("figures/close_schools.png",width = 10, height = 7)



w2 <- ggplot(data = school, aes (x = dates, y = fit))+
  geom_line(data = school[1:41,], aes(x = dates, y = fit),linetype = 3)+
  geom_point(data = database, aes(x = dates, y = num), stat = "identity", color = colors.plot[1],  size = 2)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1.2)+
  geom_ribbon(data = school[1:27,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.3, fill = colors.plot[3])+
  geom_ribbon(data = school[1:41,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.2, fill = colors.plot[3])+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,500,by = 100), limits = c(0,500))+
  xlab("Date")+
  labs(title = "14 days")+
  ylab("Daily Incidence")+
  theme_cowplot()

w3 <- ggplot(data = school, aes (x = dates, y = fit))+
  geom_line(data = school[1:44,], aes(x = dates, y = fit),linetype = 3)+
  geom_point(data = database, aes(x = dates, y = num), stat = "identity", color = colors.plot[1],  size = 2)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1.2)+
  geom_ribbon(data = school[1:23,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.3, fill = colors.plot[3])+
  geom_ribbon(data = school[1:44,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.2, fill = colors.plot[3])+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,1500,by = 100), limits = c(0,1500))+
  labs(title = "A")+
  xlab("")+
  ylab("Daily Incidence")+
  theme_cowplot()


w4 <- ggplot(data = school, aes (x = dates, y = fit))+
  geom_line(data = school[1:55,], aes(x = dates, y = fit),linetype = 3)+
  geom_point(data = database, aes(x = dates, y = num), stat = "identity", color = colors.plot[1],  size = 2)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1.2)+
  geom_ribbon(data = school[1:27,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.3, fill = colors.plot[3])+
  geom_ribbon(data = school[1:55,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.2, fill = colors.plot[3])+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,1000,by = 100), limits = c(0,1000))+
  xlab("Date")+
  ylab("Daily Incidence")+
  labs(title = "28 days")+
  theme_cowplot()

png("figures/projec_school.png", width = 1000, height = 750)
grid.arrange(w1,w2,w3,w4, nrow = 2)
dev.off()

###############################################
### Toque de queda x 7 dias
### model -- 17-marzo
##############################################
##################################################
#### Tasa de Crecimiento
pan30 <- fis$counts[1:30] 
projects <- data.frame(dates = fis$dates[1:30], counts = pan30)
model1 <- lm(log(counts+0.5) ~ dates, data = projects)

model1$call[[2]]
quan <- confint(model1)[,2]


#### School for 7 days
start.date <- fis$dates[1]
end1.date <- max(fis$dates)
end.date <- as.Date("2020-03-17") + 30
end.date - start.date


school.case.l <- exp((seq(1,60,1) * quan[1]) )
school.case.u <- exp((seq(1,60,1) * quan[2]) )
school.case <- exp(seq(1,60,1) * model1$coefficients[2])
school <- data.frame(dates = seq(start.date,end.date, by = "day"), fits.l = school.case.l,
                                          fits.u = school.case.u, fit = school.case)
public_closure21 <- school[30:51,]

write.csv(public_closure21, "salidas/public_closure21_proj.csv")

w12 <- ggplot(data = school, aes (x = dates, y = fit))+
  geom_line(data = school[1:51,], aes(x = dates, y = fit),linetype = 3)+
  geom_point(data = database, aes(x = dates, y = num), stat = "identity", color = colors.plot[1],  size = 2)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1.2)+
  geom_ribbon(data = school[1:30,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.3, fill = colors.plot[3])+
  geom_ribbon(data = school[1:51,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.2, fill = colors.plot[3])+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,500,by = 100), limits = c(0,500))+
  xlab("Date")+
  ylab("Daily Incidence")+
  labs(title = "7 days")+
  theme_cowplot()




ggsave("figures/close_schools.png",width = 10, height = 7)



w22 <- ggplot(data = school, aes (x = dates, y = fit))+
  geom_line(data = school[1:48,], aes(x = dates, y = fit),linetype = 3)+
  geom_point(data = database, aes(x = dates, y = num), stat = "identity", color = colors.plot[1],  size = 2)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1.2)+
  geom_ribbon(data = school[1:34,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.3, fill = colors.plot[3])+
  geom_ribbon(data = school[1:48,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.2, fill = colors.plot[3])+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,1000,by = 100), limits = c(0,1000))+
  xlab("Date")+
  ylab("Daily Incidence")+
  labs(title = "14 days")+
  theme_cowplot()


w32 <- ggplot(data = school, aes (x = dates, y = fit))+
  geom_line(data = school[1:51,], aes(x = dates, y = fit),linetype = 3)+
  geom_point(data = database, aes(x = dates, y = num), stat = "identity", color = colors.plot[1],  size = 2)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1.2)+
  geom_ribbon(data = school[1:30,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.3, fill = colors.plot[3])+
  geom_ribbon(data = school[1:51,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.2, fill = colors.plot[3])+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,4000,by = 400), limits = c(0,4000))+
  ylab("Daily Incidence")+
  xlab("")+
  labs(title = "B")+
  theme_cowplot()


w42 <- ggplot(data = school, aes (x = dates, y = fit))+
  geom_line(data = school[1:63,], aes(x = dates, y = fit),linetype = 3)+
  geom_point(data = database, aes(x = dates, y = num), stat = "identity", color = colors.plot[1],  size = 2)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1.2)+
  geom_ribbon(data = school[1:34,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.3, fill = colors.plot[3])+
  geom_ribbon(data = school[1:63,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.2, fill = colors.plot[3])+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,1000,by = 100), limits = c(0,1000))+
  xlab("Date")+
  ylab("Daily Incidence")+
  labs(title = "28 days")+
  theme_cowplot()


png("figures/projec_public.png", width = 1000, height = 750)
grid.arrange(w12,w22,w32,w42, nrow = 2)
dev.off()


## Toque de queda x 7 dias

### model
### Cuarentena
#### Tasa de Crecimiento
pan30 <- fis$counts[1:38] 
projects <- data.frame(dates = fis$dates[1:38], counts = pan30)
model1 <- lm(log(counts+0.5) ~ dates, data = projects)

model1$call[[2]]
quan <- confint(model1)[,2]


#### School for 7 days
start.date <- fis$dates[1]
end1.date <- max(fis$dates)
end.date <- as.Date("2020-03-25") + 30
end.date - start.date


school.case.l <- exp((seq(1,68,1) * quan[1]) )
school.case.u <- exp((seq(1,68,1) * quan[2]) )
school.case <- exp(seq(1,68,1) * model1$coefficients[2])
school <- data.frame(dates = seq(start.date,end.date, by = "day"), fits.l = school.case.l,
                     fits.u = school.case.u, fit = school.case)
lockdown21 <- school[38:59,]

write.csv(lockdown21, "salidas/lockdown_21_proj.csv")
w13 <- ggplot(data = school, aes (x = dates, y = fit))+
  geom_line(data = school[1:49,], aes(x = dates, y = fit),linetype = 3)+
  geom_point(data = database, aes(x = dates, y = num), stat = "identity", color = colors.plot[1],  size = 2)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1.2)+
  geom_ribbon(data = school[1:42,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.3, fill = colors.plot[3])+
  geom_ribbon(data = school[1:49,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.2, fill = colors.plot[3])+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,1000,by = 100), limits = c(0,1000))+
  xlab("Date")+
  ylab("Daily Incidence")+
  labs(title = "7 days")+
  theme_cowplot()



w23 <- ggplot(data = school, aes (x = dates, y = fit))+
  geom_line(data = school[1:56,], aes(x = dates, y = fit),linetype = 3)+
  geom_point(data = database, aes(x = dates, y = num), stat = "identity", color = colors.plot[1],  size = 2)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1.2)+
  geom_ribbon(data = school[1:42,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.3, fill = colors.plot[3])+
  geom_ribbon(data = school[1:56,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.2, fill = colors.plot[3])+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,1000,by = 100), limits = c(0,1000))+
  xlab("Date")+
  ylab("Daily Incidence")+
  labs(title = "14 days")+
  theme_cowplot()


w33 <- ggplot(data = school, aes (x = dates, y = fit))+
  geom_line(data = school[1:63,], aes(x = dates, y = fit),linetype = 3)+
  geom_point(data = database, aes(x = dates, y = num), stat = "identity", color = colors.plot[1],  size = 2)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1.2)+
  geom_ribbon(data = school[1:42,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.3, fill = colors.plot[3])+
  geom_ribbon(data = school[1:63,], aes(x = dates, ymin = fits.l, ymax = fits.u), 
              inherit.aes = FALSE, alpha = 0.2, fill = colors.plot[3])+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,24000,by = 2000), limits = c(0,24000))+
  ylab("Daily Incidence")+
  labs(title = "C")+
  xlab("")+
  theme_cowplot()

w33

ggsave("figures/21_lockdown.png", width = 10, height = 8)

png("figures/projec_toque.png", width = 1000, height = 750)
grid.arrange(w13, w23, nrow = 1)
dev.off()





png("figures/projec_toque.png", width = 1200, height = 500)
grid.arrange(w3, w32, w33, nrow = 1)
dev.off()
