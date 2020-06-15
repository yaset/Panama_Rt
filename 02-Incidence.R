#### load data
source("functions/libraries.R")
data <- read.csv("data/Panama_data.csv")
colors.plot <- pal_lancet(palette = "lanonc")(9)

######## grafico general de casos
data.imported <- data
data.imported$fis <- as.Date(data.imported$fis, format = "%Y-%m-%d")

data.imported <- data.imported %>%
  filter(fis >= "2020-02-10" & fis <= "2020-04-13")




fis.general <- incidence(data.imported$fis, groups = data.imported$Type_of_case)

data.imported %>%
  tabyl(Type_of_case)

start.date <- as.Date("2020-01-30")
end.date <- max(fis.general$dates)



incidencia <- plot(fis.general, border = "Black", color = colors.plot[c(7,1)], alpha = 0.4)+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,150,by = 30), limits = c(0,150))+
  geom_vline(xintercept = as.Date("2020-02-13"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-03"), linetype =2, color = colors.plot[2], size = 1.2)+
  theme_classic()+
  labs(title = "A")+
  labs(groups = "Groups")

ggsave("figures/incidence_general.png", width = 7, height = 5)




### Selecciona solo los locales y excluye los importados
data_fis <- data %>%
  filter(Type_of_case == "local")


data_fis$fis <- as.Date(data_fis$fis, format = "%Y-%m-%d")

data_fis <- data_fis %>%
  filter(fis >= "2020-02-10" & fis <= "2020-04-13")
dates_fis <- as.Date(data_fis$fis, format = "%Y-%m-%d")
summary(dates_fis)

#### Curva Epidemica

fis <- incidence(dates_fis)
fis <- fis[1:60] ##### ESTA LINEA ES VARIABLE, QUEDA 60 POR 60 DIAS DE PANAMA

locales <- plot(fis, border = "Black", color = colors.plot[1])+
  theme_classic()+
  labs(title = "Epidemic Curve - Corrected by imported cases")

ggsave("figures/incidence_fix.png", width = 7, height = 5)


#### Curva Corrected

fis <- fis[1:45] ##### ESTA LINEA ES VARIABLE, QUEDA 60 POR 60 DIAS DE PANAMA

curva.corrected <- plot(fis, border = "Black", color = colors.plot[2])+
  theme_classic()+
  labs(title = "Epidemic Curve")

ggsave("figures/incidence_corrected.png", width = 7, height = 5)


#### Pacientes ambulatorios

data.2 <- data_fis %>%
  filter(type_patient == "ambulatorio")

data.2$fis <- as.Date(data.2$fis, format = "%Y-%m-%d")
fis.ambulatorio <- incidence(data.2$fis)
fis.ambulatorio <- fis.ambulatorio[1:60]

ambulatorio <- plot(fis.ambulatorio, border = "Black", color = colors.plot[3])+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,150,by = 30), limits = c(0,150))+
  geom_vline(xintercept = as.Date("2020-02-13"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-03"), linetype =2, color = colors.plot[2], size = 1.2)+
  theme_classic()+
  labs(title = "B")

ggsave("figures/incidence_ambulatorio.png", width = 7, height = 5)

#### Pacientes hospitalizados


data.3 <- data_fis %>%
  filter(type_patient == "hospitalizado")

data.3$fis <- as.Date(data.3$fis, format = "%Y-%m-%d")
fis.hospitalizado <- incidence(data.3$fis)
fis.hospitalizado <- fis.hospitalizado[1:55] ### linea critica para replicar

hospitalizado <- plot(fis.hospitalizado, border = "Black", color = colors.plot[4])+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,20,by = 5), limits = c(0,20))+
  geom_vline(xintercept = as.Date("2020-02-13"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-03"), linetype =2, color = colors.plot[2], size = 1.2)+
  theme_classic()+
  labs(title = "C")

ggsave("figures/incidence_hospitalizados.png", width = 7, height = 5)

##### Pacientes fallecidos

data.4 <- data_fis %>%
  filter(type_patient == "fallecido")

data.4$fis <- as.Date(data.4$fis, format = "%Y-%m-%d")
fis.muerto <- incidence(data.4$fis)
fis.muerto <- fis.muerto[1:37] ### linea critica para replicar

deaths <- plot(fis.muerto, border = "Black", color = colors.plot[5])+
  scale_x_date(breaks = seq(start.date,end.date, by = "2 week"), date_labels = "%m-%d", limits = c(start.date,end.date))+
  scale_y_continuous(breaks = seq(0,20,by = 5), limits = c(0,20))+
  geom_vline(xintercept = as.Date("2020-02-13"), linetype =2, color = colors.plot[4], size = 1.2)+
  geom_vline(xintercept = as.Date("2020-03-03"), linetype =2, color = colors.plot[2], size = 1.2)+
  theme_classic()+
  labs(title = "D")

ggsave("figures/incidence_deaths.png", width = 7, height = 5)


#### grafico conjunto

resume <- grid.arrange(ambulatorio,hospitalizado,deaths)

png("figures/incidence_resume.png", width = 900, height = 500)
grid.arrange(incidencia,resume, nrow = 1)
dev.off()


#### datos descriptivos basicos
fis.date.min <- min(fis$dates)
fis.date.max <- max(fis$dates)
n.cumulative <- sum(fis$counts)
fis.weeks <- round(nrow(fis$counts)/7,0)
fis.date.first.date <- min(fis.muerto$dates)
n.deaths <- sum(fis.muerto$counts)
n.hospitalizados <- sum(fis.hospitalizado$counts)


data.frame(fis.date.min, fis.date.max, n.cumulative,
           fis.weeks, fis.date.first.date, n.deaths,
           n.hospitalizados)


###### Quedaria pendiente calcular una curva epidemica para cada region
