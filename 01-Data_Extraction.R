rm(list = ls())


#### Libraries
source("functions/libraries.R") #### Libraries  
library(dplyr)
library(readxl)
source("function/data_covid.R") #### Load data and functions to fix data
source("function/functions.R") ### Load functions to analysis

#### Load database
database_original <- read_excel("data/data_base_panama.xls")

database_clean <- database_original %>%
  select(Tipo_Paciente,Sex,Age,Region, Corregimiento, `Fecha Inicio_Sintomas`,
         Fecha_de_recibo, Fecha_de_reporte, tiempodelay, Type_of_case, exposition,
         type_of_exposition)



database_clean %>%
  tabyl(type_patient)

### Arreglo de datos

database_clean <- database_clean %>%
  rename(type_patient = Tipo_Paciente,
         sex = Sex,
         age = Age)
#Tipo Paciente
database_clean$type_patient[database_clean$type_patient == "Ambulatorio"] <- "ambulatorio"
database_clean$type_patient[database_clean$type_patient == "AMBULATORIO"] <- "ambulatorio"
database_clean$type_patient[database_clean$type_patient == "DESCONOCIDO"] <- "desconocido"
database_clean$type_patient[database_clean$type_patient == "AMBULATORIO/HOSPITALIZADO"] <- "ambulatorio"
database_clean$type_patient[database_clean$type_patient == "Fallecido"] <- "fallecido"
database_clean$type_patient[database_clean$type_patient == "FALLECIDO"] <- "fallecido"
database_clean$type_patient[database_clean$type_patient == "FALLECIDO"] <- "fallecido"
database_clean$type_patient[database_clean$type_patient == "Hospitalizado"] <- "hospitalizado"
database_clean$type_patient[database_clean$type_patient == "HOSPITALIZADO"] <- "hospitalizado"
database_clean$type_patient[is.na(database_clean$type_patient)] <- "desconocido"


### Sex
database_clean %>%
  tabyl(sex)

database_clean$sex[database_clean$sex == "Female"] <- "female"
database_clean$sex[database_clean$sex == "FEMALE"] <- "female"
database_clean$sex[database_clean$sex == "FEMENINO"] <- "female"
database_clean$sex[database_clean$sex == "Male"] <- "male"
database_clean$sex[database_clean$sex == "MALE"] <- "male"
database_clean$sex[database_clean$sex == "MASCULINO"] <- "male"

## Age

database_clean$age <- as.numeric(database_clean$age)
database_clean$age[is.na(database_clean$age)] <- rnorm(1, 
                                                       mean = mean(database_clean$age,na.rm = TRUE,
                                                       sd = sd(database_clean$age, na.rm = TRUE)))

## fis
database_clean <- database_clean %>%
  rename(fis = `Fecha Inicio_Sintomas`)
         
database_clean$fis <- as.Date(database_clean$fis, format = "%Y-%m-%d")
database_clean$Fecha_de_reporte <- as.Date(database_clean$Fecha_de_reporte, format = "%Y-%m-%d")

diff_time <- database_clean$Fecha_de_reporte - database_clean$fis
diff_time <- as.numeric(diff_time)

diff_time <- diff_time[diff_time >= 0 & diff_time <= 30]
diff_time <- abs(diff_time)
#### intervalo de fecha de inicio de sintomas y reporte
mean_time <- mean(diff_time, na.rm = TRUE)
sd_time <- sd(diff_time, na.rm = TRUE)
discrete_time.report <- discr_si(seq(1:20), mean_time, sd_time)
plot(discrete_time.report)


##### Imputacion de fecha de inicio de sintomas
trial2 <- database_clean %>%
  filter(is.na(fis) & !is.na(Fecha_de_reporte)) %>%
  mutate(fix_fis = Fecha_de_reporte - 5)

database_clean$fis[is.na(database_clean$fis) & !is.na(database_clean$Fecha_de_reporte)] <- trial2$fix_fis

summary(database_clean$fis)


#### Base de datos para trabajar
write.csv(database_clean, "data/Panama_data.csv")








