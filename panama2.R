#######Libraries and functions
rm(list = ls())
source("function/libraries.R") #### Libraries  
source("function/data_covid.R") #### Load data and functions to fix data
source("function/functions.R") ### Load functions to analysis

#### Load database
data_pan <- read_excel("data/POSITIVOSARSCOV-4.xlsx")

#### Fixing for date class
data_pan <- data_pan %>%
  filter(Type_of_case == "local")

data_pan2 <- as.Date(data_pan$Inicio_Sintomas, format = "%Y-%m-%d")


#### incidence object

fis <- incidence(data_pan2) ### incidencia por tipo de caso
fis

plot(fis, border = "Black", color = "SteelBlue") #### grafico incidence

fis_fix <- fit(fis[16:30])



plot(fis, fit = fis_fix, border = "Black", alpha = 0.5,  color = "SteelBlue" )

##### Estimacion del serial interval

mean_si = 7.2 
std_si = 3.4
cv <- mean_si/ std_si
param <- gamma_mucv2shapescale(mean_si,std_si)
w <- distcrete("gamma", interval = 1,
               shape = param$shape,
               scale = param$scale,
               w = 0)

discrete_si_distr <- discr_si(seq(0,20),mean_si,std_si)

plot(discrete_si_distr)

x2 <- fit(fis)


### Calculo del Rt, por una ventana de 20 dias
res <- estimate_R(incid = fis[10:30],
                  method = "non_parametric_si",
                  config = make_config(list(
                    si_distr = discrete_si_distr)))

#### Grafico de la epidemia
a <- plot(res, what = "R", color = "SteelBlue")
ppt <- grid.arrange(b,a, nrow = 1)

### Crear la base de datos de los resultados del Rt
results <- res$R
date <- res$dates[8:21]
results <- cbind(date,results)
write_csv(results,"data/fis_res.csv")
results

