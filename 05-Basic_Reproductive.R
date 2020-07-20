
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

##### Estimacion del serial interval

mean_si = 4.7
std_si = 2.9
cv <- mean_si/ std_si
param <- gamma_mucv2shapescale(mean_si,std_si)

w <- distcrete("gamma", interval = 1,
               shape = param$shape,
               scale = param$scale,
               w = 0)

discrete_si_distr <- discr_si(seq(0,20),mean_si,std_si)
plot(seq(0,20),discrete_si_distr, type = "b", lwd = 2, lend = 1, xlab = "time (days)", ylab = "frequency")
title(main = "Discrete distribution of the serial interval of COVID-19")

####### Estimations of R0

### exponential growth rate

library(R0)

pan30 <- fis$counts[1:38] #### LINEA CRITICA!!!
names(pan30) <- fis$dates[1:38]
si <- R0::generation.time(type = "gamma", c(4.7,2.9))
#### General Growth Rate
EG <- R0::est.R0.EG(pan30,si)

EG

#### Sensitivity analysis
EG2 <- sensitivity.analysis(pan30, GT = si, est.method = "EG", sa.type = "time",
                            begin = 1:19, end = 20:38)

png("figures/sensitive.png", width = 1000, height = 750)
plot(EG2)
dev.off()

#### New calculate based on the results of sensitivy analysis 
EG3 <- est.R0.EG(pan30,si,begin = 2,end = 36)

EG3$conf.int.r
EG3$begin
EG3$end


png("figures/EG_fit.png", width = 750, height = 500)
plotfit(EG3)
dev.off()



EG3$Rsquared
c(EG3$R,EG3$conf.int)
c(EG3$r,EG3$conf.int.r)

## Maximum likelihood
ML <- est.R0.ML(epid = pan30[2:36], GT = si, range = c(0.01,100))
ML$begin
ML$end
ML$Rsquared
plot(ML)


ML2 <- est.R0.ML(epid = pan30, GT = si, range = c(0.01,100), begin = 15, end = 38)

ML2$begin
ML2$end
ML2$Rsquared


png("figures/ML_fit.png", width = 750, height = 500)
plotfit(ML2)
dev.off()






#### Early - likelihood-based estimation using branching process
pan30
cum <- cumulate(fis)
cum$counts
fis$dates
x<- get_R(fis[12:37], si = w, max_R = 3) ### VENTANA DESDE EL DIA 15
x$R_ml

R_val  <- sample_R(x,1000)
summary(R_val)
quantile(R_val, c(0.025,.975))

png("figures/early_r.png", width = 750, height = 500)
hist(R_val, border = "Black", col = "navy",
     xlab = "Values of R",
     main = "Sample of likely R values")
dev.off()



