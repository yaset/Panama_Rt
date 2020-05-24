rm(list = ls())


#### Libraries

source("function/libraries.R") #### Libraries  
source("function/data_covid.R") #### Load data and functions to fix data
source("function/functions.R") ### Load functions to analysis

#### Load database
data <- read_excel("data/Data_Colombia_17May2020.xlsx")

data$FIS <- as.Date(data$FIS, format = "%Y-%m-%d")
data$Tipo

data_pan <- data %>%
  filter(Tipo == "Relacionado" | Tipo == "En estudio")

summary(data_pan$Tipo)
  

#### incidence object

fis <- incidence(data_pan$FIS) ### incidencia por tipo de caso
fis

plot(fis, border = "Black", color = "SteelBlue")+
  theme_classic()

###### daily growth rate
### general

fis2 <- fis[1:45]
ppt1 <- plot(fis2, border = "Black", color = "SteelBlue")+
  theme_classic()

###exponentail growth rate
general <- fit(fis2)
plot(general)
g2 <- fit(fis[1:15])
g3 <- fit(fis[15:30])

general

ge <- plot(fis2, border = "Black", color = "SteelBlue", fit = general)+
  labs(title = "General")

ge2 <- plot(fis2, border = "Black", color = "SteelBlue", fit = g3)+
  labs(title = "15-30 days")

grid.arrange(ge,ge2,nrow = 1)


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


pan30 <- fis$counts[1:30] #### LINEA CRITICA!!!
names(pan30) <- fis$dates[1:30]
si <- generation.time(type = "gamma", c(4.7,2.9))
EG <- est.R0.EG(pan30,si,begin = 1,end = 30)

EG2 <- sensitivity.analysis(pan30, GT = si, est.method = "EG", sa.type = "time",
                            begin = 1:15, end = 16:30)


plot(EG2)

EG$Rsquared
c(EG$R,EG$conf.int)
c(EG$r,EG$conf.int.r)
plotfit(EG)

#### New calculate based on the results of sensitivy analysis 
EG3 <- est.R0.EG(pan30,si,begin = 1,end = 22)
EG3$Rsquared
plotfit(EG3)
## Maximum likelihood
ML <- est.R0.ML(epid = pan30,begin = 1, end = 30, GT = si)
ML
ML$Rsquared
plot(ML)
plotfit(ML)



ml <- ML$pred
eg1 <- data.frame(EG$pred, EG$epid$t, EG$epid$incid)
names(eg1) <- c("pred", "t", "incid")
eg2 <- EG3$pred

eg1$t[5:30]


plotfit(EG)
plotfit(EG3)
plotfit(ML)


#### Early - likelihood-based estimation using branching process

cum <- cumulate(fis2)
cum$counts
x<- get_R(fis2[15:30], si_mean = 4.7, si_sd = 2.9) ### VENTANA DESDE EL DIA 15
x$R_like
x$R_ml


R_val  <- sample_R(x,1000)
summary(R_val)
quantile(R_val, c(0.025,.975))
hist(R_val, border = "Black", col = "navy",
     xlab = "Values of R",
     main = "Sample of likely R values")

x$incidence$n
print(x)

x
aa <- sim.epid(epid.nb = 100,GT = si, R0 = 1.55, epid.length = 100,family = "poisson", peak.value = 50)
apply(aa,2,mean)

### Calculo del Rt, por una ventana de 10 dias since 20 day had 25 cases of cumulative incidence

### Windows of 5 days
### CV 0.2 with incidence case 25 by Cori et al 2013

cum <- cumulate(fis2)
cum$counts
fis

t_start <- seq(2,nrow(fis[6:64])-7)
t_end <- t_start + 7
res <- estimate_R(incid = fis[6:64],
                  method = "non_parametric_si",
                  config = make_config(list(
                    si_distr = discrete_si_distr,
                    t_start = t_start,
                    t_end = t_end)))



res$R
ppt <- plot(res, what = "R")

grid.arrange(ppt1, ppt, nrow = 1)

combine <- res$R
t_stat <- min(res$R$t_end)
t_end2 <- max(res$R$t_end)
combine$dates <- res$dates[t_stat:t_end2]

write.xlsx(combine,"pictures/Colombia_Rt_Cori.xlsx")


###sequential bayesian
pan30

SB <- est.R0.SB(epid = pan30[12:30], GT = si)
plot(SB)
sb2 <- data.frame(SB$conf.int, SB$R)
sb2$dates <- row.names(sb2)


write.xlsx(sb2,"pictures/Colombia_Rt_SB.xlsx")

sb2 <- sb2[2:10,] ### LINEA CRITICA PARA LOS GRAFICOS





################### PLot incidence

class(EG$epid)
eg2 <- as.data.frame(EG$epid)


incid2 <- ggplot(data = eg2, aes(x = t, y = incid)) +
  geom_bar(stat = "identity", width = 0.9, fill = "steelblue")+
  xlab("")+
  ylab("Daily Incidence")+
  scale_x_date(date_breaks = "2 day")+
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 8))




scala <- max(incid2$data$incid)/5
incid2 + 
  geom_line(data = sb2, #SB
            mapping = aes(x = as.Date(dates), y = SB.R*scala), 
            inherit.aes = FALSE)+
  geom_ribbon(data = sb2, 
              mapping = aes(x = as.Date(dates),ymin = CI.lower.*scala, ymax = CI.upper.*scala),
              alpha = 0.3, fill = "Green",
              inherit.aes = FALSE)+
  scale_y_continuous(name = "Daily Incidence",
                     sec.axis = sec_axis(~ ./scala, name = "Rt"))+
  labs(title = "Rt by the Bettencourt's Method")

incid2 + 
  geom_line(data = combine,
            mapping = aes(x = dates, y = `Mean(R)`*scala), 
            inherit.aes = FALSE)+
  geom_ribbon(data = combine, 
              mapping = aes(x = dates,ymin = `Quantile.0.025(R)`*scala, ymax = `Quantile.0.975(R)`*scala),
              alpha = 0.3, fill = "Red",
              inherit.aes = FALSE)+
  scale_y_continuous(name = "Daily Incidence",
                     sec.axis = sec_axis(~ ./scala, name = "Rt"))+
  labs(title = "Rt by the Cori's Method")


  




