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

fis <- fis[1:45] ##### ESTA LINEA ES VARIABLE, QUEDA 60 POR 60 DIAS DE PANAMA

plot(fis, border = "Black", color = colors.plot[5])+
  theme_classic()+
  labs(title = "Epidemic Curve")

#### daily growth rate


###exponentail growth rate
general <- fit(fis)
g2 <- fit(fis[1:15])
g3 <- fit(fis[15:45])

general

ge <- plot(fis, border = "Black", color = "SteelBlue", fit = general)+
  labs(title = "General")

ge2 <- plot(fis, border = "Black", color = "SteelBlue", fit = g3)+
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

library(R0)
pan30 <- fis$counts[1:45] #### LINEA CRITICA!!!
names(pan30) <- fis$dates[1:45]
si <- R0::generation.time(type = "gamma", c(4.7,2.9))
EG <- R0::est.R0.EG(pan30,si,begin = 1,end = 45)

EG2 <- sensitivity.analysis(pan30, GT = si, est.method = "EG", sa.type = "time",
                            begin = 1:22, end = 23:44)


plot(EG2)

EG$Rsquared
c(EG$R,EG$conf.int)
c(EG$r,EG$conf.int.r)
plotfit(EG)

#### New calculate based on the results of sensitivy analysis 
EG3 <- est.R0.EG(pan30,si,begin = 2,end = 38)
plotfit(EG3)

EG3$Rsquared

## Maximum likelihood
ML <- est.R0.ML(epid = pan30,begin = 1, end = 45, GT = si)
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
x<- get_R(fis[1:45], si_mean = 4.7, si_sd = 2.9) ### VENTANA DESDE EL DIA 15
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


t_start <- seq(2,nrow(fis[20:45])-5)
t_end <- t_start + 5
res <- estimate_R(incid = fis[20:45],
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
library(xlsx)
write.csv(combine,"salidas/Panama_Rt_Cori2.csv")


###sequential bayesian
pan30

SB <- est.R0.SB(epid = pan30[20:30], GT = si)
plot(SB)
sb2 <- data.frame(SB$conf.int, SB$R)
sb2$dates <- row.names(sb2)


write.xlsx(sb2,"pictures/Panama_Rt_SB.xlsx")

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






