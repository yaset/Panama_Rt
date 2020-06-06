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

### Calculo del Rt, por una ventana de 10 dias since 20 day had 25 cases of cumulative incidence

### Windows of 5 days
### CV 0.2 with incidence case 25 by Cori et al 2013

cum <- cumulate(fis)
cum$counts


t_start <- seq(2,nrow(fis[21:45])-7) #### numero de casos acumulados > 25
t_end <- t_start + 7
res <- estimate_R(incid = fis[21:45],
                  method = "non_parametric_si",
                  config = make_config(list(
                    si_distr = discrete_si_distr,
                    t_start = t_start,
                    t_end = t_end)))

combine <- res$R
t_stat <- min(res$R$t_end)
t_end2 <- max(res$R$t_end)
combine$dates <- res$dates[t_stat:t_end2]

write.csv(combine,"salidas/Panama_Rt_Cori2.csv")


##### GRAFICO

database <- data.frame(dates = fis$dates[1:45], num = fis$counts[1:45])

scala <- max(database$num)/5

incidencia <- ggplot(data = database, aes(x = dates, y = num))+
  geom_bar(stat = "identity", width = 0.9, fill = colors.plot[1], alpha = 0.4)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1)


incidencia + 
  geom_line(data = combine,
            mapping = aes(x = dates, y = `Mean(R)`*scala), 
            inherit.aes = FALSE)+
  geom_ribbon(data = combine, 
              mapping = aes(x = dates,ymin = `Quantile.0.025(R)`*scala, ymax = `Quantile.0.975(R)`*scala),
              alpha = 0.3, fill = "Green",
              inherit.aes = FALSE)+
  scale_y_continuous(name = "Daily Incidence",
                     sec.axis = sec_axis(~ ./scala, name = "Rt"))+
  geom_hline(yintercept = scala, color = "Red", linetype = 3)+
  labs(title = "Rt by the Cori's Method")+
  theme_cowplot()

############################################################
####### PROJECTIONS
################################################################


incidencia <- ggplot(data = database, aes(x = dates, y = num))+
  geom_point(stat = "identity", color = colors.plot[1],  size = 2)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1)

incidencia



set.seed(1)
pro <- project(fis[26:32],R = 2.21, si = w, n_days = 14, n_sim = 10000,
               R_fix_within = FALSE)


project <- data.frame(dates = rownames(pro),
                      median = apply(pro, 1, median),
                      p25 = apply(pro, 1, quantile,0.025),
                      p97 = apply(pro, 1, quantile,0.975))


incidencia +
  geom_ribbon(data = project, aes(x = as.Date(dates), ymin = p25, ymax = p97), 
              inherit.aes = FALSE, alpha = 0.5)

