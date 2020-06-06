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



################################################################
############# DAILY GROWTH RATE ###############################
###############################################################

###################################
###Exponential growth rate
###################################


f.45 <- fit(fis)
f.30 <- fit(fis[30:45])
f.15 <- fit(fis[1:30])



plot.f45 <- plot(fis, border = "Black", color = colors.plot[1], fit = f.45)+
  labs(title = "Timeframe 1-45 days",
       subtitle = paste("r =",round(f.45$info$r,2),"cases/day"))+
  theme_cowplot()

plot.f30 <- plot(fis, border = "Black", color = colors.plot[2], fit = f.30)+
  labs(title = "Timeframe 30-45 days",
       subtitle = paste("r =",round(f.30$info$r,2),"cases/day"))+
  theme_cowplot()


plot.f15 <- plot(fis, border = "Black", color = colors.plot[3], fit = f.15)+
  labs(title = "Timeframe 1-30 days",
       subtitle = paste("r =",round(f.15$info$r,2),"cases/day"))+
  theme_cowplot()

bside <- grid.arrange(plot.f30, plot.f15,nrow = 2)

png("figures/daily_general.png", width = 1200, height = 600)
grid.arrange(plot.f45, bside, nrow = 1)
dev.off()


#############################################################
####### DAILY GROWTH RATE - WEEKS ##########################

### AJUSTADO POR SEMANAS
fis <- incidence(dates_fis)
fis <- fis[1:49] 

f.7 <- fit(fis[1:7])
f.14 <- fit(fis[8:14])
f.21 <- fit(fis[15:21])
f.28 <- fit(fis[22:28])
f.35 <- fit(fis[29:35])
f.42 <- fit(fis[36:42])
f.49 <- fit(fis[43:49])

dailys.rates <- c(f.7$info$r,
                  f.14$info$r,
                  f.21$info$r,
                  f.28$info$r,
                  f.35$info$r,
                  f.42$info$r,
                  f.49$info$r)

info <- data.frame(rates = dailys.rates)

info$d2 <- log(2)/log(1+info$rates)
fis
date.inicial <- min(fis$dates)
date.end <- max(fis$dates)
info$week <- seq(date.inicial+6,date.end,by = "week")

info <- info %>%
  filter(d2 >= 0)

write.csv(info,"salidas/rate_growth_week.csv")

scala <- max(info$d2)/10

database <- data.frame(dates = fis$dates[1:49], num = fis$counts[1:49])

m2 <- ggplot(data = database, aes(x = dates, y = num))+
  geom_bar(stat = "identity", width = 0.9, fill = colors.plot[1], alpha = 0.4)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1)
  
  
  
m2 +  geom_point(data = info, aes(week, y = d2*scala), size = 2, inherit.aes = FALSE)+
  geom_line(data = info, aes(week, y = d2*scala), linetype = 1,  inherit.aes = FALSE)+
  scale_y_continuous(name = "Daily Incidence",
                   sec.axis = sec_axis(~ ./scala, name = "Doubling Time in days by Week"))+
  theme_cowplot()
 
ggsave("figures/doubling_time.png",width = 10, height = 7)


###################################################
######## Doubling time por cada dia
###################################################


i.start <- seq(1,42,by = 1)
i.end <- i.start + 7
r2 <- list()
for (i in 1:length(i.end)){
  r2[[i]] <- fit(fis[i.start[i]:i.end[i]])$info$r
}
r.diary <- as.numeric(r2[1:42])

info.r <- data.frame(rates = r.diary)

info.r$d2 <- log(2)/log(1+info.r$rates)
date.inicial <- min(fis$dates)+7
date.end <- max(fis$dates)
info.r$week <- seq(date.inicial,date.end,by = 1)

info.r <- info.r %>%
  filter(d2 >= 0 & d2 != Inf)

write.csv(info.r,"salidas/rate_growth_diary.csv")


scala <- max(info.r$d2)/100

database <- data.frame(dates = fis$dates[1:49], num = fis$counts[1:49])

m2 <- ggplot(data = database, aes(x = dates, y = num))+
  geom_bar(stat = "identity", width = 0.9, fill = colors.plot[1], alpha = 0.4)+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype =2, color = colors.plot[4], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-16"), linetype =2,color = colors.plot[2], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-17"), linetype =2,color = colors.plot[3], size = 1)+
  geom_vline(xintercept = as.Date("2020-03-25"), linetype =2,color = colors.plot[7], size = 1)



m2 +  geom_point(data = info.r, aes(week, y = d2*scala), size = 2, inherit.aes = FALSE)+
  geom_line(data = info.r, aes(week, y = d2*scala), linetype = 1,  inherit.aes = FALSE)+
  scale_y_continuous(name = "Daily Incidence",
                     sec.axis = sec_axis(~ ./scala, name = "Doubling Time in days by Week"))+
  theme_cowplot()

ggsave("figures/doubling_time_diary.png",width = 10, height = 7)



 









