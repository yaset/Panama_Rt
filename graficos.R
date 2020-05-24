rm(list = ls())
source("function/libraries.R") #### Libraries  
source("function/data_covid.R") #### Load data and functions to fix data
source("function/functions.R") 


central_america <- c("Panama", "Costa Rica", "Belize", "El Salvador", "Guatemala",
                     "Honduras", "Nicaragua")

central <- list()
for(i in 1:length(central_america)){
  central[[i]] <- countries(central_america[i])
}
names(central) <- central_america 


for(i in 1:length(central)){
  central[[i]][,1] <- as.Date(central[[i]][,1], format = "%Y-%m-%d")
}

aa <- central$Panama

tt <- data.frame(central[[1]][2])
tt <- cbind(tt, central[[2]][2])
tt <- cbind(tt, central[[3]][2])
tt <- cbind(tt, central[[4]][2])
tt <- cbind(tt, central[[5]][2])
tt <- cbind(tt, central[[6]][2])
tt <- cbind(tt, central[[7]][2])

names(tt)
head(tt)

tt <- stack(tt)
tt$dates <- rep(aa$dates,7)

max(tt$values)
centro1 <- ggplot(data = tt, aes( x = dates, y = log(values)))+
  geom_point(aes(color = ind))+
  geom_line(aes(color = ind))+
  labs(color = "Countries", labels = central_america)+
  ylab("Log(Cumulative Cases)")+
  theme_cowplot()+
  scale_y_continuous(breaks = seq(1000,10000,by = 1000))+
  scale_color_lancet(labels = central_america)


#### MUERTOS
  
central <- list()
  for(i in 1:length(central_america)){
    central[[i]] <- countries_death(central_america[i])
  }
  names(central) <- central_america 
  
  
  for(i in 1:length(central)){
    central[[i]][,1] <- as.Date(central[[i]][,1], format = "%Y-%m-%d")
  }
  
  aa <- central$Panama
  
  tt <- data.frame(central[[1]][2])
  tt <- cbind(tt, central[[2]][2])
  tt <- cbind(tt, central[[3]][2])
  tt <- cbind(tt, central[[4]][2])
  tt <- cbind(tt, central[[5]][2])
  tt <- cbind(tt, central[[6]][2])
  tt <- cbind(tt, central[[7]][2])
  
  names(tt)
  head(tt)
  
  tt <- stack(tt)
  tt$dates <- rep(aa$dates,7)
  
  centro2 <- ggplot(data = tt, aes( x = dates, y = log(values)))+
    geom_point(aes(color = ind))+
    geom_line(aes(color = ind))+
    labs(color = "Countries", labels = central_america)+
    ylab("Log(Cumulative Deaths)")+
    xlab("")+
    theme_cowplot()+
    scale_y_continuous(breaks = seq(50,400,by = 50))+
    scale_color_lancet(labels = central_america)

plot_grid(centro1,centro2)
ggsave("data/centro.jpg", width = 16, height = 8)
  
###################################3
############################################
##### AMERICA LATINA

latin_america <- c("Panama","Argentina", "Bolivia", "Brazil", "Chile",
                   "Colombia","Ecuador", "Paraguay",
                     "Peru","Uruguay","Venezuela")


latin <- list()
for(i in 1:length(latin_america)){
  latin[[i]] <- countries(latin_america[i])
}
names(latin) <- latin_america 


for(i in 1:length(latin)){
  latin[[i]][,1] <- as.Date(latin[[i]][,1], format = "%Y-%m-%d")
}

aa <- latin$Panama

tt <- data.frame(latin[[1]][2])
tt <- cbind(tt, latin[[2]][2])
tt <- cbind(tt, latin[[3]][2])
tt <- cbind(tt, latin[[4]][2])
tt <- cbind(tt, latin[[5]][2])
tt <- cbind(tt, latin[[6]][2])
tt <- cbind(tt, latin[[7]][2])
tt <- cbind(tt, latin[[8]][2])
tt <- cbind(tt, latin[[9]][2])
tt <- cbind(tt, latin[[10]][2])
tt <- cbind(tt, latin[[11]][2])


names(tt)
head(tt)

tt <- stack(tt)
tt$dates <- rep(aa$dates,11)

lancet <- pal_lancet("lanonc")(9)

miscolores <- colorRampPalette(lancet)



max(tt$values)
amer1 <- ggplot(data = tt, aes( x = dates, y = log(values)))+
  geom_point(aes(color = ind))+
  geom_line(aes(color = ind))+
  labs(color = "Countries", labels = latin_america)+
  ylab("log(Cumulative Cases)")+
  theme_cowplot()+
  scale_y_continuous(breaks = seq(0,15,by = 1))+
  scale_color_manual(labels = latin_america, values = miscolores(20))


amer1
#### MUERTOS

latin <- list()
for(i in 1:length(latin_america)){
  latin[[i]] <- countries_death(latin_america[i])
}
names(latin) <- latin_america 


for(i in 1:length(latin)){
  latin[[i]][,1] <- as.Date(latin[[i]][,1], format = "%Y-%m-%d")
}

aa <- latin$Panama

tt <- data.frame(latin[[1]][2])
tt <- cbind(tt, latin[[2]][2])
tt <- cbind(tt, latin[[3]][2])
tt <- cbind(tt, latin[[4]][2])
tt <- cbind(tt, latin[[5]][2])
tt <- cbind(tt, latin[[6]][2])
tt <- cbind(tt, latin[[7]][2])
tt <- cbind(tt, latin[[8]][2])
tt <- cbind(tt, latin[[9]][2])
tt <- cbind(tt, latin[[10]][2])
tt <- cbind(tt, latin[[11]][2])

tt <- stack(tt)
tt$dates <- rep(aa$dates,11)


amer2 <- ggplot(data = tt, aes( x = dates, y = log(values)))+
  geom_point(aes(color = ind))+
  geom_line(aes(color = ind))+
  labs(color = "Countries", labels = latin_america)+
  ylab("Log(Cumulative Deaths)")+
  xlab("")+
  theme_cowplot()+
  scale_y_continuous(breaks = seq(0,10,by = 1))+
  scale_color_manual(labels = latin_america, values = miscolores(20))

amer2
plot_grid(centro1,centro2,amer1,amer2)
ggsave("data/deaths.jpg", width = 15, height = 7.5)

#####################################

north_america <- c("Panama","Mexico", "Cuba", "Dominican Republic", "Haiti")

latin <- list()
for(i in 1:length(north_america)){
  latin[[i]] <- countries(north_america[i])
}
names(latin) <- north_america 


for(i in 1:length(latin)){
  latin[[i]][,1] <- as.Date(latin[[i]][,1], format = "%Y-%m-%d")
}

aa <- latin$Panama

tt <- data.frame(latin[[1]][2])
tt <- cbind(tt, latin[[2]][2])
tt <- cbind(tt, latin[[3]][2])
tt <- cbind(tt, latin[[4]][2])
tt <- cbind(tt, latin[[5]][2])

tt <- stack(tt)
tt$dates <- rep(aa$dates,5)

lancet <- pal_lancet("lanonc")(5)

miscolores <- colorRampPalette(lancet)



max(tt$values)
car1 <- ggplot(data = tt, aes( x = dates, y = log(values)))+
  geom_point(aes(color = ind))+
  geom_line(aes(color = ind))+
  labs(color = "Countries", labels = north_america)+
  ylab("log(Cumulative Cases)")+
  theme_cowplot()+
  scale_y_continuous(breaks = seq(0,15,by = 1))+
  scale_color_manual(labels = north_america, values = miscolores(5))


car1
#### MUERTOS

latin <- list()
for(i in 1:length(north_america)){
  latin[[i]] <- countries_death(north_america[i])
}
names(latin) <- north_america 


for(i in 1:length(latin)){
  latin[[i]][,1] <- as.Date(latin[[i]][,1], format = "%Y-%m-%d")
}

aa <- latin$Panama

tt <- data.frame(latin[[1]][2])
tt <- cbind(tt, latin[[2]][2])
tt <- cbind(tt, latin[[3]][2])
tt <- cbind(tt, latin[[4]][2])
tt <- cbind(tt, latin[[5]][2])

names(tt)
head(tt)

tt <- stack(tt)
tt$dates <- rep(aa$dates,5)


car2 <- ggplot(data = tt, aes( x = dates, y = log(values)))+
  geom_point(aes(color = ind))+
  geom_line(aes(color = ind))+
  labs(color = "Countries", labels = north_america)+
  ylab("Log(Cumulative Deaths)")+
  xlab("")+
  theme_cowplot()+
  scale_y_continuous(breaks = seq(0,10,by = 1))+
  scale_color_manual(labels = north_america, values = miscolores(5))

car2
plot_grid(centro1,centro2,amer1,amer2,car1,car2, nrow = 3,
          labels = c("Central America", "", "South America","","North America and Caribean",""))
ggsave("data/deaths.jpg", width = 20, height = 10)

