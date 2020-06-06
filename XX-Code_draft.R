

###sequential bayesian
pan30 <- fis$counts[1:45]
names(pan30) <- fis$dates[1:45]
si <- R0::generation.time(type = "gamma", c(4.7,2.9))

SB <- est.R0.SB(epid = pan30[20:45], GT = si)
sb2 <- data.frame(SB$conf.int, SB$R)
sb2$dates <- row.names(sb2)
plotfit(SB)

write.xlsx(sb2,"pictures/Panama_Rt_SB.xlsx")

sb2 <- sb2[2:10,] ### LINEA CRITICA PARA LOS GRAFICOS





################### PLot incidence

class(EG$epid)
eg2 <- as.data.frame(SB$epid)


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


