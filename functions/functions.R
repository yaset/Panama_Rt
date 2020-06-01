
i.country <- function(study,mean_si = 4.7, std_si = 2.9){
  i.country <- study
  i.country$dates <- as.Date(i.country$dates, format = "%Y-%m-%d")
  i.country2 <- i.country %>%
    filter(I >= 1)
  ###Incidence
  incidence <- ggplot(data = i.country2, aes(x = dates, y = I)) +
    geom_bar(stat = "identity", width = 0.9, fill = "steelblue")+
    xlab("")+
    ylab("Daily Incidence")+
    scale_x_date(date_breaks = "2 day")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))
  ####discrete_si_distribution
  #mean_si <-  4.7 #7.5#4.7
  #std_si <-  2.9 #3.4#2.9
  cv <- mean_si/ std_si
  param <- gamma_mucv2shapescale(mean_si,std_si)
  w <- distcrete("gamma", interval = 1,
                 shape = param$shape,
                 scale = param$scale,
                 w = 0)
  
    discrete_si_distr <- discr_si(seq(0,20),mean_si,std_si)
  
  
  ########R_estimate
  
  i.country2 <- i.country %>%
    filter(I_a >= 25)
  
  
  ###semana
  t_start <- seq(7,length(i.country2$dates)-7)
  t_end <- t_start+7
  
  res <- estimate_R(incid = i.country2,
                    method = "non_parametric_si",
                    config = make_config(list(
                      si_distr = discrete_si_distr,
                      t_start = t_start,
                      t_end = t_end
                    )))
  
  
  
  #all_graphic <- plot(res, legend = FALSE)
  #all_r <-plot(res, what = "R")+
  #scale_x_date(date_breaks = "2 day")+
  #theme(axis.text.x = element_text(angle = 0, vjust = .5))
  
  
  list(incidence = incidence,
       res = res)
}



  
  plot_incidence <- function(study){
    grapi <- study
    combine <- grapi$res$R
    t_stat <- min(grapi$res$R$t_stat)
    t_end <- max(grapi$res$R$t_end)
    
    combine$dates <- grapi$res$dates[t_stat:t_end]
    
    incid_2 <- grapi$incidence
    scala <- max(incid_2$data$I)/5
    
    final <- incid_2 + 
      geom_line(data = combine,
                mapping = aes(x = dates, y = `Mean(R)`*scala), 
                inherit.aes = FALSE)+
      geom_ribbon(data = combine, 
                  mapping = aes(x = dates,ymin = `Quantile.0.025(R)`*scala, ymax = `Quantile.0.975(R)`*scala),
                  alpha = 0.3, fill = "Green",
                  inherit.aes = FALSE)+
      scale_y_continuous(name = "Daily Incidence",
                         sec.axis = sec_axis(~ ./scala, name = "Rt"))+
      theme_cowplot()+
      scale_fill_nejm()+
      theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 9))
    ##
    return(final)
  }

tabla_resume <- function(study){
  datas_bases <- study
  first_case <- min(datas_bases$incidence$data$dates)
  days <- as.Date("20-04-11") - first_case
  day_20_cases <- min(datas_bases$res$dates)
  days_r <- as.Date("20-04-11") - day_20_cases
  time_r <- day_20_cases - first_case
  acumulative <- max(datas_bases$incidence$data$I_a)
  model1 <- lm(log(I_a) ~ dates, data = datas_bases$incidence$data)
  dgr <- c(model1$coefficients[2], confint(model1)[2,])
  
  median_R <- datas_bases$res$R$`Median(R)`
  q025_R <- datas_bases$res$R$`Quantile.0.025(R)`
  q975_R <- datas_bases$res$R$`Quantile.0.975(R)`
  
  R <- cbind(median_R,q025_R,q975_R)
  return(list(first_case = first_case,
              days = days,
              day_20_cases = day_20_cases,
              days_r = days_r,
              time_r = time_r,
              acumulative = acumulative,
              dgr = dgr,
              R = R))
}




i.country_death <- function(study){
  i.country <- study
  i.country$dates <- as.Date(i.country$dates, format = "%Y-%m-%d")
  i.country2 <- i.country %>%
    filter(I >= 1)
  ###Incidence
  incidence <- ggplot(data = i.country2, aes(x = dates, y = I)) +
    geom_bar(stat = "identity", width = 0.9, fill = "Black")+
    xlab("")+
    ylab("Daily Incidence")+
    scale_x_date(date_breaks = "2 day")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))
  
  return(incidence)
}
