maltus_simulation <- function(x0,t0,r,alpha,t,ro){
  ## Maltus simulation  
  ## ------------------
  ## x0     : Initial value
  ## t0     : Initial time
  ## r     : Constant
  ## alpha  : Constant
  ## t      : Final time
  ## 
  ## --------------------
  ## 
  ## mu = r- 1/2*alpha^2
  ## if r > alpha^2/ -> Xt -> inf
  ## if r < alpha^2/ -> Xt -> 0
  ## if r = alpha^2/ -> E(Xt) -> X_0
  ##
  
    mu <- r-alpha^2/2
    ts <- seq(t0,t,ro)
    
    xs <- rep(0,t)
    es <- rep(0,t)
    
    for (i in 1:length(ts)){
      xs[i] <- x0*exp(mu*ts[i] + alpha*rnorm(1,0,ts[i]))
      es[i] <- x0*exp(mu*ts[i])
    }
    
    maltus <- as.data.frame(list(ts = ts, xs = xs, es = es))
    
    maltus <- bind_rows( maltus %>% select(ts,xs) %>%  mutate(xs = xs, type = "Xt", variables = paste0("X0 = ",x0,", r = ",r,", alpha = ",alpha)),
                         maltus %>% select(ts,xs=es) %>%  mutate(xs = xs, type = "Et", variables = paste0("X0 = ",x0,", r = ",r,", alpha = ",alpha)))
    maltus
}






maltus1_t20 <- maltus_simulation(x0 = 5000000000,t0 = 0,r = 1,alpha = 0.5,t = 20,ro = 0.2)
maltus2_t20 <- maltus_simulation(x0 = 5000000000,t0 = 0,r = -1,alpha = 0.5,t = 20,ro = 0.2)
maltus3_t20 <- maltus_simulation(x0 = 5000000000,t0 = 0,r = 0.125,alpha = 0.5,t = 20,ro = 0.2)
   
maltus_t20 <- bind_rows(maltus1_t20,
                        maltus2_t20,
                        maltus3_t20)             

p <- 
  maltus_t20 %>% 
      ggplot(aes(ts,log(xs+1), color = type))+
      facet_wrap(~variables)+
      geom_line() + 
      scale_color_manual(values=c("#EE2C2C", "#000000"))+
      labs(c("Xt","Et"))+
      ylab("Xt")+
      xlab("t")+
      ggtitle("Simulación del modelo de Maltus")

savePlot(p,"Modelo_Maltus_t50.jpeg")




multiple_simulation_maltus <- function(x0,t0,r,alpha,t,ro,n_iter){
  
  sol <- maltus_simulation(x0 = x0,
                           t0 = t0,
                           r = r,
                           alpha = alpha,
                           t = t,
                           ro = ro) %>%  
    mutate(iter = 1)
  
  for (i in 2:n_iter){
    sol <- bind_rows(
      sol, maltus_simulation(x0 = x0,
                             t0 = t0,
                             r = r,
                             alpha = alpha,
                             t = t,
                             ro = ro) %>%  
        mutate(iter = i)
    )
  }
  sol
}


multiple_simulation_maltus(x0 = 5000000000,t0 = 0,r = 1,alpha = 0.5,t = 5,ro = 0.2, n_iter = 10) %>% 
  ggplot(aes(ts,log(xs+1), color = as.factor(iter)  ))+ #, group=as.factor(iter)
  geom_line() +
  labs(c("Xt","Et"))+
  ylab("Xt")+
  xlab("t")+
  ggtitle("Simulación del modelo de Maltus ")
  