maltus_simulation <- function(x0,t0,r,alpha,t,ro, times){
  ## Maltus simulation  
  ## ------------------
  ## x0     : Initial value
  ## t0     : Initial time
  ## r      : Constant
  ## alpha  : Constant
  ## t      : Final time
  ## r0     : Step time
  ## times  : Num simulations
  ## 
  ## --------------------
  ## 
  ## mu = r- 1/2*alpha^2
  ## if r > alpha^2/2 -> Xt -> inf
  ## if r < alpha^2/2 -> Xt -> 0
  ## if r = alpha^2/2 -> E(Xt) -> X_0
  ##
  
    mu <- r-alpha^2/2
    ts <- seq(t0,t,ro)
    
    xs <- rep(0,t)
    es <- rep(0,t)
    sd <- rep(0,t)
    
    maltus <- data.frame()
    
    for (time in 1:times){
      
      for (i in 1:length(ts)){
        xs[i] <- x0*exp(mu*ts[i] + alpha*rnorm(1,0,ts[i]))
        es[i] <- x0*exp(mu*ts[i])
        sd[i] <- sqrt( (x0^2)*exp(2*mu*ts[i])*(exp((alpha^2)*ts[i])-1) )
      }
      
      maltus_aux <- as.data.frame(list(ts = ts, xs = xs, es = es, sd = sd))
      
      maltus <- bind_rows(maltus,
                          bind_rows( maltus_aux %>% select(ts,xs, sd = sd) %>%  mutate(xs = xs, 
                                                                              Type = "Xt",
                                                                              Iter=time, 
                                                                              variables = paste0("X0 = ",x0,", r = ",r,", alpha = ",alpha)),
                                     maltus_aux %>% select(ts,xs=es, sd = sd) %>%  mutate(xs = xs, 
                                                                                 Type = "Et",
                                                                                 Iter=time, 
                                                                                 variables = paste0("X0 = ",x0,", r = ",r,", alpha = ",alpha)))
                          
                          )
    }
    
    maltus
}

times <- 20

maltus1_t20 <- maltus_simulation(x0 = 50,t0 = 0,r = 0.245,alpha = 0.3,t = 20,ro = 0.2, times = times)
maltus2_t20 <- maltus_simulation(x0 = 50,t0 = 0,r = 0.045,alpha = 0.3,t = 20,ro = 0.2, times = times)
maltus3_t20 <- maltus_simulation(x0 = 50,t0 = 0,r = -0.155,alpha = 0.3,t = 20,ro = 0.2, times = times)
   
maltus_t20 <- bind_rows(maltus1_t20,
                        maltus2_t20,
                        maltus3_t20)             

p <- 
  maltus_t20 %>% 
  filter(Type == "Et") %>% View()
      ggplot(aes(x = ts, group = Type))+
      geom_ribbon(aes(ymin = log((xs+sd +1), ymax = ifelse(is.na(log((xs-sd))),0,log((xs-sd)+1))), fill = "grey70", alpha = 0.2) +
      geom_line(aes(y = log(xs+1))) + 
      facet_wrap(~variables)+
      ylab("Log Et")+
      xlab("t")+
      ggtitle("Simulación del modelo de Maltus")

savePlot(p,"Modelo_Maltus_t50.jpeg")

