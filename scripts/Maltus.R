maltus_simulation <- function(x0,t0,r,alpha,t,N, times){
  ## Maltus simulation  
  ## ------------------
  ## x0     : Initial value
  ## t0     : Initial time
  ## r      : Constant
  ## alpha  : Constant
  ## t      : Final time
  ## N      : Number points grid
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
    ro <- (t-t0)/N
    ts <- seq(t0,t,ro)
    
    xs <- rep(0,t)
    es <- rep(0,t)
    sd <- rep(0,t)
    
    maltus <- data.frame()
    
    for (time in 1:times){
      
      for (i in 1:length(ts)){
        xs[i] <- x0*exp(mu*ts[i] + alpha*rnorm(1,0,ts[i] ) )
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

