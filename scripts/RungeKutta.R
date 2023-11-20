rk_simulation <- function(x0,t0,tn,N,MU,SIGMA,times){
  ## Runge-Kutta
  ## With one EDE, this function is to simulate solutions.
  ## ------------------
  ## x0     : Initial value
  ## t0     : Initial time
  ## tn     : Final time
  ## N      : Number points to grid
  ## times  : Num simulations
  ## MU     : Function MU
  ## SIGMA  : Function SIGMA
  ## times  : N iterations
  ## ------------------
  ##
  ## Given a EDE like:
  ## dXt = MU(Xt,t)dt + SIGMA(Xt,t)dWt
  ## with initial condition X0 = x0
  ## 
  ## The solution follows the next Markov-Chain:
  ## Let 0 = t0 < t1 < ... < tn = t, where Dt = (t-t0)/N
  ## Let Y0 = x0
  ## Recursively define Yn for 0 <= n <= N-1 by
  ## Yn+1 = Yn + MU(Yn,tn)*Dt + SIGMA(Yn,tn)*dWtn + 
  ##        (1/2)*(SIGMA(ÿn,tn)-SIGMA(Yn,tn))*( (dWtn)^2 - Dt )*((Dt)^(-1/2))
  ## where dWtn = Wtn+1 - Wtn
  ## and ÿn = Yn + MU(Yn,tn)*Dt + SIGMA(Yn,tn)*(Dt^(1/2))
  ##
  
  # Define ts,dt 
  dt <- (tn-t0)/N
  ts <- seq(t0,tn,dt)
  
  
  # Define solution
  sol <- data.frame()
  
  # iteration loop  
  for (ti in seq(1,times+1)){
    
    # Init ys, solution vector
    ys <- rep(0,N+1)
    ys[1] <- x0
    
    # for loop
    for (i in seq(2,N+1)){
      
      dwt <- rnorm(1,0,sqrt(dt))
      
      ys[i] <- ys[i-1] + 
        MU(ys[i-1],ts[i-1])*dt + 
        SIGMA(ys[i-1],ts[i-1])*dwt + 
        (1/2)*(SIGMA(ys[i-1] + MU(ys[i-1],ts[i-1])*dt + SIGMA(ys[i-1],ts[i-1])*(dt^(1/2)),ts[i-1])-SIGMA(ys[i-1],ts[i-1]) )*((dwt)^2 - dt)*(dt^(-1/2))
      
    }
    
    sol_aux <- as.data.frame(list(ts = ts, ys = ys )) %>% mutate( iter = paste0("it_",ti) )
    
    sol <- sol %>% 
      bind_rows(sol_aux) 
    rm(sol_aux)
  }
  
  sol
}
