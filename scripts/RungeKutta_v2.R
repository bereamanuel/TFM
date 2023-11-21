rk4_simulation <- function(x0,t0,tn,N,MU,SIGMA,times){
  ## Runge-Kutta orden 4
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
  ## Yn+1 = Yn + (1/2)*(k1 + k2)
  ## where k1 = MU(yk,tk)*Dt + SIGMA(yk,tk)*(Dwtk - st*sqrt(Dt)) ,
  ## k2 = MU(yk + k1, t(k+1) )+ SIGMA(yk + k1 ,t(k+1))*(Dwtk + st*sqrt(Dt)) ,
  ## dWtn = Wtn+1 - Wtn,
  ## and Sk = +-1 with probability 1/2
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
      
      F1 <- MU(ys[i-1],ts[i-1])
      G1 <- SIGMA(ys[i-1],ts[i-1])
      
      F2 <- MU(ys[i-1] + (1/2)*F1*dt + (1/2)*G1*dwt, ts[i-1]+dt/2)
      G2 <- SIGMA(ys[i-1] + (1/2)*F1*dt + (1/2)*G1*dwt, ts[i-1]+dt/2)
      
      F3 <- MU(ys[i-1] + (1/2)*F2*dt + (1/2)*G2*dwt, ts[i-1]+dt/2)
      G3 <- SIGMA(ys[i-1] + (1/2)*F2*dt + (1/2)*G2*dwt, ts[i-1]+dt/2)
      
      F4 <- MU(ys[i-1] + F3*dt + (1/2)*G3*dwt, ts[i])
      G4 <- SIGMA(ys[i-1] + F3*dt + (1/2)*G3*dwt, ts[i])
      
      #Sol
      ys[i] <- ys[i-1] + (1/6)*((F1+2*F2+2*F3+F4)*dt + (G1+2*G2+2*G3+G4)*dwt)
      
    }
    
    sol_aux <- as.data.frame(list(ts = ts, ys = ys )) %>% mutate( iter = paste0("it_",ti) )
    
    sol <- sol %>% 
      bind_rows(sol_aux) 
    rm(sol_aux)
  }
  
  sol
}
