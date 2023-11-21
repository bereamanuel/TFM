
# Modelo Ornstein-Uhlenbeck ------------------------------------------------------
theta <- 0.7
mu <- 1.5
sigma <- 0.06

OU_MU <- function(x,t){
  theta*(mu-x)
}

OU_SIGMA <- function(x,t){
  sigma
}

t0 <- 3
tn <- 7
N <- 1000
x0 <- 0
times <- 100

## E-M Simulation --------------------------------------------------------------
ou_em <- euler_simulation(x0,t0,tn,N,OU_MU,OU_SIGMA,times)

ou_em %>% 
  ggplot(aes(x = ts, y = ys, color= as.factor(iter)))+
  geom_line()+ 
  geom_line( data = ou_em %>% group_by(ts) %>% summarise(ys = mean(ys)), aes(x = ts, y=ys, color = "mean"),  color = "black" )

## RK 2 Simulation --------------------------------------------------------------
ou_rk <- rk_simulation(x0,t0,tn,N,OU_MU,OU_SIGMA,times)

ou_rk %>% 
  ggplot(aes(x = ts, y = ys, color= as.factor(iter)))+
  geom_line()+ 
  geom_line( data = ou_rk %>% group_by(ts) %>% summarise(ys = mean(ys)), aes(x = ts, y=ys, color = "mean"),  color = "black" )

## RK 4 Simulation --------------------------------------------------------------
ou_rk4 <- rk4_simulation(x0,t0,tn,N,OU_MU,OU_SIGMA,times)

ou_rk4 %>% 
  ggplot(aes(x = ts, y = ys, color= as.factor(iter)))+
  geom_line()+ 
  geom_line( data = ou_rk %>% group_by(ts) %>% summarise(ys = mean(ys)), aes(x = ts, y=ys, color = "mean"),  color = "black" )




ou <- bind_rows(
  ou_em %>% 
    group_by(ts) %>% 
    summarise(es = mean(ys),
              std = sd(ys),
              type = "Euler"),
  ou_rk %>% 
    group_by(ts) %>% 
    summarise(es = mean(ys),
              std = sd(ys),
              type = "RK"),
  ou_rk4 %>% 
    group_by(ts) %>% 
    summarise(es = mean(ys),
              std = sd(ys),
              type = "RK4")
)


ou %>% 
  ggplot(aes(x = ts, y = es, color = as.factor(type))) +
  geom_line()+
  geom_ribbon(data = ou %>% filter(type == "Euler") ,aes( ymin = (es-std *1.96) , ymax = (es+std *1.96)), fill = "grey70", alpha = 0.2) +
  geom_ribbon(data = ou %>% filter(type == "RK") ,aes( ymin = (es-std *1.96) , ymax = (es+std *1.96)), fill = "grey70", alpha = 0.2) +
  geom_ribbon(data = ou %>% filter(type == "RK4") ,aes( ymin = (es-std *1.96) , ymax = (es+std *1.96)), fill = "grey70", alpha = 0.2) +
  xlab("t")+
  ylab("Es")+
  ggtitle("Modelo de Ornstein-Uhlenbeck")


