#Maltus simulation  ------------------------------------------------------------
t0 <- 0
tn <- 2
N <- 100
x0 <- 500000
times <- 1000
r <- -0.155
alpha <- 0.3


maltus1_t20 <- maltus_simulation(x0 = x0,t0 = t0,r = 0.245,alpha = alpha,t = tn,N = N, times = times)
maltus2_t20 <- maltus_simulation(x0 = x0,t0 = t0,r = 0.045,alpha = alpha,t = tn,N = N, times = times)
maltus3_t20 <- maltus_simulation(x0 = x0,t0 = t0,r = -0.155,alpha = alpha,t =tn,N = N, times = times)

maltus_t20 <- bind_rows(maltus1_t20,
                        maltus2_t20,
                        maltus3_t20)             

p <-   
  maltus_t20 %>% filter(Type == "Et") %>% filter(Iter == 1) %>% 
  ggplot(aes(x = ts))+
  geom_ribbon(aes( ymin = (xs-sd*1.96) , ymax = (xs+sd*1.96)), fill = "grey70", alpha = 0.2) +
  # geom_line( aes(y = es , color = "Media Muestral" ), linetype = "dashed") +
  geom_line(data = maltus_t20 %>% filter(Type == "Et") %>% filter(Iter == 1), aes(y = xs)) +
  facet_wrap(~variables)+
  ylab("Et")+
  xlab("t")+
  ggtitle("Modelo de Maltus")

p

# savePlot(p,"Modelo_Maltus_t1.jpeg")

## E-M Execution ------------------
M_MU <- function(x,t){
  mu*x
}

M_SIGMA <- function(x,t){
  alpha*x
}

mu <- r-alpha^2/2

m <- euler_simulation(x0,t0,tn,N,M_MU,M_SIGMA,times)

p_eu <-
  m %>% 
  group_by(ts) %>% 
  mutate(std = sd(ys),
         es = mean(ys)) %>% 
  ungroup() %>% 
  mutate(std_min = es-std*1.96,
         std_max = es+std*1.96) %>% 
  ggplot(aes(x = ts)) +
  geom_line(aes(y = es, color = "Muestral"),  linetype = "dashed" )+
  geom_line( data = maltus3_t20 %>% filter(Type == "Et") %>% filter(Iter == 1), aes(x=ts, y = xs, color = "Teórica")) +
  geom_ribbon(data = maltus3_t20 %>% filter(Type == "Et") %>% filter(Iter == 1),aes( ymin = (xs-sd*1.96) , ymax = (xs+sd*1.96)), fill = "grey70", alpha = 0.2) +
  ylab("Xt")+
  xlab("t")+
  scale_colour_manual("Media", 
                      breaks = c("Muestral", "Teórica"),
                      values = c("Muestral"="red", "Teórica"="black"))+
  theme(legend.position = c(0.9, 0.9),
        legend.background = element_rect(fill = alpha("white",0.5)) )+
  ggtitle(paste0(times," soluciones modelo Maltus por Euler-Maruyama"))
p_eu

# savePlot(p_eu,"Modelo_Maltus_EM.jpeg")

## Milstein Execution -------------------
M_MU <- function(x,t){
  mu*x
}

M_SIGMA <- function(x,t){
  alpha*x
}

dM_SIGMA <- function(x,t){
  alpha
}

mu <- r-alpha^2/2

mils <- milstein_simulation(x0,t0,tn,N,M_MU,M_SIGMA,dM_SIGMA,times)

p_m <-
  mils %>% 
  group_by(ts) %>% 
  mutate(std = sd(ys),
         es = mean(ys)) %>% 
  ungroup() %>% 
  mutate(std_min = es-std*1.96,
         std_max = es+std*1.96) %>% 
  ggplot(aes(x = ts)) +
  geom_line(aes(y = es, color = "Muestral"),  linetype = "dashed" )+
  geom_line( data = maltus3_t20 %>% filter(Type == "Et") %>% filter(Iter == 1), aes(x=ts, y = xs, color = "Teórica")) +
  geom_ribbon(data = maltus3_t20 %>% filter(Type == "Et") %>% filter(Iter == 1),aes( ymin = (xs-sd*1.96) , ymax = (xs+sd*1.96)), fill = "grey70", alpha = 0.2) +
  ylab("Xt")+
  xlab("t")+
  scale_colour_manual("Media", 
                      breaks = c("Muestral", "Teórica"),
                      values = c("Muestral"="red", "Teórica"="black"))+
  theme(legend.position = c(0.9, 0.9),
        legend.background = element_rect(fill = alpha("white",0.5)) )+
  ggtitle(paste0(times," soluciones modelo Maltus por Milstein"))

p_m

# savePlot(p_m,"Modelo_Maltus_Milstein.jpeg")

## Runge-Kuta simulation ----------------------------------------------------
M_MU <- function(x,t){
  mu*x
}

M_SIGMA <- function(x,t){
  alpha*x
}

mu <- r-alpha^2/2

rk <- rk_simulation(x0,t0,tn,N,M_MU,M_SIGMA,times)

p_rk <-
  rk %>% 
  group_by(ts) %>% 
  mutate(std = sd(ys),
         es = mean(ys)) %>% 
  ungroup() %>% 
  mutate(std_min = es-std*1.96,
         std_max = es+std*1.96) %>% 
  ggplot(aes(x = ts)) +
  geom_line(aes(y = es, color = "Muestral"),  linetype = "dashed" )+
  geom_line( data = maltus3_t20 %>% filter(Type == "Et") %>% filter(Iter == 1), aes(x=ts, y = xs, color = "Teórica")) +
  geom_ribbon(data = maltus3_t20 %>% filter(Type == "Et") %>% filter(Iter == 1),aes( ymin = (xs-sd*1.96) , ymax = (xs+sd*1.96)), fill = "grey70", alpha = 0.2) +
  ylab("Xt")+
  xlab("t")+
  scale_colour_manual("Media", 
                      breaks = c("Muestral", "Teórica"),
                      values = c("Muestral"="red", "Teórica"="black"))+
  theme(legend.position = c(0.9, 0.9),
        legend.background = element_rect(fill = alpha("white",0.5)) )+
  ggtitle(paste0(times," soluciones modelo Maltus por Runge Kutta"))

p_rk

# savePlot(p_rk,"Modelo_Maltus_RK.jpeg")

## Runge-Kuta simulation ----------------------------------------------------
M_MU <- function(x,t){
  mu*x
}

M_SIGMA <- function(x,t){
  alpha*x
}

mu <- r-alpha^2/2

rk2 <- rk2_simulation(x0,t0,tn,N,M_MU,M_SIGMA,times)

p_rk2 <-
  rk2 %>% 
  group_by(ts) %>% 
  mutate(std = sd(ys),
         es = mean(ys)) %>% 
  ungroup() %>% 
  mutate(std_min = es-std*1.96,
         std_max = es+std*1.96) %>% 
  ggplot(aes(x = ts)) +
  geom_line(aes(y = es, color = "Muestral"),  linetype = "dashed" )+
  geom_line( data = maltus3_t20 %>% filter(Type == "Et") %>% filter(Iter == 1), aes(x=ts, y = xs, color = "Teórica")) +
  geom_ribbon(data = maltus3_t20 %>% filter(Type == "Et") %>% filter(Iter == 1),aes( ymin = (xs-sd*1.96) , ymax = (xs+sd*1.96)), fill = "grey70", alpha = 0.2) +
  ylab("Xt")+
  xlab("t")+
  scale_colour_manual("Media", 
                      breaks = c("Muestral", "Teórica"),
                      values = c("Muestral"="red", "Teórica"="black"))+
  theme(legend.position = c(0.9, 0.9),
        legend.background = element_rect(fill = alpha("white",0.5)) )+
  ggtitle(paste0(times," soluciones modelo Maltus por Runge Kutta v2"))

p_rk2

# savePlot(p_rk2,"Modelo_Maltus_RK_v2.jpeg")


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

## Milstein Simulation --------------------------------------------------------------
ou_rk <- rk_simulation(x0,t0,tn,N,OU_MU,OU_SIGMA,times)

ou_rk %>% 
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
            type = "RK")
)


ou %>% 
  ggplot(aes(x = ts, y = es, color = as.factor(type))) +
  geom_line()+
  geom_ribbon(data = ou %>% filter(type == "Euler") ,aes( ymin = (es-std *1.96) , ymax = (es+std *1.96)), fill = "grey70", alpha = 0.2) +
  geom_ribbon(data = ou %>% filter(type != "Euler") ,aes( ymin = (es-std *1.96) , ymax = (es+std *1.96)), fill = "grey70", alpha = 0.2) +
  xlab("t")+
  ylab("Es")+
  ggtitle("Modelo de Ornstein-Uhlenbeck")

