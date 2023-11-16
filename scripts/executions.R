source("libraries.R")
source("Maltus.R")
#Maltus simulation  ------------------------------------------------------------
t0 <- 0
tn <- 2
N <- 100
x0 <- 500000
times <- 1000
r <- -0.155
sigma <- 0.3


maltus1_t20 <- maltus_simulation(x0 = x0,t0 = t0,r = 0.245,alpha = alpha,t = tn,N = N, times = times)
maltus2_t20 <- maltus_simulation(x0 = x0,t0 = t0,r = 0.045,alpha = alpha,t = tn,N = N, times = times)
maltus3_t20 <- maltus_simulation(x0 = x0,t0 = t0,r = -0.155,alpha = alpha,t =tn,N = N, times = times)

maltus_t20 <- bind_rows(maltus1_t20,
                        maltus2_t20,
                        maltus3_t20)             

p <- maltus_t20 %>%
  filter(Type != "Et") %>% 
  group_by(Type,variables,ts) %>% 
  mutate( es = sum(xs)/times,
          sd = sd(xs) )  %>% 
  ggplot(aes(x = ts))+
  geom_ribbon(aes( ymin = (es-sd*1.96) , ymax = (es+sd*1.96)), fill = "grey70", alpha = 0.2) +
  geom_line( aes(y = es , color = "Media Muestral" ), linetype = "dashed") +
  geom_line(data = maltus_t20 %>% filter(Type == "Et") %>% filter(Iter == 1), aes(y = xs, color = "Media Teórica")) +
  facet_wrap(~variables)+
  scale_colour_manual("", 
                      breaks = c("Media Muestral", "Media Teórica"),
                      values = c("Media Muestral"="red", "Media Teórica"="black"))+
  ylab("Xt")+
  xlab("t")+
  ggtitle("1000 simulaciones del modelo de Maltus")

p
# savePlot(p,"Modelo_Maltus_t1.jpeg")

# E-M Execution
M_MU <- function(x,t){
  mu*x
}

M_SIGMA <- function(x,t){
  sigma*x
}

mu <- r-alpha^2/2

m <- euler_simulation(x0,t0,tn,N,M_MU,M_SIGMA,times)

p_eu <-
  m %>% 
  group_by(ts) %>% 
  mutate(std_min = ys-sd(ys)*1.96,
         std_max = ys+sd(ys)*1.96,
         es = mean(ys)) %>% 
  ungroup() %>% 
  ggplot(aes(x = ts))+
  geom_line(aes(y = es, color = "Media Muestral"), color = "red",  linetype = "dashed" )+
  geom_ribbon(aes( ymin = std_min , ymax = std_max ), fill = "grey70", alpha = 0.2) +
  geom_line( data = maltus3_t20 %>% filter(Type == "Et") %>% filter(Iter == 1), aes(x=ts, y = xs, color = "Media Teórica"), color = "black") 
p_eu

savePlot(p_eu,"Modelo_Maltus_EM.jpeg")

# Modelo Ornstein-Uhlenbeck
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
ou <- euler_simulation(x0,t0,tn,N,OU_MU,OU_SIGMA,times)

ou %>% 
  ggplot(aes(x = ts, y = ys, color= as.factor(iter)))+
  geom_line()+ 
  geom_line( data = ou %>% group_by(ts) %>% summarise(ys = mean(ys)), aes(x = ts, y=ys, color = "mean"),  color = "black" )









