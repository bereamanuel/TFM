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

p2 <- maltus3_t20 %>% filter(Type != "Et") %>% filter(Iter == 1) %>% 
  ggplot(aes(x = ts))+
  geom_line( aes(y = xs, color = "Observada")) +
  geom_line(data = maltus_t20 %>% filter(Type == "Et") %>% filter(Iter == 1) %>% filter(variables == "X0 = 5e+05, r = -0.155, alpha = 0.3"), aes(y = xs, color = "Teórica")) +
  facet_wrap(~variables)+
  ylab("Xt")+
  xlab("t")+
  scale_colour_manual("Method", 
                      breaks = c("Observada", "Teórica"),
                      values = c("Observada"="black", 
                                 "Teórica"="red"))+
  ggtitle("Modelo de Maltus")
  

savePlot(p2,"Modelo_Maltus_1iter.jpeg")

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

savePlot(p_eu,"Modelo_Maltus_EM.jpeg")

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

savePlot(p_m,"Modelo_Maltus_Milstein.jpeg")

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

savePlot(p_rk,"Modelo_Maltus_RK.jpeg")

## Runge-Kuta o4 simulation ----------------------------------------------------
M_MU <- function(x,t){
  mu*x
}

M_SIGMA <- function(x,t){
  alpha*x
}

mu <- r-alpha^2/2

rk4 <- rk4_simulation(x0,t0,tn,N,M_MU,M_SIGMA,times)

p_rk4 <-
  rk4 %>% 
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
  ggtitle(paste0(times," soluciones modelo Maltus por Runge Kutta 4"))

p_rk4

# savePlot(p_rk4,"Modelo_Maltus_RK_v2.jpeg")


# ---------------------------------------
maltus <- bind_rows(
  m %>% 
    group_by(ts) %>% 
    summarise(es = mean(ys),
              std = sd(ys),
              type = "Euler"),
  mils %>% 
    group_by(ts) %>% 
    summarise(es = mean(ys),
              std = sd(ys),
              type = "Milstein")
)

comp <- maltus %>% 
            mutate(Method = as.factor(type)) %>% 
            ggplot(aes(x = ts)) +
            geom_line(aes(y = es, color = Method) )+
            geom_line( data = maltus3_t20 %>% filter(Type == "Et") %>% filter(Iter == 1), aes(x=ts, y = xs, color = "Teórica")) +
            geom_ribbon(data = maltus3_t20 %>% filter(Type == "Et") %>% filter(Iter == 1),aes(x =ts, ymin = (xs-sd*1.96) , ymax = (xs+sd*1.96)), fill = "grey70", alpha = 0.2) +
            xlab("t")+
            ylab("Es")+
            scale_colour_manual("Method", 
                                breaks = c("Euler","Milstein","RK2","RK4", "Teórica"),
                                values = c("Euler"="red", 
                                           "Milstein"="blue",
                                           "Teórica"="black"))+
            ggtitle("Modelo de Maltus")

gt_two_column_layout( list(
  gt(maltus %>% filter(type  == "Euler") %>% select(ts, euler = es) %>%
  inner_join(maltus %>% filter(type  == "Milstein") %>% select(ts, milstein = es) , by = "ts") %>% 
  inner_join(maltus3_t20 %>% filter(Type == "Et") %>% filter(Iter == 1) %>% select(ts, et = xs) , by = "ts") %>% 
  mutate(error_euler = et-euler,
         error_1_euler = abs(error_euler),
         error_2_euler = error_euler^2,
         error_milst = et-milstein,
         error_1_milst = abs(error_milst),
         error_2_milst = error_milst^2) %>% 
  summarise(RMAE_Euler = round(sqrt(sum(error_1_euler)/(N+1)),2),
            RMAE_Milst = round(sqrt(sum(error_1_milst)/(N+1)),2),
            RMSE_Euler = round(sqrt(sum(error_2_euler)/(N+1)),2),
            RMSE_Milst = round(sqrt(sum(error_2_milst)/(N+1)),2)) %>% 
  gather(key = "Medida", value = "Error") %>% 
  mutate(Method = str_sub(Medida,start = -5),
         Medida = str_sub(Medida,start = 1,end = 4)) %>% 
    filter(Medida == "RMAE") %>% 
    select(Method,Error) )%>% 
  tab_header(
    title = "RMAE",
  )  ,
  gt(maltus %>% filter(type  == "Euler") %>% select(ts, euler = es) %>%
       inner_join(maltus %>% filter(type  == "Milstein") %>% select(ts, milstein = es) , by = "ts") %>% 
       inner_join(maltus3_t20 %>% filter(Type == "Et") %>% filter(Iter == 1) %>% select(ts, et = xs) , by = "ts") %>% 
       mutate(error_euler = et-euler,
              error_1_euler = abs(error_euler),
              error_2_euler = error_euler^2,
              error_milst = et-milstein,
              error_1_milst = abs(error_milst),
              error_2_milst = error_milst^2) %>% 
       summarise(RMAE_Euler = round(sqrt(sum(error_1_euler)/(N+1)),2),
                 RMAE_Milst = round(sqrt(sum(error_1_milst)/(N+1)),2),
                 RMSE_Euler = round(sqrt(sum(error_2_euler)/(N+1)),2),
                 RMSE_Milst = round(sqrt(sum(error_2_milst)/(N+1)),2)) %>% 
       gather(key = "Medida", value = "Error") %>% 
       mutate(Method = str_sub(Medida,start = -5),
              Medida = str_sub(Medida,start = 1,end = 4)) %>% 
       filter(Medida == "RMSE") %>% 
       select(Method,Error) )%>% 
    tab_header(
      title = "RMSE",
    ) 
  )
)

savePlot(comp,"comparision_maltus.jpeg")
