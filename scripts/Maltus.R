# Modelo de Maltus

setClass('Maltus',
         slots = c(
           X0 = 'numeric',
           MU = 'numeric',
           ALPHA = 'numeric')
)
# Xt
setGeneric("Xt", function(x,t)
  standardGeneric("Xt"))
setMethod(
  "Xt",
  signature = "Maltus",
  definition = function(x,t){
    xt <- x@X0 * exp(x@MU * t + x@ALPHA * rnorm(1,0,t))
    xt
  })
# MU
setGeneric("mu", function(x,t)
  standardGeneric("mu"))
setMethod(
  "mu",
  signature = "Maltus",
  definition = function(x,t){
    x@X0 * exp(x@MU * t)
  })
# Simulation
setGeneric("simulation", function(x,tn)
  standardGeneric("simulation"))
setMethod(
  "simulation",
  signature = "Maltus",
  definition = function(x,tn){
    t0 <- 1
    ts <- seq(t0+1,tn)
    
    xs <- rep(0,tn)
    xs[1] <- x@X0
    
    es <- rep(0,tn)
    es[1] <- mu(x,t0)
    
    for (t in ts){
      xs[t] <- Xt(maltus_1,t)
      es[t] <- mu(maltus_1,t)
    }
    
    as.data.frame(list(ts = c(t0,ts), xs = xs, es = es))
    
  })

maltus_1 <- new("Maltus",
                X0 = 2,
                MU = 2,
                ALPHA = -10)

maltus_2 <- new("Maltus",
                X0 = 2,
                MU = 2,
                ALPHA = 1)

maltus1_t50 <- simulation(maltus_1,50)
maltus1_t50 <- bind_rows( maltus1_t50 %>% select(ts,xs) %>%  mutate(xs = log(xs+1), type = "Xt", variables = "X0 = 2, mu = 1, alpha = 1"),
                          maltus1_t50 %>% select(ts,xs=es) %>%  mutate(xs = log(xs+1), type = "Et", variables = "X0 = 2, mu = 1, alpha = 1"))

maltus2_t50 <- simulation(maltus_2,50)
maltus2_t50 <- bind_rows( maltus2_t50 %>% select(ts,xs) %>%  mutate(xs = log(xs+1), type = "Xt", variables = "X0 = 2, mu = 5, alpha = -1"),
                          maltus2_t50 %>% select(ts,xs=es) %>%  mutate(xs = log(xs+1), type = "Et", variables = "X0 = 2, mu = 5, alpha = -1"))
   
maltus_t50 <- bind_rows(maltus1_t50,
                        maltus2_t50)             

p<- maltus_t50 %>% 
  ggplot(aes(ts,xs, color = type))+
  facet_wrap(~variables)+
  geom_line() + 
  scale_color_manual(values=c("#EE2C2C", "#000000"))+
  labs(c("Xt","Et"))+
  ylab("Xt")+
  xlab("t")+
  ggtitle("Simulación del modelo de Maltus")

savePlot(p,"Modelo_Maltus_t50.jpeg")
