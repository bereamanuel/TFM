# Upload data
get_data <- function(x){
  stocks <- Ticker$new(x)
  stocks$get_history(period = "5y", interval = "1d", start = NULL, end = NULL)
}

df <- get_data('aapl') %>% 
  transmute(date = ymd(as.Date(date)),
            t = row_number(),
            value = log(adj_close))

df_decomp<- decompose(df %>%  
              select(value) %>% 
              ts(frequency=365, start=c("2019",1)), type ="additive") 



acc_apple <- df %>% 
              ggplot(aes(date, value,color = "Valor cierre")) + 
              geom_line()+  
              geom_line(data = df_decomp$trend %>% as.data.frame() %>%  mutate(date = df$date) , aes(x = date, y = V1, color = "Tendencia") )+ 
              ylab("Precio de cierre")+
              xlab("t")+
              scale_colour_manual("", 
                                  breaks = c("Valor cierre", "Tendencia"),
                                  values = c("Valor cierre"="black", 
                                             "Tendencia"="red"))+
              ggtitle("Cotización últimos 5 años de acción de Apple")



savePlot(acc_apple,"acc_apple_L5Y.jpeg")



estimador <- estimacionMV(df)

modelo<- validacion(estimador,df) %>% 
  as.data.frame() %>% 
  mutate(date = df$date)

log_ajus <- df %>% 
  ggplot(aes(x = date))+
  geom_line(aes(y = value,color = "Cierre")) + 
  geom_line(data = modelo, aes(y = mm, color = "Media estimada"))+
  geom_ribbon(data = modelo, aes(ymin = (mm-dsm*1.96) , ymax = (mm+dsm*1.96)), fill = "grey70", alpha = 0.2) +
  ylab("Log cierre")+
  xlab("t")+
  scale_colour_manual("", 
                      breaks = c("Cierre", "Media estimada"),
                      values = c("Cierre"="black", 
                                 "Media estimada"="red"))+
  ggtitle("Log-Normal ajustado a cotizaciones al cierre L5Y Apple Inc.")


savePlot(log_ajus,"log_ajus_L5Y.jpeg")


modelo %>%  inner_join(df, by = "date") %>% 
  mutate(error = value-mm,
          error_1 = abs(error),
          error_2 = error^2) %>% 
  summarise(RMAE = round(sqrt(sum(error_1)/(N+1)),4),
            RMSE = round(sqrt(sum(error_2)/(N+1)),4))

# setClass('metodo_EM',
#          slots = c(
#            THETA = 'numeric',
#            MU = 'numeric',
#            SIGMA = 'numeric',
#            DATA =  'data.frame')
#          )
# # MU
# setGeneric("mu", function(x,y,t)
#   standardGeneric("mu"))
# setMethod(
#   "mu",
#   signature = "metodo_EM",
#   definition = function(x,y,t){
#     x@THETA * (x@MU - y)
#   })
# # SIGMA
# setGeneric("sigma", function(x,y,t)
#   standardGeneric("sigma"))
# setMethod(
#   "sigma",
#   signature = "metodo_EM",
#   definition = function(x,y,t){
#     x@SIGMA
#   })
# # dW
# setGeneric("dW", function(x,dt)
#   standardGeneric("dW"))
# setMethod(
#   "dW",
#   signature = "metodo_EM",
#   definition = function(x,dt){
#     rnorm(n=1, mean = 0, sd = sqrt(dt))
#   })
# # Simulation
# setGeneric("simulation", function(x,...)
#   standardGeneric("simulation"))
# setMethod(
#   "simulation",
#   signature = "metodo_EM",
#   definition = function(x,...){
#     t0 <- x@DATA$t[1] %>% as.numeric()
#     tn <- tail(x@DATA$t,1) %>% as.numeric()
#     N  <- dim(x@DATA)[1] %>% as.numeric()
#     dt <- (tn-t0)/n
#     ts <- seq(t0,tn)
#     
#     y0 <- x@DATA$value[1]
#     ys <- rep(0,N)
#     ys[1] <- y0
#     
#     for (i in seq(2,N)){
#       t <- t0 + (i-1)*dt
#       y <- ys[i-1]
#       ys[i] <- y + mu(ejemplo,y,t)*dt + sigma(ejemplo,y,t)*dW(ejemplo,dt)
#     }
#     
#     list(ts = ts, ys = ys)
#     
#   })
# # plot
# setGeneric("plot", function(x,...)
#   standardGeneric("plot"))
# setMethod(
#   "plot",
#   signature = "metodo_EM",
#   definition = function(x,num_sim){
#     simu <- data.frame()
#     for (i in 1:num_sim){
#       simu <- bind_rows(simu,simulation(ejemplo) %>% as.data.frame() %>%  mutate(sim = i))
#     }
#     simu <<- bind_rows(simu)
#     
#     # ggplot(simu, aes(ts,ys,color=as.factor(sim)))+
#     #   geom_line()+
#     #   stat_summary(aes(color=as.factor(sim)), fun=mean, geom="line", colour="black")
#   })
# 
# 
# ejemplo <- new("metodo_EM",
#                   THETA = 0.01,
#                   MU = mean(df$value),
#                   SIGMA = sd(df$value),
#                   DATA  = df)
# 
# plot(ejemplo,100)
# 
# simu %>% 
#   group_by(ts) %>% 
#   summarise(ys = exp(mean(ys))) %>% 
#   ungroup() %>% 
#   mutate(sim="media") %>% 
#   bind_rows(df %>% transmute(ts = t, ys = exp(value),sim = "original" ) ) %>% 
#   ggplot(aes(ts,ys,color=sim))+geom_line()
