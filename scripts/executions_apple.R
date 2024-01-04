#Executions Apple
# Upload data
get_data <- function(x){
  stocks <- Ticker$new(x)
  stocks$get_history(period = "1y", interval = "1d", start = NULL, end = NULL)
}

df <- get_data('aapl') %>% 
  transmute(date = ymd(as.Date(date)),
            t = row_number(),
            value = log(adj_close))

t0 <- df$t[1]
tn <- df$t[length(df$t)]
N <- dim(df)[1]
x0 <- df$value[1]
times <- 1000

estimador <- estimacionMV(df)

## E-M Execution ------------------
M_MU <- function(x,t){
  estimador$muE*x
}

M_SIGMA <- function(x,t){
  estimador$siE*x
}

m<- euler_simulation(x0,t0,tn,N,M_MU,M_SIGMA,times) 

## Milstein Execution -------------------
dM_SIGMA <- function(x,t){
  estimador$siE
}

mils <- milstein_simulation(x0,t0,tn,N,M_MU,M_SIGMA,dM_SIGMA,times) 


## Modelo Log-Normal ----------------------------------------
modelo<- validacion(estimador,df) %>% 
  as.data.frame() %>% 
  mutate(date = df$date)

plot10<- m %>% 
  group_by(ts) %>% 
  summarise(es = mean(ys)) %>% 
  ggplot(aes(x = ts, y = es)) +
  geom_line(aes(color = "E-M")) + 
  geom_line(data = mils %>%  group_by(ts) %>% summarise(es = mean(ys)) , aes(x = ts, y = es, color = "Milstein")) + 
  geom_line(data = df, aes(x = t, y = value, color = "Cierre"))+
  geom_line(data = modelo %>% mutate(ts = row_number()), aes(x = ts, y = mm, color = "Modelo Log-Normal"))+
  ylab("Log cierre")+
  xlab("t")+
  scale_colour_manual("Métodos", 
                      breaks = c("Cierre", "E-M","Milstein","Modelo Log-Normal"),
                      values = c("Cierre"="black", 
                                 "E-M"="red",
                                 "Milstein" = "blue",
                                 "Modelo Log-Normal" = "purple"))+
  ggtitle("Cierre L5Y Apple Inc.")

savePlot(plot10,"comp_L5Y.jpeg")

df %>%  
  rename(ts = t) %>% 
  inner_join(modelo %>% mutate(ts = row_number()), by = "ts") %>% 
  inner_join(m %>%  group_by(ts) %>% summarise(m = mean(ys)) %>% head(-1) %>%  mutate(ts = df$t ) , by = "ts") %>% 
  inner_join(mils %>%  group_by(ts) %>% summarise(mils = mean(ys)) %>% head(-1) %>% mutate(ts = df$t ) , by = "ts") %>% 
  mutate(error_mu = value-mean(value),
         error_ln = value-mm,
         error_em = value-m,
         error_mi = value-mils,
         error_mu_1 = abs(error_mu),
         error_mu_2 = error_mu^2,
         error_ln_1 = abs(error_ln),
         error_ln_2 = error_ln^2,
         error_em_1 = abs(error_em),
         error_em_2 = error_em^2 ,
         error_mi_1 = abs(error_mi),
         error_mi_2 = error_mi^2  ) %>% 
  summarise(RMAE_mu = round(sqrt(sum(error_mu_1)/(N+1)),4),
            RMSE_mu = round(sqrt(sum(error_mu_2)/(N+1)),4),
            RMAE_ln = round(sqrt(sum(error_ln_1)/(N+1)),4),
            RMSE_ln = round(sqrt(sum(error_ln_2)/(N+1)),4),
            RMAE_em = round(sqrt(sum(error_em_1)/(N+1)),4),
            RMSE_em = round(sqrt(sum(error_em_2)/(N+1)),4),
            RMAE_mi = round(sqrt(sum(error_mi_1)/(N+1)),4),
            RMSE_mi = round(sqrt(sum(error_mi_2)/(N+1)),4))

