## Brownian motion

iters <- 2
brownian <- data.frame()
for (i in seq(1,iters)){

t <- 1500 # Tmax

x <- rnorm(t,0,1) # Normal distribution, mean 0, std 1, t times
x <- append(0,x) # Brownian motion starts in 0

x <- cumsum(x) 

brownian <- bind_rows(brownian,
  bind_cols(x,seq(0,t)) %>% 
  rename(y = 1,
         x = 2) %>% 
    mutate(iter = as.character(i)) )
}




brownian <- bind_rows(brownian, 
                      brownian %>% 
                        group_by(x) %>% 
                        summarise(y = mean(y)) %>% 
                        ungroup() %>% 
                        mutate(iter = "E") )

brownian %>% 
  filter(iter %in% c("1")) %>% 
  mutate(iter = as.factor(iter)) %>% 
  ggplot(aes(x = x, y=y))+
  geom_line() 
