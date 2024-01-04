# Libraries
packages <- c("ggplot2","scales","dplyr", "tidyr", "lubridate", "purrr", "yahoofinancer","gt","gtExtras","stringr","gridExtra")
for(p in packages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# Functions 
savePlot <- function(plot,name){
  plot
  ggsave(path = paste0(getwd(),"/plots/"),
         filename  = name,
         width = 12000,
         height= 8000, 
         units= "px", 
         device='jpeg', 
         dpi=1200,
         limitsize = FALSE)}

my_gt_fn <- function(x) {
  gt(x) %>%
    gtExtras::gt_color_rows(columns = row_n, domain = 1:32)
}

