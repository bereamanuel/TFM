# Libraries
packages <- c("ggplot2", "dplyr", "tidyr", "lubridate", "purrr", "yahoofinancer")
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


