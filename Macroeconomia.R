#-----------------------------
#------Macroeconomía---------
#-----------------------------

library(tidyverse)

#Actividad 1 
{
#Cambios en la demanda y la oferta

Precio <- c(10, 20, 30, 40, 50, 60)
Cantidad <- c(100, 90, 80, 70, 60, 50)
Demanda <- data.frame(Precio, Cantidad)

ggplot() + geom_line(aes(x=Cantidad, y=Precio), color="black") + theme_classic()

formattable(Demanda, list(
  Cantidad = color_tile("white", "orange")),
  Precio = color_tile("white", "red"))

 
datatable(Demanda)

kable(Demanda) %>% 
  kable_styling("striped", full_width = T) %>% 
  row_spec(0, bold=T, color="black", background = "turquoise") %>% 
  row_spec(1:6, bold=T, color="black", background = "pink") 
  

}








