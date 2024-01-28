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

ggplot() + 
  geom_line(aes(x=Cantidad, y=Precio), color="black", size=1.5) + 
  theme_classic()

}

# Gráficas incisos a) al d)
{
  
Incremento_Bien_normal <- Cantidad+10
Incremento_Bien_inferior <- Cantidad-10
Decremento_Bien_normal <- Incremento_Bien_inferior
Decremento_Bien_inferior <- Incremento_Bien_normal

Demanda <- data.frame(Precio, Cantidad, Incremento_Bien_normal,
                      Incremento_Bien_inferior, Decremento_Bien_normal,
                      Decremento_Bien_inferior)
    
Demanda <- Demanda %>% gather(key="Movimiento",value = "Cantidad",-Precio)


ggplot(Demanda, aes(x = Cantidad, y = Precio, group = Movimiento)) +
  geom_line(aes(color = Movimiento, linetype = Movimiento), size = 1.5) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_classic() +
  labs(title = "Gráfica 9: Bonos a 10 años",
       subtitle = "Variación % diaria",
       x = "Fecha", y = "Porcentaje",
       caption = "Fuente: Elaboración propia con datos del Investing") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.caption = element_text(hjust = 0, face = "bold", size = 10),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_manual(values = c("blue", "orange", "green", "purple", "red")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dashed", "dashed"))

}

