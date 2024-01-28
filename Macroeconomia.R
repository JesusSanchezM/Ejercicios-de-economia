#-----------------------------
#------Macroeconomía---------
#-----------------------------

library(tidyverse)

#Actividad 1 
{
#`Ocurre un...`s en la demanda y la oferta

Precio <- c(10, 20, 30, 40, 50, 60)
Cantidad_original <- c(100, 90, 80, 70, 60, 50)
Demanda <- data.frame(Precio, Cantidad_original)

ggplot() + 
  geom_line(aes(x=Cantidad_original, y=Precio), color="black", size=1.5) + 
  theme_classic()

}

# Gráficas incisos a) al d)
{
  
Incremento_Bien_normal <- Cantidad_original+10
Incremento_Bien_inferior <- Cantidad_original-10
Decremento_Bien_normal <- Incremento_Bien_inferior
Decremento_Bien_inferior <- Incremento_Bien_normal

Demanda <- data.frame(Precio, Cantidad_original, Incremento_Bien_normal,
                      Incremento_Bien_inferior, Decremento_Bien_normal,
                      Decremento_Bien_inferior)
    
Demanda_1 <- Demanda %>% gather(key="Ocurre un...",value = "Cantidad_original",-Precio)


ggplot(Demanda_1, aes(x = Cantidad_original, y = Precio, group = `Ocurre un...`)) +
  geom_line(aes(color = `Ocurre un...`, linetype = `Ocurre un...`), size = 1.5) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_classic() +
  labs(title = "Movimientos en la demanda",
       subtitle = "Variación por unidad",
       x = "Cantidad", y = "Precio",
       caption = "Fuente: Elaboración propia con datos propios") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.caption = element_text(hjust = 0, face = "bold", size = 10),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_manual(values = c("blue", "orange", "green", "purple", "red")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dashed", "dashed"))

}

