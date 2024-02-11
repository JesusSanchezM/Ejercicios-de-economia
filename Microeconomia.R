#-----------------------------
#------Macroeconomía---------
#-----------------------------

library(tidyverse)
library(kableExtra) #tablas estilizadas
library(patchwork) #Combinar graficos


#Actividad 1 
{
#3. Represente gráficamente los movimientos de la demanda y de la oferta (según sea el caso) bajo cada uno de los escenarios siguientes:
# a) Incremento en el ingreso: x es un bien normal
# b) Incremento en el ingreso: x es un bien inferior
# c) Decremento en el ingreso: x es un bien normal
# d) Decremento en el ingreso: x es un bien inferior
# e) Incremento en el precio de un bien sustituto de x
# f) Incremento en el precio de un bien complementario de x
# g) Decremento en el precio de un bien sustituto de x
# h) Decremento en el precio de un bien complementario de x
# i) Incremento en el costo de producción de x
# j) Decremento en el costo de producción de x

Precio <- c(10, 20, 30, 40, 50, 60)
Cantidad_original <- c(100, 90, 80, 70, 60, 50)
Demanda <- data.frame(Precio, Cantidad_original)

ggplot() + 
  geom_line(aes(x=Cantidad_original, y=Precio), color="black", size=1.5) + 
  theme_classic()

}

# Gráficas incisos a) al d)
{
  
Incremento_ingreso_Bien_normal <- Cantidad_original+10
Incremento_ingreso_Bien_inferior <- Cantidad_original-10
Decremento_ingreso_Bien_normal <- Incremento_ingreso_Bien_inferior
Decremento_ingreso_Bien_inferior <- Incremento_ingreso_Bien_normal

Demanda <- data.frame(Precio, Cantidad_original, Incremento_ingreso_Bien_normal,
                      Incremento_ingreso_Bien_inferior, Decremento_ingreso_Bien_normal,
                      Decremento_ingreso_Bien_inferior)
    
Demanda_1 <- Demanda %>% gather(key="Ocurre un...",value = "Cantidad_original",-Precio)


ggplot(Demanda_1, aes(x = Cantidad_original, y = Precio, group = `Ocurre un...`)) +
  geom_line(aes(color = `Ocurre un...`, linetype = `Ocurre un...`), size = 1.5) +
  geom_segment(x = 60, y = 50,
               xend = 56, yend = 46,
               color = "steelblue",
               arrow = arrow(), size=1.5)+
  geom_segment(x = 80, y = 30,
               xend = 84, yend = 34,
               color = "steelblue",
               arrow = arrow(), size=1.5)+
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

kable(Demanda) %>% 
  kable_styling("striped", full_width = F) %>% 
  row_spec(1:6, bold=F, background = "turquoise") %>% 
  column_spec(1, bold=T, background = "white") 
  

}

# Gráficas incisos e) al h)
{
  
Incremento_precio_bien_sustituto <- Cantidad_original+10
Incremento_precio_bien_complementario <- Cantidad_original-10
Decremento_precio_bien_sustituto <- Incremento_precio_bien_complementario
Decremento_precio_bien_complementario <- Incremento_precio_bien_sustituto

Demanda <- data.frame(Precio, Cantidad_original, 
                      Incremento_precio_bien_sustituto,
                      Incremento_precio_bien_complementario, 
                      Decremento_precio_bien_sustituto,
                      Decremento_precio_bien_complementario)

Demanda_2 <- Demanda %>% gather(key="Ocurre un...",value = "Cantidad_original",-Precio)


ggplot(Demanda_2, aes(x = Cantidad_original, y = Precio, group = `Ocurre un...`)) +
  geom_line(aes(color = `Ocurre un...`, linetype = `Ocurre un...`), size = 1.5) +
  geom_segment(x = 60, y = 50,
               xend = 56, yend = 46,
               color = "red",
               arrow = arrow(), size=1.5)+
  geom_segment(x = 80, y = 30,
               xend = 84, yend = 34,
               color = "red",
               arrow = arrow(), size=1.5)+
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
  scale_color_manual(values = c("grey59", "darkorchid", "green", "cornflowerblue", "#FF69B4")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dashed", "dashed"))


kable(Demanda) %>% 
  kable_styling("striped", full_width = F) %>% 
  row_spec(1:6, bold=F, background = "coral") %>% 
  column_spec(1, bold=T, background = "white")  

  
}

# Gráficas incisos i) al j)
{

Cantidad_original <- c(50,60,70,80,90,100) 
    
Incremento_costo_de_produccion <- Cantidad_original-10
Decremento_costo_de_produccion <- Cantidad_original+10  
 
Demanda <- data.frame(Precio, Cantidad_original, 
                      Incremento_costo_de_produccion,
                      Decremento_costo_de_produccion)

Demanda_3 <- Demanda %>% gather(key="Ocurre un...",value = "Cantidad_original",-Precio)

ggplot(Demanda_3, aes(x = Cantidad_original, y = Precio, group = `Ocurre un...`)) +
  geom_line(aes(color = `Ocurre un...`, linetype = `Ocurre un...`), size = 1.5) +
  geom_segment(x = 88, y = 50,
               xend = 83, yend = 50,
               color = "magenta",
               arrow = arrow(), size=1.5)+
  geom_segment(x = 63, y = 20,
               xend = 68, yend = 20,
               color = "magenta",
               arrow = arrow(), size=1.5)+
  theme(axis.text.x = element_text(angle = 90)) +
  theme_classic() +
  labs(title = "Movimientos en la oferta",
       subtitle = "Variación por unidad",
       x = "Cantidad", y = "Precio",
       caption = "Fuente: Elaboración propia con datos propios") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.caption = element_text(hjust = 0, face = "bold", size = 10),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_manual(values = c("blue","gold", "hotpink")) +
  scale_linetype_manual(values = c("solid", "solid", "solid"))

kable(Demanda) %>% 
  kable_styling("striped", full_width = F) %>% 
  row_spec(1:6, bold=F, background = "steelblue") %>% 
  column_spec(1, bold=T, background = "white") 
 
}


#Actividad 3
{
# 3. Considere los bienes siguientes:
# a.	Guante derecho
# b.	Moscas por día
# c.	Refresco Coca-Cola
# d.	Refresco Pepsi-Cola
# e.	Betabel
# f.	Alimentos por día
# g.	Guante izquierdo
# h.	Zanahorias
# con la información anterior, forme distintas cestas de consumo (de dos bienes) que puedan representar gráficamente las preferencias de:
# I.	complementarios perfectos
# II.	sustitutos perfectos
# III.	un mal
# IV.	un neutral


x <- c(1:10)
y <- c(1:10)
df <- data.frame(x, y)
}

#Bienes complementarios
{
p1 <-ggplot(df) + 
  geom_line(aes(x = x, y = y), color="white") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  geom_segment(x = 3, y = 9,
               xend = 3, yend = 3,
               color = "black", size=1)+
  geom_segment(x = 3, y = 3,
               xend = 9, yend = 3,
               color = "black", size=1) +  # Cambia los nombres de los ejes
  theme_classic()+
    labs(title = "Bienes complementarios",
         subtitle = "Guante derecho y guante izquierdo",
         x = "Guante derecho", y = "Guante izquierdo",
         caption = "Fuente: Elaboración propia con datos propios") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          plot.caption = element_text(hjust = 0, face = "bold", size = 10),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

}



#Combinar gráficas
{
p1 + p1  
plot_annotation(title = 'Título para todos los gráficos',
                subtitle = "Subtítulo",
                caption = "Pie del gráfico")
plot_layout(ncol = 2) 

}
