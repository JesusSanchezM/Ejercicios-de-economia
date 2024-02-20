#-----------------------------
#------Macroeconomía---------
#-----------------------------

library(tidyverse)
library(kableExtra) #tablas estilizadas
library(patchwork) #Combinar graficos


#Actividad 1 
{
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
}

#Actividad 3
{
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
    labs(title = "Complementarios perfectos",
         subtitle = "",
         x = "Guante derecho", y = "Guante izquierdo",
         caption = "") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          plot.caption = element_text(hjust = 0, face = "bold", size = 10),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
}

#Bienes sustitutos
{
p2 <- ggplot(df) + 
  geom_line(aes(x = x, y = y), color="white") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  geom_segment(x = 0, y = 8,
               xend = 8, yend = 0,
               color = "black", size=1)+
  theme_classic()+
  labs(title = "Sustitutos perfectos",
       subtitle = "",
       x = "Refresco Coca Cola", y = "Refresco Pepsi",
       caption = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.caption = element_text(hjust = 0, face = "bold", size = 10),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  
}

#Bienes, un mal

{
p3 <- ggplot(df) + 
  geom_line(aes(x = x, y = y), color="white") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  geom_segment(x = 0, y = 3,
               xend = 7, yend = 8,
               color = "black", size=1)+
  theme_classic()+
  labs(title = "Un mal",
       subtitle = "",
       x = "Alimento por día", y = "Moscas por día",
       caption = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.caption = element_text(hjust = 0, face = "bold", size = 10),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

}

#Bienes neutrales
{
p4 <- ggplot(df) + 
  geom_line(aes(x = x, y = y), color="white") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  geom_segment(x = 5, y = 9,
               xend = 5, yend = 0,
               color = "black", size=1)+
  theme_classic()+
  labs(title = "Un neutral",
       subtitle = "",
       x = "Betabel", y = "Zanahoria",
       caption = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.caption = element_text(hjust = 0, face = "bold", size = 10),
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

}

#Combinar gráficas
{
(p1 + p2)
  plot_annotation(title = '',
                subtitle = "",
                caption = "Fuente: Elaboración propia con datos propios")
  plot_layout(ncol = 2) 
  
(p3 + p4)
  plot_annotation(title = '',
                  subtitle = "",
                  caption = "Fuente: Elaboración propia con datos propios")
  plot_layout(ncol = 2) 

}

#Restricción presupuestaria y curva de indiferencia
{
# Crea un dataframe con valores para x
df1 <- data.frame(x = c(0, 20), y= c(80, 0))
df <- data.frame(x = c(5:20))
df$y <- (400/df$x)

# Grafica la restricción presupuestaria

ggplot() +
  geom_line(data=df1, aes(x, y), size=1) +
  geom_line(data=df, aes(x, y), size=1) +
  geom_point(aes(x = 20, y = 0), color = "red", size = 5) +  # Punto para 20 manzanas
  geom_point(aes(x = 0, y = 80), color = "blue", size = 5) +  # Punto para 80 naranjas
  geom_point(aes(x = 10, y = 40), color = "green", size = 5) +  # Punto después de comprar 10 manzanas
  geom_point(aes(x = 9, y = 44), color = "purple", size = 5) + 
  geom_point(aes(x = 5, y = 80), color = "blue", size = 5) +  # Punto para 80 naranjas
  geom_point(aes(x = 10, y = 40), color = "green", size = 5) +  # Punto después de comprar 10 manzanas
  geom_point(aes(x = 20, y = 20), color = "purple", size = 5) + 
  geom_hline(yintercept = 0, linetype = "dashed") +  # Línea horizontal para y=0
  geom_vline(xintercept = 0, linetype = "dashed") +  # Línea vertical para x=0
  labs(title = "Restricción Presupuestaria y curva de indiferencia",
       x = "Manzanas (x)",
       y = "Naranjas (y)") +
  theme_classic()
}
}
  
#Actividad 4 
{
#Curvas de indiferencia preferencias: 
#Regulares 
{
# Crea un dataframe con valores para x
df <- data.frame(x = c(1:10))
df$y <- (10/df$x)
df1 <- data.frame(x = c(1:20))
df1$y <- (20/df1$x)

# Grafica la restricción presupuestaria
p1 <- ggplot() +
  geom_line(data=df, aes(x, y), size=1) +
  geom_line(data=df1, aes(x, y), size=1) +
  geom_point(aes(x = 1, y = 10), color = "red", size = 5) + 
  geom_point(aes(x = 2, y = 5), color = "blue", size = 5) +  
  geom_point(aes(x = 4, y = 2.5), color = "green", size = 5) + 
  geom_point(aes(x = 5, y = 2), color = "purple", size = 5) +
  #2ndos puntos 
  geom_point(aes(x = 1, y = 20), color = "red", size = 5) +  
  geom_point(aes(x = 2, y = 10), color = "blue", size = 5) +  
  geom_point(aes(x = 4, y = 5), color = "green", size = 5) + 
  geom_point(aes(x = 5, y = 4), color = "purple", size = 5) +   
  geom_hline(yintercept = 0, linetype = "dashed") +  # Línea horizontal para y=0
  geom_vline(xintercept = 0, linetype = "dashed") +  # Línea vertical para x=0
  labs(title = "Curva de indiferencia preferencias regulares (1)",
       subtitle= expression("Función de utilidad =" ~ u == x[1] * x[2]),
       x = expression(x[1]),
       y = expression(x[2])) +
  annotate("text", x = 10, y = 0.5, label = expression(10 == x[1] * x[2] ), color = "black", size = 5) +
  annotate("text", x = 18, y = 0.5, label = expression(20 == x[1] * x[2] ), color = "black", size = 5) +
  theme_classic()
}

#Regulares
{
  # Crea un dataframe con valores para x
  df$a <- (sqrt(10/df$x))
  df1$a <- (sqrt(20/df1$x))
  
  # Grafica la restricción presupuestaria
  
p2 <-  ggplot() +
    geom_line(data=df, aes(x, a), size=1) +
    geom_line(data=df1, aes(x, a), size=1) +
    geom_point(aes(x = 1, y = 3.16), color = "red", size = 5) + 
    geom_point(aes(x = 2, y = 2.23), color = "blue", size = 5) +  
    geom_point(aes(x = 4, y = 1.58), color = "green", size = 5) + 
    geom_point(aes(x = 5, y = 1.41), color = "purple", size = 5) +
    #2ndos puntos 
    geom_point(aes(x = 1, y = 4.47), color = "red", size = 5) +  
    geom_point(aes(x = 2, y = 3.16), color = "blue", size = 5) +  
    geom_point(aes(x = 4, y = 2.23), color = "green", size = 5) + 
    geom_point(aes(x = 5, y = 2), color = "purple", size = 5) +   
    geom_hline(yintercept = 0, linetype = "dashed") +  # Línea horizontal para y=0
    geom_vline(xintercept = 0, linetype = "dashed") +  # Línea vertical para x=0
    labs(title = "Curva de indiferencia preferencias regulares (2)",
         subtitle= expression("Función de utilidad =" ~ u == x[1] * x[2]^2),
         x = expression(x[1]),
         y = expression(x[2])) +
    annotate("text", x = 10, y = 0.7, label = expression(10 == x[1] * x[2]^2 ), color = "black", size = 5) +
    annotate("text", x = 18, y = 0.7, label = expression(20 == x[1] * x[2]^2 ), color = "black", size = 5) +
    theme_classic()
}

#Sustitutos perfectos
{
  # Crea un dataframe con valores para x
  df$b <- (10-(2*df$x))
  df1$b <- (20-(2*df1$x))
  
  # Grafica la restricción presupuestaria
  

p3 <- ggplot() +
    geom_line(data=filter(df, b>=0), aes(x, b), size=1) +
    geom_line(data=filter(df1, b>=0), aes(x, b), size=1) +
    geom_point(aes(x = 1, y = 8), color = "red", size = 5) + 
    geom_point(aes(x = 2, y = 6), color = "blue", size = 5) +  
    geom_point(aes(x = 4, y = 2), color = "green", size = 5) + 
    geom_point(aes(x = 5, y = 0), color = "purple", size = 5) +
    #2ndos puntos 
    geom_point(aes(x = 1, y = 18), color = "red", size = 5) +  
    geom_point(aes(x = 2, y = 16), color = "blue", size = 5) +  
    geom_point(aes(x = 4, y = 12), color = "green", size = 5) + 
    geom_point(aes(x = 5, y = 10), color = "purple", size = 5) +   
    geom_hline(yintercept = 0, linetype = "dashed") +  # Línea horizontal para y=0
    geom_vline(xintercept = 0, linetype = "dashed") +  # Línea vertical para x=0
    labs(title = "Curva de indiferencia preferencias por sustitutos perfectos (3)",
         subtitle= expression("Función de utilidad =" ~ u == 2*x[1] + x[2]),
         x = expression(x[1]),
         y = expression(x[2])) +
    annotate("text", x = 5.5, y = 1.6, label = expression(10 == 2*x[1] + x[2] ), color = "black", size = 3.5) +
    annotate("text", x = 9.3, y = 4, label = expression(20 == 2*x[1] + x[2] ), color = "black", size = 3.5) +
    theme_classic()
}

p1 + p2 / p3
}





