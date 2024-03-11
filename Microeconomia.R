#-----------------------------
#------Macroeconomía---------
#-----------------------------

library(tidyverse)
library(kableExtra) #tablas estilizadas
library(gridExtra) 
library(patchwork) #Combinar graficos


x <- c(1:10)
y <- c(21:30)
juntos <- data.frame(x, y)
View(juntos)
str(juntos)




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
  annotate("text", x = 10, y = 0.5, label = expression(x[2] == 10/x[1]), color = "black", size = 5) +
  annotate("text", x = 18, y = 0.5, label = expression(x[2] == 20/x[1]), color = "black", size = 5) +
  theme_classic()
p1
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
    annotate("text", x = 10, y = 0.5, label = expression(x[2] == sqrt(10/x[1]) ), color = "black", size = 4) +
    annotate("text", x = 18, y = 0.5, label = expression(x[2] == sqrt(20/x[1])  ), color = "black", size = 4) +
    theme_classic()
p2
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
    annotate("text", x = 5.5, y = 1.6, label = expression(x[2] == 10-2*x[1]), color = "black", size = 3.5) +
    annotate("text", x = 9.3, y = 4, label = expression(x[2] == 20-2*x[1]), color = "black", size = 3.5) +
    theme_classic()
p3
}

p1 + p2 / p3
}

#Actividad 5
{
  {
  #Esta actividad se centra en graficar la curva de oferta-renta y la curva de engel
  #funcion u=(x1)^2(x2)
  # restriccion presupuestaria p1x1+p2x2=m
  
  p1 <- 10
  p2 <- 20
  m1 <- 100
  m2 <- 80
  m3 <- 60

  x1 <- c(1:10)
  x2_m1 <- (m1-p1*x1)/p2
  x2_m2 <- (m2-p1*x1)/p2
  x2_m3 <- (m3-p1*x1)/p2
  
  utilidad <- data.frame(x1_u=x1)
  r <- x1*x2_m1
  for (i in r) {
    a <- i / (x1)
    col_name <- paste("utilidad_", i, sep = "")
    utilidad <- cbind(utilidad, a)
    colnames(utilidad)[ncol(utilidad)] <- col_name
  }
  
  utilidad <- utilidad[1:6] #elimino algunas columnas porque tienen nombres repetidos
  data_1 <- data.frame(x1, x2_m1, x2_m2, x2_m3)
  
  p1<-ggplot() + 
    geom_line(data=data_1, aes(x=x1, y=x2_m1), color="red", size=1) +
    geom_line(data=data_1, aes(x=x1, y=x2_m2), color="red", size=1)+
    geom_line(data=data_1, aes(x=x1, y=x2_m3), color="red", size=1)+
    geom_line(data=utilidad, aes(x=x1_u, y=utilidad_4.5), size=1)+
    geom_line(data=utilidad, aes(x=x1_u, y=utilidad_12.5), size=1)+
    geom_line(data=utilidad, aes(x=x1_u, y=utilidad_8), size=1)+
    geom_point(aes(x = 3, y = 1.5), color = "red", size = 5)+
    geom_point(aes(x = 4, y = 2), color = "blue", size = 5)+
    geom_point(aes(x = 5, y = 2.5), color = "green", size = 5)+
    geom_line(data = data.frame(x = c(3, 4, 5), y = c(1.5, 2, 2.5)),
              aes(x = x, y = y),
              color = "orange", size=2)+
    labs(
      title = "Curva de oferta-renta",
      subtitle = expression("Función de utilidad:" ~ u == x[1] * x[2]),
      x = "Bien 1",
      y = "Bien 2",
      caption = "Los precios se establecieron en p1= 10 y p2=20. 
      Los niveles de renta se establecieron en 100, 80 y 60. 
      Las cantidades del bien 1 y el bien 2 son (1,2,3,4,5,6,7,8,9,10)"
    ) +
    theme_classic()
    
  #Curva de Engel
  # Function for exponential curve
  exponential_function <- function(x) {
    return(1000 * exp(0.05 * x))
  }
  # Values for x from 10 to 100
  x_values <- seq(10, 100, by = 1)
  # Calculate corresponding y values using the exponential function
  y_values <- exponential_function(x_values)
  # Create a data frame
  data <- data.frame(x = x_values, y = y_values)
  # Plot the exponential curve
  p2 <- ggplot(data, aes(x, y)) +
    geom_line(color = "blue", size = 2) +
    labs(
      title = "Curva de Engel",
      subtitle = expression("Función:" ~ m==10000*e^(0.05*x[1])),
      x = "Bien 1",
      y = "Renta"
    ) + theme_classic()
  
  p1+p2
  }
  {#Esta segunda actividad representa la curva de oferta renta y la curva de engel para bienes sustitutos perfectos y complementarios perfectos
  
    p1 <- ggplot(df) + 
      geom_line(aes(x = x, y = x), color="white") +
      scale_x_continuous(breaks = seq(1, 10, 1)) +
      scale_y_continuous(breaks = seq(1, 10, 1)) +
      geom_segment(x = 0, y = 8,
                   xend = 8, yend = 0,
                   color = "black", size=1)+
      geom_segment(x = 0, y = 6,
                   xend = 6, yend = 0,
                   color = "black", size=1)+
      geom_segment(x = 0, y = 10,
                   xend = 10, yend = 0,
                   color = "black", size=1)+
      geom_segment(x = 0, y = 6,
                   xend = 8.2, yend = 0,
                   color = "blue", size=1)+
      geom_segment(x = 0, y = 4,
                   xend = 6.2, yend = 0,
                   color = "blue", size=1)+
      geom_segment(x = 0, y = 8,
                   xend = 10.2, yend = 0,
                   color = "blue", size=1)+
      geom_segment(x = 0, y = 0.5,
                   xend = 10, yend = 0.5,
                   color = "orange", size=3)+
      theme_classic()+
      labs(title = "Sustitutos perfectos",
           subtitle = "Curva de oferta-renta",
           x = "Bien 1", y = "Bien 2",
           caption = "") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
            plot.caption = element_text(hjust = 0, face = "bold", size = 10),
            plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
    
    p1
    
    p2 <- ggplot(df) + 
      geom_line(aes(x = x, y = y), color="white") +
      scale_x_continuous(breaks = seq(1, 10, 1)) +
      scale_y_continuous(breaks = seq(1, 10, 1)) +
      geom_segment(x = 4, y = 9,
                   xend = 4, yend = 4,
                   color = "black", size=1)+
      geom_segment(x = 4, y = 4,
                   xend = 9, yend = 4,
                   color = "black", size=1) +  # Cambia los nombres de los ejes
      geom_segment(x = 2, y = 9,
                   xend = 2, yend = 2,
                   color = "black", size=1)+
      geom_segment(x = 2, y = 2,
                   xend = 9, yend = 2,
                   color = "black", size=1)+
      geom_segment(x = 6, y = 9,
                   xend = 6, yend = 6,
                   color = "black", size=1)+
      geom_segment(x = 6, y = 6,
                   xend = 9, yend = 6,
                   color = "black", size=1)+
      geom_segment(x = 4, y = 0,
                   xend = 0, yend = 4,
                   color = "blue", size=1)+
      geom_segment(x = 8, y = 0,
                   xend = 0, yend = 8,
                   color = "blue", size=1)+
    geom_segment(x = 12, y = 0,
                 xend = 0, yend = 12,
                 color = "blue", size=1)+
      geom_segment(x = 2, y = 2,
                   xend = 6, yend = 6,
                   color = "orange", size=1)+
      theme_classic()+
      labs(title = "Complementarios perfectos",
           subtitle = "Curva de oferta-renta",
           x = "Bien 1", y = "Bien 2",
           caption = "") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
            plot.caption = element_text(hjust = 0, face = "bold", size = 10),
            plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
    
    p1+p2
    
    p3 <- ggplot(df) + 
      geom_line(aes(x = x, y = x), color="white") +
      scale_x_continuous(breaks = seq(1, 10, 1)) +
      scale_y_continuous(breaks = seq(1, 10, 1)) +
      geom_segment(x = 0, y = 0,
                   xend = 10, yend = 10,
                   color = "salmon", size=1)+
      theme_classic()+
      labs(title = "Sustitutos perfectos",
           subtitle = "Curva de Engel",
           x = "Bien 1", y = "Renta",
           caption = "") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
            plot.caption = element_text(hjust = 0, face = "bold", size = 10),
            plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    p4 <- ggplot(df) + 
      geom_line(aes(x = x, y = x), color="white") +
      scale_x_continuous(breaks = seq(1, 10, 1)) +
      scale_y_continuous(breaks = seq(1, 10, 1)) +
      geom_segment(x = 0, y = 0,
                   xend = 10, yend = 10,
                   color = "salmon", size=1)+
      theme_classic()+
      labs(title = "Complementarios perfectos",
           subtitle = "Curva de Engel",
           x = "Bien 1", y = "Renta",
           caption = "") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
            plot.caption = element_text(hjust = 0, face = "bold", size = 10),
            plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    (p1+p3)
    p2+p4
    
  }
  {
  #Curva de oferta precio
    df_1 <- data.frame(x = (1:10),
                     y = (1:10))
    
    p1<- ggplot(df_1, aes(x = x, y = y)) +
      scale_x_continuous(breaks = seq(1, 10, 1)) +
      scale_y_continuous(breaks = seq(1, 10, 1)) +
      geom_point(color="white") +
      geom_curve(x = 2, y = 8,
                 xend = 7, yend = 3,
                 color = "black", size=1) +
      geom_curve(x = 3, y = 9,
                 xend = 8, yend = 4,
                 color = "black", size=1) +
      geom_curve(x = 1, y = 7,
               xend = 6, yend = 2,
               color = "black", size=1) +
      geom_curve(x = 2, y = 3,
                 xend = 5.7, yend = 4,
                 color = "orange", size=2, curvature = 0.2)+
      geom_segment(x = 0, y = 7.7,
                   xend = 3.40, yend = 0,
                   color = "red", size=1)+
      geom_segment(x = 0, y = 7.2,
                   xend = 6.4, yend = 0,
                   color = "red", size=1)+
      geom_segment(x = 0, y = 7,
                   xend = 10.5, yend = 0,
                   color = "red", size=1)+
      geom_point(aes(x = 2.1, y = 2.95), color = "red", size = 5)+
      geom_point(aes(x = 4, y = 2.75), color = "blue", size = 5)+
      geom_point(aes(x = 5.25, y = 3.55), color = "green", size = 5)+
      labs(
        title = "Curva de oferta-precio",
        x = "Bien 1",
        y = "Bien 2",
        caption = ""
      ) +
      theme_classic()
    
    p1
    
    
  }
  
  
}

#Elasticidades
{
  p <- c(1:10)
  q <- c(140, 114,86,70,63,54,45,39,33,29)
  q1 <- seq(200, 20, -20)
  data_1 <- data.frame(p,q,q1)
  data_1 <- mutate(data_1, e_q1=c(NA, round((abs(diff(q1))*p[2:10])/(abs(diff(p))*q1[2:10]), 2)))
  data_1 <- mutate(data_1, e_1_q=c(NA, round((abs(diff(q))*p[2:10])/(abs(diff(p))*q[2:10]), 2)))
  data_1 <- mutate(data_1, Diff_q1=c(NA, diff(q1)),
                   Diff_q=c(NA, diff(q)))
  
  
  p1 <- ggplot(data=data_1) + 
    geom_line(data=data_1, aes(x=q, y=p), size=1, color="blue")+
    geom_line(data=data_1, aes(x=q1, y=p), size=1, color="red")+
    theme_classic()
  
  
  p2 <- ggplot(data=data_1) + 
    geom_line(aes(x=q1, y=e_q1), size=1, color="red")+
    geom_line(aes(x=q, y=e_1_q), size=1, color="blue")+
    geom_segment(x = 0, y = 1,
                 xend = 200, yend = 1,
                 color = "black", size=1) +
    theme_classic()

  p1+p2
  
  #DOS FORMAS DE INCLUIR GRAFICAS Y TABLAS 
  #FORMA 1
  {
  tbl1 <- tableGrob(data_1, theme=ttheme_gtbw(), rows=NULL)
  
  plot_layout <- 
    "AABB
     AACC"
  
  wrap_plots(tbl1, p1, p2, 
             design = plot_layout)
  
  }
  
  #FORMA 2
  {
  #install.packages("ggpmisc")                         # Install & load ggpmisc package
  library("ggpmisc")
  
  ggp_table <- ggplot() +                             # Create empty plot with table
    theme_void() +
    annotate(geom = "table",
             x = 1,
             y = 1,
             label = list(data_1))
  ggp_table
  
  ggp_table + p1/p2 
  
  }
  
  
}






