
#Crear tablas visualmente atractivas
{
# install.packages("devtools")
#devtools::install_github("renkun-ken/formattable")

library(formattable)

df <- data.frame(
  id = 1:10,
  name = c("Bob", "Ashley", "James", "David", "Jenny",
           "Hans", "Leo", "John", "Emily", "Lee"),
  age = c(28, 27, 30, 28, 29, 29, 27, 27, 31, 30),
  grade = c("C", "A", "A", "C", "B", "B", "B", "A", "C", "C"),
  test1_score = c(8.9, 9.5, 9.6, 8.9, 9.1, 9.3, 9.3, 9.9, 8.5, 8.6),
  test2_score = c(9.1, 9.1, 9.2, 9.1, 8.9, 8.5, 9.2, 9.3, 9.1, 8.8),
  final_score = c(9, 9.3, 9.4, 9, 9, 8.9, 9.25, 9.6, 8.8, 8.7),
  registered = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE)

formattable(df, list(
  age = color_tile("white", "orange"),
  grade = formatter("span", style = x ~ ifelse(x == "A",
                                               style(color = "green", font.weight = "bold"), NA)),
  area(col = c(test1_score, test2_score)) ~ normalize_bar("pink", 0.2),
  final_score = formatter("span",
                          style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
                          x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),
  registered = formatter("span",
                         style = x ~ style(color = ifelse(x, "green", "red")),
                         x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
))

#---------------------------------------------

library(DT)  
datatable(Demanda)

#---------------------------------------------

library(kableExtra)

kable(Demanda) %>% 
  kable_styling("striped", full_width = F) %>% 
  column_spec(1, bold=T) %>% 
  row_spec(1:6, bold=T, color="black", background = "turquoise")

}


#Graficas y modificacion de datos
{
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
  
  
df <- data.frame(x = rnorm(100),
                 y = rnorm(100))

ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_segment(x = -3, y = 3,
               xend = 1, yend = -1,
               color = 2,
               arrow = arrow(), size=1.5)


x <- c()
y <- c(1:10)
df <- data.frame(x, y)

p1 <- ggplot(df) + 
  geom_line(aes(x = x, y = y), color="white") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  geom_segment(x = 3, y = 9,
               xend = 3, yend = 3,
               color = "black", size=1)+
  geom_segment(x = 3, y = 3,
               xend = 9, yend = 3,
               color = "black", size=1) +
  labs(x = "Guante derecho", y = "Guante izquierdo") +  # Cambia los nombres de los ejes
  theme_classic()


library(patchwork)

# Combinamos los gráficos
p1 + p2
  plot_annotation(title = 'Título para todos los gráficos',
                                          subtitle = "Subtítulo",
                                          caption = "Pie del gráfico")
  plot_layout(ncol = 2) 

  

  y <- data.frame(x1)
  
  for (i in x1) {
    res <- i * 2
    col_name <- paste("utilidad_", i, sep = "")
    y <- cbind(y, res)
    colnames(y)[ncol(y)] <- col_name
  }
  
  print(y)
  
  
  
  # Assuming x1 and x2 are vectors or columns in a data frame
  utilidad <- data.frame(x1_u = x1 * x2)
  r <- x1*x2
  
  for (i in r) {
    a <- i / x1
    col_name <- paste("utilidad_", i, sep = "")
    utilidad <- cbind(utilidad, a)
    colnames(utilidad)[ncol(utilidad)] <- col_name
  }
  utilidad
  str(utilidad)
  # Load the ggplot2 library if not already loaded
  # library(ggplot2)
  
  # Plot the data using ggplot2
  ggplot(data = uti) + 
    geom_line(aes(y = utilidad_4.5)) +  # Adjust the column name accordingly
    theme_classic()

  uti <- utilidad[1:4]
  
plot(utilidad$x1_u, utilidad$utilidad_4.5, type="l")


#------------------------------------------------


library(ggplot2)

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
ggplot(data, aes(x, y)) +
  geom_line(color = "blue", size = 2) +
  labs(
    title = "Exponential Function",
    x = "X-axis",
    y = "Y-axis"
  ) +
  theme_minimal() + theme_classic()



