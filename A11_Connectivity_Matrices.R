#---------------------------------------------------------------------------
## II Curso Internacional "Uso de la teoría de grafos en la evaluación de la
## conectividad larval de poblaciones marinas"
## contacto: Andres Ospina-Alvarez (aospina.co@me.com)
## Organizadores(as) del curso: Catherine González (catherine.gonzalez@ifop.cl)
#---------------------------------------------------------------------------

# Título: Generando "heat maps" como representaciones de matrices de conectividad potencial y realizada.

# Vamos a cargar una matriz de conectividad realizada previamente guardada en una hoja de MS Excel, sin embargo podeis usar csv o txt, u otros formatos para almacenar vuestras matrices.

##### Importando una matriz ####
library(readxl) # Esta librería nos permite leer e importar hojas de MS Excel
cmat.rea <- as.matrix(read_excel("A11_example_matrix.xlsx", 
sheet = "realizada", col_types = c("skip", 
"numeric", "numeric", "numeric", 
"numeric", "numeric", "skip"), n_max = 5))

##### Convirtiendo la matriz realizada en una matriz potencial ####
cmat.pot <- cmat.rea / t(matrix(rep(colSums(cmat.rea),5),nrow = 5, ncol = 5))
# Ahora tenemos una matriz de probabilidades
cmat.pot
# donde la suma de cada una de las columnas es 1
colSums(cmat.pot)

# Podemos visualizar la matriz rapidamente con la función image
image(cmat.pot)

# También podemos cambiar a una paleta de colores que sea mas de nuestro agrado.
# La librería RColorBrewer tiene una serie de paletas secuenciales muy utiles
library(RColorBrewer)
display.brewer.all(type="seq")

# La librería fields nos proporciona herramientas para usar nuestra escala de colores y personalizar un poco mas la figura
library(fields)

# Usaremos la función image.plot de la librería fields y una escala de amarillo a verde "YlGn"
image.plot(t(cmat.pot), xlab = "Origen", 
      col = hcl.colors(10, "YlGn", rev = TRUE),
      ylab = "Destination",
      legend.lab="Probability")

# Está bien, pero tenemos un problema con las etiquetas de los ejes. Solucionemos esto...
image.plot(t(cmat.pot), xlab = "Origen", 
           col = hcl.colors(10, "YlGn", rev = TRUE),
           ylab = "Destination",
           legend.lab="Probability",
           axes = FALSE,
           frame = TRUE)
axis(1,seq(0,1,length.out = 5),1:5)
axis(2,seq(0,1,length.out = 5),1:5)

# Y ahora hagamos una figura de la matriz realizada, pero cambiemos las etiquetas a un codigo alfabético
image.plot(t(cmat.rea), xlab = "Origen", 
           col = hcl.colors(10, "YlGn", rev = TRUE),
           ylab = "Destination",
           legend.lab="Individuals",
           axes = FALSE,
           frame = TRUE)
axis(1,seq(0,1,length.out = 5),LETTERS[1:5])
axis(2,seq(0,1,length.out = 5),LETTERS[1:5])

# Es fácil y han quedado bastante bien las figuras de nuestras matrices, pero tenemos una librería que permite mucho mas control de los elementos de las figuras. Esta es ggplot2.
# También usaremos algunas otras librerías para asegurarnos un resultado optimo.

library(ggplot2)
library(reshape2)
library(hrbrthemes)
library(ggExtra)

# Aquí creamos el objeto gráfico básico, es decir, especificamos los atos a usar y damos un par de instrucciones para la leyenda y los títulos de los ejes.
pm <- ggplot(melt(t(cmat.pot)), aes(Var1,Var2, fill=value)) +
  # scale_x_reverse() +
  # scale_y_reverse() +
  labs(y = "Settlement latitude") +
  labs(x = "Release latitude") +
  labs(fill="Probability") +
  geom_raster() +
  theme_ipsum()

# Podemos ver el resultado inicial, es decir el objeto pm, el cual está bien, pero puede ser mejorado.

# Adicionemos al objeto pm unas cuantas instrucciones mas y finalicemos nuestra figura.
pm + scale_fill_distiller(palette = "YlGn",
                          direction = 1,
                          na.value = "transparent",
                          breaks=seq(1,0, length.out = 3),
                          limits=c(0,1)) +
  scale_x_continuous(breaks= c(1:5), labels = as.character(c(1:5))) +
  scale_y_continuous(breaks= c(1:5), labels = as.character(c(1:5))) +
  removeGrid(x = TRUE, y = TRUE) +
  xlab("Source area") +
  ylab("Destination area") +
  labs(fill="Probability") +
  theme(axis.title.x = element_text(face="bold", size=rel(2)),
        axis.text.x  = element_text(size=rel(2))) +
  theme(axis.title.y = element_text(face="bold", size=rel(2)),
        axis.text.y  = element_text(size=rel(2))) +
  theme(legend.text = element_text(size=rel(1.5)),
        legend.title = element_text(size=rel(1.5), face="bold"),
        legend.position="right") +
  theme(strip.text = element_text(face="bold", size=rel(1.5)))

