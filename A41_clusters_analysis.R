#---------------------------------------------------------------------------
## II Curso Internacional "Uso de la teoría de grafos en la evaluación de la
## conectividad larval de poblaciones marinas"
## contacto: Andres Ospina-Alvarez (aospina.co@me.com)
## Organizadores(as) del curso: Catherine González (catherine.gonzalez@ifop.cl)
#---------------------------------------------------------------------------

## Título: Particionando un grafo en conjuntos o comunidades no-biológicas.

rm(list = ls()) # Limpiar todos los objetos y variables en el entorno de trabajo

library(igraph) # Cargar la librería igraph
library(readxl)

##### Importar los datos generados previamente en script A31 ####
df1 <- read.csv("A41_g4.cent.measures.nodes.csv")
df2 <- df1 # Solo como propósito de backup


#### Geocoding caletas ####
# Vamos a importar los datos de longitud y latitud de las caletas, los cuales tenemos guardados en la hoja de calculo A31_caletas.xlsx
caletasgeocodes <- read_excel("A31_caletas.xlsx", sheet = "Sheet2")

# Ahora vamos a asignar los datos de latitud y longitud automáticamente las caletas de nuestra base de datos df2, la cual carece de ellos.
df2$lat <- NaN
df2$lon <- NaN
# Usamos un bucle y la función which
for (i in 1:nrow(df2)){
  idx          <- which(caletasgeocodes$CALETA == df2$name[i])
  df2$lat[i]   <- caletasgeocodes$lat[idx]
  df2$lon[i]   <- caletasgeocodes$lon[idx]
}

#### ================= Técnica de partición ================= ####
# Realizamos la agrupación de los k-medias en una matriz de datos, inicialmente lo haremos solo con una sola de las medidas de centralidad, el betweenness.
# Debemos elegir el número de agrupaciones (sí, es muy subjetivo... ya iremos a eso)
model <- kmeans(data.frame(df2$lat, df2$betweenness),centers = 8)

# Visualizamos los grupos identificándolos mediante colores.
plot(df2$lat, df2$betweenness, col = model[[1]], xlab = "Latitude", ylab = "betweenness", pch = 16)

# Tip: Todos los posibles colores para una figura en R se pueden listar mediante la función colors
colors()

# ¿Será el betweenness una medida que separa los datos en un par de conjuntos diferenciados? ¿Habrá otra medida que nos pueda ser de utilidad?

# Veamos una matriz de gráficos de dispersión para algunas medidas de centralidad, he excluido las que tienen que ver con el numero de arcos incidentes en los nodos (i.e. degree, in-degree, out-degree, strength, in-strength y out-strength)
pairs(df2[,c(5:7,11:14)], pch = 16, col = "steelblue3")
# Parece ser que betweenness, closeness y hub score dividen los nodos en por lo menos dos conjuntos diferenciados.

# Hagamos enfasis en esas tres medidas...
pairs(df2[,c("betweenness", "closeness", "hub_score", "lat")], pch = 16, col = "sienna2")
# Parece que tenemos una presunción razonable, sigamos adelante...

# Construimos un modelo de partición con 3 grupos, veamos los resultados...
kmeans(df2[,c("betweenness", "closeness", "hub_score", "lat")], centers = 3)
# El 78,3% de nuestros nodos quedan "bien" clasificados, sigamos adelante, escogamos una de las variables, por ejemplo hub score.

model <- kmeans(data.frame(df2$lat, df2$hub_score),centers = 3)
plot(df2$lat, df2$hub_score, col = model[[1]], xlab = "Latitude", ylab = "hub score", pch = 16)

# Tema de discusión: ¿Nos puede llegar a ser útil la técnica de partición para buscar conjuntos en este tipo de bases de datos?

#### ================= Hierarchical cluster analysis ================= ####
g4.data <-
  read_excel("A31_caletas.xlsx") # Leer una matriz de adyacencia almacenada en un archivo MS Excel

# Para esta técnica de análisis necesitamos una matriz, por tanto vamos a obtener una.
M1 <- as.matrix(g4.data)
# Nuestra matriz tiene nombres para las columnas pero no para las filas ¿Verificamos?
colnames(M1)
rownames(M1)
# Como es una matriz cuadrada asignemos el nombre de las columnas también a las filas
rownames(M1) <- colnames(M1)

# Y ahora si, con una única función, podemos hacer el análisis de agrupación jerárquica.
mat.hc <- hclust(dist(M1))
plot(mat.hc, main="")

plot(hclust(dist(df2[,2:7])), labels = df2$name, main = "")

# Comentario adaptado de Crawley (2013), para que lo discutamos.
# Cuando conocemos algunas características de los nodos, los modelos de árboles son maravillosamente eficientes en la construcción de claves para distinguir entre nodos, y en la asignación de las categorías pertinentes. Cuando no conocemos características de los nodos, entonces la tarea estadística es mucho más severa, e inevitablemente termina siendo mucho más propensa a errores. Conclusión: La agrupación multivariante sin una variable de respuesta es fundamentalmente difícil y equívoca.


# Agrupación jerárquica eligiendo solo unas cuantas de las medidas, similar a lo que hicimos en la técnica de particionado
# Asignemos como nombres de las filas los nombres de las caletas
row.names(df2) <- df2$name
# Y ahora usaremos pipes y la librería tidyverse para filtrar y organizar nuestros datos
library(tidyverse)
dend <- df2 %>% 
  select(closeness, betweenness, hub_score, lat) %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram()

# Plot
par(mar=c(7,3,1,1))  # Increase bottom margin to have the complete label
plot(dend)
# He aquí el resultado gráfico, mejoremos visualmente este árbol un poco

# Usaremos la librería dendextend, que es especial para el trabajo con dendrogramas. 
library(dendextend)
# Color en función del grupo
par(mar=c(1,1,1,7))
dend %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  plot(horiz=TRUE, axes=FALSE)
abline(v = 3, lty = 2)

# Resaltar un grupo con un rectángulo
par(mar=c(9,1,1,1))
dend %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  plot(axes=FALSE)
rect.dendrogram(dend, k=3, lty = 5, lwd = 0, x=1, col=rgb(0.1, 0.2, 0.4, 0.1) ) 

# Y eso es todo, ahora podemos discutir sobre estas técnicas "clásicas" de asignación de elementos a grupos o conjuntos.