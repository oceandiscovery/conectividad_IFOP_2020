#---------------------------------------------------------------------------
## II Curso Internacional "Uso de la teoría de grafos en la evaluación de la
## conectividad larval de poblaciones marinas"
## contacto: Andres Ospina-Alvarez (aospina.co@me.com)
## Organizadores(as) del curso: Catherine González (catherine.gonzalez@ifop.cl)
#---------------------------------------------------------------------------

# Título: Caso de estudio de una especie con PLD 20 y reclutamiento en invierno austral

rm(list = ls()) # Limpiar todos los objetos y variables en el entorno de trabajo

library(igraph)
library(networktools)
library(ggplot2)
library(plyr)
library(dplyr)
library(ggraph)
library(RColorBrewer)
library(scales)
library(ggpubr)
library(rockchalk) # for combineLevels

##### CARGA DE DATOS ####
# Importaremos un set de datos correspondientes a un ejercicio de simulación donde se acopló un modelo biológico basado en características individuales a un modelo hidrodinámico para la costa de Chile. El PLD de la especie es de 20 días que presenta comportamiento de migración vertical, con un periodo de competencia de 8 días.
data_20_PLD_DVM <- readRDS("A71_data_20_PLD_DVM.rds")

# Veamos un resumen de los datos
summary(data_20_PLD_DVM)

# Establezcamos como factores algunas variables
data_20_PLD_DVM$season_ini <- as.factor(data_20_PLD_DVM$season_ini)
data_20_PLD_DVM$season_end <- as.factor(data_20_PLD_DVM$season_end)

# Un nuevo resumen
summary(data_20_PLD_DVM)

# Si queremos podemos filtrar la base de datos, por ejemplo concentremonos en las larvas (partículas) que se asientan o reclutan en el invierno austral
data_ready <- filter(data_20_PLD_DVM, season_end == "Q3 JUN-AUG")

##### CREANDO LAS MATRICES ####
# Ahora extraemos la matriz de conectividad larval realizada.

# Estas son nuestras zonas de estudio, fueron creadas durante la configuración del modelo transporte y dispersión larval.
size.matrix.zones <- max(c(max(data_ready$zone_ini), max(data_ready$zone_end)))

# Creamos una matriz cuadrada llena de ceros, el tamaño dependera del número máximo de zonas que tengamos, en este caso son 405, entonces tenemos una matriz de 405 * 405
conmatrix = matrix(rep(0, size.matrix.zones * size.matrix.zones),
                   # the data elements
                   nrow = size.matrix.zones,
                   # number of rows
                   ncol = size.matrix.zones,
                   # number of columns
                   byrow = TRUE)

# Con este bucle llenamos la matriz con la información
for (ii in 1:nrow(data_ready)) {
  q <- as.numeric(data_ready$zone_ini[ii])
  w <- as.numeric(data_ready$zone_end[ii])
  conmatrix[q, w] <- conmatrix[w, q] + data_ready$super_particle_size[ii]
}

# super_particle_size determina si la partícula representa una larval individual o una cohorte de larvas con un tamaño determinado y conocido. 

# La siguiente línea es con propósitos de backup
cmat.rea <- conmatrix

# Convirtiendo la matriz realizada en una matriz potencial
cmat.pot <- cmat.rea / t(matrix(rep(colSums(cmat.rea),size.matrix.zones),nrow = size.matrix.zones, ncol = size.matrix.zones))

# Comprobemos que todas las columnas sumen 1. Es como teóricamente debería ser pero en la práctica podemos encontrar algunos casos especiales.
colSums(cmat.pot)

#### Representaciones tipo "mapa de calor" de las matrices de conectividad con zonas ####
library(ggplot2)
library(reshape2)
library(hrbrthemes)
library(ggExtra)

# Matriz potencial
ggplot(melt(t(cmat.pot)), aes(Var1,Var2, fill=value)) +
  theme_ipsum() + 
  geom_raster() +
  scale_fill_distiller(palette = "Spectral",
                          direction = -1,
                          na.value = "transparent",
                          breaks=seq(1,0, length.out = 5),
                          limits=c(1E-10,1)
                       ) +
  removeGrid(x = TRUE, y = TRUE) +
  xlab("Source cell") +
  ylab("Destination cell") +
  labs(fill="Probability") +
  theme(axis.title.x = element_text(face="bold", size=rel(2)),
        axis.text.x  = element_text(size=rel(2))) +
  theme(axis.title.y = element_text(face="bold", size=rel(2)),
        axis.text.y  = element_text(size=rel(2))) +
  theme(legend.text = element_text(size=rel(1.5)),
        legend.title = element_text(size=rel(1.5), face="bold"),
        legend.position="right") +
  theme(strip.text = element_text(face="bold", size=rel(1.5)))

# Matriz realizada
ggplot(melt(t(cmat.rea)), aes(Var1,Var2, fill=value)) +
  theme_ipsum() + 
  geom_raster() +
  scale_fill_distiller(palette = "Spectral",
                       direction = -1,
                       na.value = "transparent",
                       limits=c(1E-10,250)
                       ) +
  removeGrid(x = TRUE, y = TRUE) +
  xlab("Source cell") +
  ylab("Destination cell") +
  labs(fill="Individuals") +
  theme(axis.title.x = element_text(face="bold", size=rel(2)),
        axis.text.x  = element_text(size=rel(2))) +
  theme(axis.title.y = element_text(face="bold", size=rel(2)),
        axis.text.y  = element_text(size=rel(2))) +
  theme(legend.text = element_text(size=rel(1.5)),
        legend.title = element_text(size=rel(1.5), face="bold"),
        legend.position="right") +
  theme(strip.text = element_text(face="bold", size=rel(1.5)))

##### Kernel de dispersion ####

# ¿ Puedes intentar calcularlo?

##### Matrices latitudinales ####
# Este tipo de matrices solo son útiles para costas que discurren casi en línea recta siguiendo el eje S - N, como la costa chilena.

# Las latitudes del dataframe están en formato decimal y con valores negativos cuando hacen referencia a una posición al sur de la línea del ecuador. Vamos a quitar ese signo.
latitude_ini_abs <- abs(data_ready$latitude_ini)
latitude_end_abs <- abs(data_ready$latitude_end)

# Ahora vamos a categorizar la línea de costa por franjas latitudinales de un grosor de una decima de grado.
lat_seq = seq(from = 31.5, to = 40, by = 0.1)
# Asignamos estas categorías a cada posición
data_ready$lat_ini_lev = findInterval(latitude_ini_abs, lat_seq)
data_ready$lat_end_lev = findInterval(latitude_end_abs, lat_seq)

# Y ahora sumamos el número de partículas en cada franja latitudinal
realized.connectivity.df <- ddply(data_ready, c("lat_ini_lev", "lat_end_lev"), summarise,
                                weigth = sum(super_particle_size, na.rm=TRUE))

# Aquí verificamos el tamaño de nuestra nueva matriz
size.matrix.latitudes <- max(c(max(realized.connectivity.df$lat_ini_lev), max(realized.connectivity.df$lat_end_lev)))

# Y creamos una nueva matriz de ese tamaño, llena de ceros, que será la matriz de conectividad realizada 
realized.connectivity.matrix <-
  matrix(rep(0, size.matrix.latitudes * size.matrix.latitudes),
                   # the data elements
                   nrow = size.matrix.latitudes,
                   # number of rows
                   ncol = size.matrix.latitudes,
                   # number of columns
                   byrow = TRUE)


# Como en el bloque anterior llenamos la matriz usando un bucle
for (ii in 1:nrow(realized.connectivity.df)) {
  q <- as.numeric(realized.connectivity.df$lat_ini_lev[ii])
  w <- as.numeric(realized.connectivity.df$lat_end_lev[ii])
  realized.connectivity.matrix[q, w] <- realized.connectivity.matrix[w, q] + realized.connectivity.df$weigth[ii]
}

# Convertimos la matriz realizada en una matriz potencial y verificamos la suma de las columnas
potential.connectivity.matrix <- realized.connectivity.matrix / t(matrix(rep(colSums(realized.connectivity.matrix),size.matrix.latitudes),nrow = size.matrix.latitudes, ncol = size.matrix.latitudes))
colSums(potential.connectivity.matrix)

# Listo, ahora tenemos unas matrices de conectividad latitudinal

#### Representaciones tipo "mapa de calor" de las matrices de conectividad latitudinales ####

# Vamos a crear una serie de etiquetas que nos sirvan para los ejes de nuestro gráfico, de esta manera no quedará saturado de información, pondremos una etiqueta cada 0.4 grados.
breaks_lat_seq = seq(from = 31.5, to = 40, by = 0.4)

# Aquí convertimos la secuencia anterior en formato texto
labset <- as.character(round(breaks_lat_seq, digits = 2))
# labset <- labset[1:length(breaks_lat_seq)-1] # Pasad esto por alto.
# Y aquí establecemos los puntos de corte para la leyenda.
breaks_gplot <- seq(1,length(lat_seq),by=4)

# Matriz de conectividad potencial
ggplot(melt(t(potential.connectivity.matrix)), aes(Var1,Var2, fill=value)) +
  geom_raster() +
  theme_ipsum() +
  scale_x_continuous(breaks= breaks_gplot, labels = labset, trans = "reverse") +
  scale_y_continuous(breaks= breaks_gplot, labels = labset, trans = "reverse") +
  scale_fill_distiller(palette = "Spectral",
                       direction = -1,
                       na.value = "transparent") +
  removeGrid(x = TRUE, y = TRUE) +
  xlab("Source area (ºS)") +
  ylab("Destination area (ºS)") +
  labs(fill="Probability") +
  theme(axis.title.x = element_text(face="bold", size=rel(2)),
        axis.text.x  = element_text(size=rel(2))) +
  theme(axis.title.y = element_text(face="bold", size=rel(2)),
        axis.text.y  = element_text(size=rel(2))) +
  theme(legend.text = element_text(size=rel(1.5)),
        legend.title = element_text(size=rel(1.5), face="bold"),
        legend.position="right") +
  theme(strip.text = element_text(face="bold", size=rel(1.5)))

# Matriz de conectividad realizada
ggplot(melt(t(realized.connectivity.matrix)), aes(Var1,Var2, fill=value)) +
  geom_raster() +
  theme_ipsum() +
  scale_x_continuous(breaks= breaks_gplot, labels = labset, trans = "reverse") +
  scale_y_continuous(breaks= breaks_gplot, labels = labset, trans = "reverse") +
  scale_fill_distiller(palette = "Spectral",
  direction = -1,
  na.value = "transparent") +
  removeGrid(x = TRUE, y = TRUE) +
  xlab("Source area (ºS)") +
  ylab("Destination area (ºS)") +
  labs(fill="Individuals") +
  theme(axis.title.x = element_text(face="bold", size=rel(2)),
        axis.text.x  = element_text(size=rel(2))) +
  theme(axis.title.y = element_text(face="bold", size=rel(2)),
        axis.text.y  = element_text(size=rel(2))) +
  theme(legend.text = element_text(size=rel(1.5)),
        legend.title = element_text(size=rel(1.5), face="bold"),
        legend.position="right") +
  theme(strip.text = element_text(face="bold", size=rel(1.5)))


# Matrices de conectividad realizada del tipo latitudinal, un método alternativo usando el objeto tipo dataframe y no el objeto tipo matrix
p <- ggplot(realized.connectivity.df, aes(x=lat_ini_lev, y=lat_end_lev, fill=weigth))
p + geom_raster() + theme_ipsum() +
  scale_x_continuous(breaks= breaks_gplot, labels = labset, trans = "reverse") +
  scale_y_continuous(breaks= breaks_gplot, labels = labset, trans = "reverse") +
  scale_fill_distiller(palette = "Spectral",
                       direction = -1,
                       na.value = "transparent") +
  xlab("Source area (ºS)") +
  ylab("Destination area (ºS)") +
  labs(fill="Individuals") +
  theme(axis.title.x = element_text(face="bold", size=rel(1.5)),
        axis.text.x  = element_text(size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", size=rel(1.5)),
        axis.text.y  = element_text(size=rel(1.5))) +
  theme(legend.text = element_text(size=rel(1)),
        legend.title = element_text(size=rel(1.5), face="bold"),
        legend.position="right") +
  theme(strip.text = element_text(face="bold", size=rel(1.5)))


