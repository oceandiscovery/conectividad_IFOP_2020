#---------------------------------------------------------------------------
## II Curso Internacional "Uso de la teoría de grafos en la evaluación de la
## conectividad larval de poblaciones marinas"
## contacto: Andres Ospina-Alvarez (aospina.co@me.com)
## Organizadores(as) del curso: Catherine González (catherine.gonzalez@ifop.cl)
#---------------------------------------------------------------------------

# Título: Calcular IIC - Indice Integral de Conectividad para cada parche en un paisaje.

# Objetivo: Obtener la contribución individual de cada parche a la conectividad general del paisaje

# En este caso utilizaremos el método presentado en el artículo de Saura & Pascual-Hortal (2007) “A new habitat availability index to integrate connectivity in landscape conservation planning: Comparison with existing indices and application to a case study“, utilizando su Indice Integral de Conectividad (IIC) y la ecuación 1 para derivar la relevancia de cada parche para la conectividad general.

rm(list = ls()) # Limpiar todos los objetos y variables en el entorno de trabajo

# Cargamos el paquete o libreria para hacer los análsis
library(MetaLandSim)

# Necesitamos una base de datos con la siguiente información sobre los parches:
# ID         - identificador númerico del parche
# X          - coordenada o longitud decimal
# Y          - coordenada o latitud decimal
# Area       - tamaño del parche, en hectáreas.
# Occupation - ausencia/presencia  (0/1).


# Nosotros vamos a usar una base de datos que simula un archipielago compuesto por 50 islas de un tamaño entre 0.056 ha (560 m2) y 0.165 ha (1650 m2).

# Importaremos los datos directamente desde MS Excel
library(readxl)
A13_patch_data <- as.data.frame(read_excel("A13_patch_data.xlsx"))
# usamos la función as.data.frame porque el paquete MetaLandSim necesita dataframes como archivos de entrada.

# Ahora, creamos un objeto de la clase "metapopulation", esto es porque en nuestra base
# de datos tenemos una columna de presencia/ausencia de especies.
# Hay dos opciones en las que debemos elegir un valor:
# mapsize es la longitud del eje x o y, en metros
# dispersal es la capacidad de dispersión media de la especie, en metros.
mp1 <- convert.graph(dframe = A13_patch_data,
                     mapsize = 600,
                     dispersal = 70)

# Ahora, convertimos el objeto mp1 ("metapoupaltion") en un objeto del tipo "landscape"
# los objetos del tipo gráfico del paisaje no contienen información de las especies.
rl1 <- remove.species(sp = mp1)

## EXTRA: Podemos simular gráficos de paisaje con las siguientes instrucciones:
## set.seed(123)
## La opción plotG = TRUE, muestra el gráfico
## rl0 <- rland.graph(mapsize=500, dist_m=50, areaM=0.1,
##                    areaSD=0.02, Npatch=50,
##                    disp=70, plotG=TRUE)

# Algunas estadísticas de nuestro paisaje
summary_landscape(rl1)

# Visualizamos nuestros datos
plot_graph(rl = rl1, species = FALSE, links = FALSE)

# Este paisaje tiene 50 parches, y queremos conocer cada dIIC individual, o su
# contribución individual a la conectividad.
# Cada punto representa un parche y los clusters o grupos se dibujan en colores
# diferentes.
# El área de cada parche es representada por el círculo que rodea al punto.

# La ecuación 1 en el artículo de Saura et al. implica computar el IIC del paisaje completo, con todos los parches (full.IIC):
# la función as.numeric es sólo para garantizar que la salida es sólo un número
full.IIC <- as.numeric(metrics.graph (rl = rl1, metric = "IIC"))

# Ahora extraemos una base de datos con la información de los parches:
patches <- rl1$nodes.characteristics
# Esta base datos contiene la siguiente info.
head(patches)

# Luego creamos un vector para insertar los valores del IIC parcial (dIIC). Usamos una función personalizada para remover puntos. La versión original en el paquet MetaLandSim elimina una serie de parches de forma aleatoria. Esta versión adaptada elimina un parche específico (por ID), se llama removepoints.byID.

dIIC <- rep(NA, 50)

# La función personalizada
removepoints.byID <-
  function (rl, nr = 1, ID) {
    if (class(rl) != "landscape")
    {
      stop(paste(rl, " should be an object of class class 'landscape'.", sep =
                   ""),
           call. = FALSE)
    }
    mapsize2 <- rl$mapsize
    dist_m2 <- rl$minimum.distance
    areaM2 <- rl$mean.area
    areaSD2 <- rl$SD.area
    Npatch2 <- rl$number.patches
    disp2 <- rl$dispersal
    rl_0 <- rl$nodes.characteristics
    ID2 <- rl_0$ID
    nr_select <- nrow(rl_0) - nr
    rl_1 <- rl_0[-ID,]
    rl_2 <- rl_1[sort.list(as.numeric(rownames(rl_1))),]
    names(rl_2)[names(rl_2) == "ID2"] <- "ID"
    rl_3 <- list(
      mapsize = mapsize2,
      minimum.distance = dist_m2,
      mean.area = mean(rl_2$areas),
      SD.area = sd(rl_2$areas),
      number.patches = nrow(rl_2),
      dispersal = disp2,
      nodes.characteristics = rl_2
    )
    class(rl_3) <- "landscape"
    rl_4 <- cluster.id(rl_3)
    rownames(rl_4$nodes.characteristics) <-
      1:nrow(rl_4$nodes.characteristics)
    class(rl_4) <- "landscape"
    return(rl_4)
  }

# Entonces calculamos el dIIC (conectividad parcial) para cada parche:

# Usamos un loop para hacer el cálculo
for (i in 1:50) {
  rl2 <- rl1 #This is just no to change rl1
  rl3 <- removepoints.byID(rl1, ID = i) #removing patch i
  partial.IIC <- as.numeric(metrics.graph (rl = rl3, metric = "IIC"))
  dIIC[i] <-
    100 * ((full.IIC - partial.IIC) / full.IIC) #send the result to the vector
}

dIIC

# Ahora lo visualizamos activando la opción de links, para ver la conexión entre parches.
plot_graph(rl = rl1,
           species = FALSE,
           links = TRUE)

# y ponemos en la figura la información del dIIC para cada parche
text(
  x = patches[, 'x'],
  y = patches[, 'y'],
  pos = 3,
  offset = 0.2,
  labels = round(dIIC, 2)
)



