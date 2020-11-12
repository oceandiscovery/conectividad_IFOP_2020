#---------------------------------------------------------------------------
## II Curso Internacional "Uso de la teoría de grafos en la evaluación de la
## conectividad larval de poblaciones marinas"
## contacto: Andres Ospina-Alvarez (aospina.co@me.com)
## Organizadores(as) del curso: Catherine González (catherine.gonzalez@ifop.cl)
#---------------------------------------------------------------------------

# Título: Calcular IIC - Indice Integral de Conectividad para áreas representadas como polígonos en un paisaje.

# Objetivo: Derivar la contribución de cada parche individual a la conectividad general del paisaje.


# La gran mayoría de las veces los parches de hábitat en un paisaje no se pueden representar como áreas circulares porque en la práctica son polígonos complejos. La gran ventaja es que hoy en día disponemos de una ingente cantidad de productos cartográficos que nos ofrecen esos polígonos en varios formatos (e.g. shapefile shp).

# Vamos a usar el paquete lcnonnect para importar y trabajar con esos polígonos.


# Instalamos el librería lconnect vía GitHub, para esto necesitamos la librería devtools
#install.packages("devtools")
library(devtools)
install_github("FMestre1/lconnect")
library(lconnect)


##### Importar un shapefile ####
# Aquí especificamos la ruta del archivo shp. En este caso vamos a usar un archivo shape de ejemplo, el cual hace parte de la librería lconnect
vec_path <- system.file("extdata/vec_projected.shp", package = "lconnect")

# Creamos un objeto de la clase ‘lconnect’:
land <- upload_land(vec_path, habitat = 1, max_dist = 500)
class(land)

# Generamos una visualización gráfica
plot(land, main="Parches de habitat")

# Ahora derivamos la importancia del parche (la contribución de cada parche individual a la conectividad general del paisaje):
land1 <- patch_imp(land, metric="IIC")
class(land1)
# Esto produce un objeto de la clase ‘pimp’, que quiere decir "patch importance plot"

# El valor de "pimp" está indicando el porcentaje de reducción de la métrica de conectividad que la pérdida de un parche específico representa en el paisaje total. 

# Los objetos de la clase pimp son muy útiles porque con ellos podemos graficar la contribución relativa de cada parche a la conectividad del paisaje
plot(land1, main="Priorización de parche (%)")

# No es muy extraño que el parche de mayor tamaño y en la posición mas central sea el mas representativo en la métrica de conectividad del paisaje.

