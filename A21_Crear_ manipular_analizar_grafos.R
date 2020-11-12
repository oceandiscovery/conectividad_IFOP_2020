#---------------------------------------------------------------------------
## II Curso Internacional "Uso de la teoría de grafos en la evaluación de la
## conectividad larval de poblaciones marinas"
## contacto: Andres Ospina-Alvarez (aospina.co@me.com)
## Organizadores(as) del curso: Catherine González (catherine.gonzalez@ifop.cl)
#---------------------------------------------------------------------------

# Título: Crear, manipular y analizar grafos (nivel básico)

rm(list = ls()) # Limpiar todos los objetos y variables en el entorno de trabajo

library(igraph) # Cargar la librería igraph

#### (1) Crear nuestra primer grafo usando una lista de arcos ####
g1 <- graph(
  edges = c(1, 2, 2, 3, 1, 4, 2, 5, 3, 5),
  n = 5,
  directed = F
)

# ¿Cómo está guardada la estructura de nuestro grafo en R
g1

# ¿Cómo se visualiza gráficamente el grafo g1?
plot(g1)

# Adicionemos algunos arcos mas al grafo existente
g1 <- add_edges(g1, c(4, 5, 3, 1))
plot(g1)

# Adicionemos nuevos vértices o nodos al grafo
g1 <- add_vertices(g1, 2)
plot(g1)

is.connected(g1)

# Adicionemos otros arcos mas al grafo
g1 <- add_edges(g1, c(6, 7, 7, 3))
plot(g1)

is.connected(g1)



#### (2) Crear un segundo grafo que represente la conectividad larval entre islas del Pacífico Oriental, desconocemos la probabilidad o magnitud de la conectividad, solo tenemos conocimiento de la existencia de algunas de las relaciones de conectividad entre islas, por esta razón usaremos una matriz binaria, pero también es posible usar matrices de probabilidad y con ponderaciones (e.g. número de larvas que se intercambian entre localidades) ####

g2 <-
  make_graph(
    c(
      "Robinson Crusoe",     "Santa Clara",
      "Robinson Crusoe",     "Alejandro Selkirk",
      "Robinson Crusoe",     "Alejandro Selkirk",
      "Alejandro Selkirk",   "Santa Clara",
      "San Felix",           "San Ambrosio",
      "San Felix",           "González",
      "San Ambrosio",        "González",
      "Rapa Nui",            "Salas y Gómez",
      "Pitcairn",            "Salas y Gómez",
      "Pitcairn",            "Rapa Nui",
      "Robinson Crusoe",     "San Ambrosio",
      "Robinson Crusoe",     "San Felix",
      "Alejandro Selkirk",   "San Ambrosio",
      "Alejandro Selkirk",   "San Felix",
      "Alejandro Selkirk",   "Salas y Gómez"
    ),
    directed = FALSE
  )

plot(g2,
     vertex.size = 30,
     vertex.color = "lightblue")

# Habíamos visto que podíamos acceder a los grafos como objetos de R, ahora accedamos a los vértices o a los arcos específicaamente
V(g2)
E(g2)

# Y ahora empecemos a hacer preguntas sobre el grafo y a calcular métricas

is.directed(g2) # ¿Es g2 un grafo dirigido?
is.weighted(g2) # ¿Es g2 un grafo con valores o ponderado?
is.simple(g2) # ¿Es g2 un grafo simple?
is.loop(g2) # ¿Hay bucles en el grafo g2?

#### (3) Creemos un grafo mas completo, con ponderaciones de las conexiones entre vértices ####
# Exportemos el grafo anterior (g2) como una lista de arcos a un archivo separado por comas
A21_g2_to_export <- as_edgelist(g2)
write.csv(x = A21_g2_to_export, file = "A21_g2_to_export.csv")

# Externamente, agregamos una serie de pesos o probabilidades a la lista de arcos.
# Importamos el archivo
A21_g2_to_import <- read.csv(file = "A21_g2_to_import.csv")

# Y ahora vamos a asumir que conocemos la dirección y ponderación de la conexión (from, to, weigth)
g3 <- graph_from_data_frame(A21_g2_to_import[,2:4], directed = TRUE)

plot(g3)

is.directed(g3) # ¿Es g3 un grafo dirigido?
is.weighted(g3) # ¿Es g3 un grafo con valores o ponderado?
is.simple(g3) # ¿Es g2 un grafo simple?
is.loop(g3) # ¿Hay loops en el grafo g2?


# Se puede representar el valor de la conexión en el grafo
plot(g3, layout=layout_with_kk,
     edge.width = E(g3)$weight * 4,
     edge.color = rainbow(length(factor(E(g3)$weight)), alpha = 0.3),
     vertex.color="lightblue")

# Pero los gráficos básicos que genera la librería igraph no permiten mucha personalización, por ejemplo, es casí imposible poner una leyenda que nos indique que son los colores de los arcos.

# Veamos algunas otras formas mas de personalizar el grafo...
plot(
  g3,
  main                = "Red de conexión larval\n entre islas del Pacífico Oriental",
  sub                 = "¡Atención! Red meramente hipotética",
  edge.curved         = TRUE,
  edge.lty            = 3,
  vertex.label.cex    = 1,
  vertex.label.dist   = -1,
  vertex.label.color  = "blue",
  vertex.color        = "lightblue",
  edge.color          = "orange",
  edge.width          = E(g3)$weight * 3,
  vertex.size         = 12
)
# Nos empezamos a quedar un poco cortos con la función básica de igraph para dibujar grafos, pero por ahora pasemos a otro tema...

#### (4) Cálculo de métricas (básico) ####
# Esta sección será de diálogo con los participantes para discutir sobre las métricas y su significado

degree(g3)
degree(g3, normalized = T) # Alternativamente, podríamos usar el valor normalizado para hacer un nuevo gráfico donde el tamaño de los vértices corresponda a este valor.

# También es posible calcular el degree de entrada y salida
degree(g3, mode = "out")
degree(g3, mode = "in")

# La densidad de un gráfico es la relación entre el número de vértices y el número de vértices posibles.
edge_density(g3)

# Y obviamente, podemos calcular las distancias entre vértices.
distances(g3)
distances(g3, mode = "in")
distances(g3, mode = "out")
# ¿Cómo se calculan? Teclea en la consola "help(distances)"

mean_distance(g3, directed = T)


# Hasta el momento todas estas medidas son descriptivas y aunque nos proporcionan información es más interesante empezar a categorizar los vértices de acuerdo a sus relaciones.

# Empecemos con algo muy sencillo de calcular: Promedio al vecino más cercano.
# Mas información: help(knn)
# Vamos a listar (sort) en sentido decreciente (decreasing = T), los valores de knn en una red simplificada (simplify).
# La razón de simplificar la red es que el cálculo de knn no admite redes con loops
sort(knn(simplify(g3))$knn, decreasing = T)
# Tema de discusión grupal: ¿Por qué San Félix tiene el valor mas alto de knn? ¿Por qué Rapa Nui el mas bajo?



# Recordad ahora un poco de historia de la ciencia, específicamente cuando Darwin se inspira en un ensayo del economista T. Malthus para extender la lucha natural y siempre presente por la supervivencia al esquema evolutivo. El tema central de la obra de Malthus era que el crecimiento de la población siempre superaría al crecimiento del suministro de alimentos, creando estados perpetuos de hambre, enfermedad y lucha. 

# Ahora nosotros vamos a hacer algo similar. Vamos a tomar el concepto de diversidad de los vértices, tal como la describen Eagle et al. 2010 en un artículo de Economía publicado en Science.
sort(diversity(g3, weights = E(g3)$weight), decreasing = T)
# La diversidad de un vertice se define como la entropía (ponderada) de Shannon de los pesos de sus arcos incidentes.

# Tema de discusión grupal: ¿Cómo podemos trasladar la métrica de diversidad a redes de conexión larval? ¿Qué significado tiene un mayor valor de diversidad, o uno menor?


##### (5) Una figura final de nuestro grafo ####
V(g3)$degree     <-   degree(g3, mode = "all")
V(g3)$in_degree  <-   degree(g3, mode = "in")
V(g3)$out_degree <-   degree(g3, mode = "out")
V(g3)$diversity  <-   diversity(g3, weights = E(g3)$weight)
V(g3)$knn        <-   knn(simplify(g3))$knn

plot(
  g3,
  main                = "Red de conexión larval\n entre islas del Pacífico Oriental",
  sub                 = "¡Atención! Red meramente hipotética",
  edge.curved         = TRUE,
  edge.lty            = 5,
  vertex.label.cex    = 1,
  vertex.label.dist   = -1,
  vertex.label.color  = "blue",
  vertex.color        = "lightblue",
  edge.color          = "orange",
  edge.width          = E(g3)$weight * 2,
  vertex.size         = V(g3)$knn * 5,
)
