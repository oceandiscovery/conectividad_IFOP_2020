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

# ------------------ #
#Normalize function
# ------------------ #
normalize_fun <- function(x) {
  a <- min(x)
  b <- max(x)
  (x - a) / (b - a)
}

##### CARGA DE DATOS ####
# Importaremos un set de datos correspondientes a un ejercicio de simulación donde se acopló un modelo biológico basado en características individuales a un modelo hidrodinámico para la costa de Chile. El PLD de la especie es de 20 días que presenta comportamiento de migración vertical, con un periodo de competencia de 8 días y hemos filtrado para solo trabajar con el asentamiento en invierno austral.
data_ready <- readRDS("A72_data_PLD_20_DVM_WINTER.rds")
join_pos <- readRDS("A73_geocode_zones.rds")

##### CONTINUACION DE A71 ####
data.attributes <- data.frame(name = as.character(1:405),
                        x    = join_pos$lon,
                        y    = join_pos$lat,
                        id   = 1:405)

data.relations <- ddply(data_ready, c("zone_ini", "zone_end"), summarise,
                                  weight = sum(super_particle_size, na.rm=TRUE))

### Creación de un grafo ponderado usando la matriz realizada ####
g1 <- graph_from_data_frame(data.relations, directed=TRUE, vertices=data.attributes)

### Manipulando grafos y convirtiendo a otros formatos y viceversa ####
# Vamos a quedarnos con un grafo mas pequeño que solo contenga las zonas 221 a 405
g2 <- induced.subgraph(g1,
                        which(V(g1)$id >= 221))

# Volvamos este grafo una matriz de adyacencia, aedmás extraigamos la fuerza de la conexión, es decir la ponderación, que ya sabemos que son el número de larvas que se intercambian entre celdas
con.mat.rea.2 <- as_adjacency_matrix(g2, attr="weight")

# Pero nos interesa mas tener una matriz realizada convencional
con.mat.rea.2 <- as.matrix(as_adjacency_matrix(g2, attr="weight"))

# Nos interesan las dimensiones para el siguiente paso
dim(con.mat.rea.2)

# Convertimos esta matriz realizada en una matriz potencial y verificamos la suma de las columnas
con.mat.pot.2 <- con.mat.rea.2 / t(matrix(rep(colSums(con.mat.rea.2),dim(con.mat.rea.2)[1]),nrow = dim(con.mat.rea.2)[1], ncol = dim(con.mat.rea.2)[1]))
colSums(con.mat.pot.2)

# Y volvamos a crear un nuevo grafo con la matriz potencial
g3 <- graph_from_adjacency_matrix(con.mat.pot.2, weighted=TRUE)

# Veamos si contiene las probabilidades
head(E(g3)$weight)

g3
# Pero hemos perdido las posiciones y los nombres ¿Qué hacemos?
V(g3)$name <- data.attributes$name[221:405]
V(g3)$x <- data.attributes$x[221:405]
V(g3)$y <- data.attributes$y[221:405]

g3

# Ahora trabajemos con el grafo g3


### Crear una lista y poner g3 ####
gs <- list()
gs[[1]] <- g3

# Esta sección determina que el "strength" sea la fuerza de la conexión y que el costo sea su valor inverso.
gs <- lapply(gs, function(x) {
  E(x)$strength                  <- E(x)$weight
  E(x)$cost                      <- mean(E(x)$weight) / E(x)$weight
  return(x)
})

# Esta sección fuerza que los "weights" en el grafo representen el costo de conexión, lo que parece ser la manera por defecto en la que trabaja igraph
gs <- lapply(gs, function(x) {
  E(x)$weight                    <- E(x)$cost
  return(x)
})

gs <- lapply(gs, function(x) {
  # Centrality measures
  V(x)$degree                     <- degree(x, mode = "total")
  V(x)$indegree                   <- degree(x, mode = "in")
  V(x)$outdegree                  <- degree(x, mode = "out")
  # Aquí se calcula betweenness respecto al costo (de esta manera es idéntico al calculado pr la librería tnet)
  V(x)$betweenness                <- betweenness(x)
  # Aquí se calcula betweenness respecto a la fuerza
  V(x)$betweenness.strength       <- betweenness(x, weights = E(x)$strength) 
  # V(x)$closeness                  <- closeness(x)

  
  # Aquí se calcula E. betweenness respecto al costo
  E(x)$betweenness                <- edge.betweenness(x) 
  # Aquí se calcula E. betweenness respecto a la fuerza
  E(x)$betweenness.strength       <- edge.betweenness(x, weights = E(x)$strength)
  
  
  V(x)$strength                   <-
    strength(x,
             weights = E(x)$strength,
             mode = "all",
             loops = T)
  V(x)$in_strength               <-
    strength(x,
             weights = E(x)$strength,
             mode = "in",
             loops = T)
  V(x)$out_strength              <-
    strength(x,
             weights = E(x)$strength,
             mode = "out",
             loops = T)
  
  V(x)$evcent                    <- evcent(x)$vector
  V(x)$hub_score                 <- hub.score(x)$vector
  V(x)$auth_score                <- authority.score(x)$vector
  V(x)$page_rank                 <- page_rank(x)$vector
  
  # Normalized Centrality measures
  V(x)$n.degree                  <- normalize_fun(V(x)$degree)
  V(x)$n.strength                <- normalize_fun(V(x)$strength)
  V(x)$n.in_strength             <- normalize_fun(V(x)$in_strength)
  V(x)$n.out_strength            <- normalize_fun(V(x)$out_strength)
  V(x)$n.betweenness             <- normalize_fun(V(x)$betweenness)
  V(x)$n.betweenness.strength    <- normalize_fun(V(x)$betweenness.strength)
  V(x)$n.evcent                  <- normalize_fun(V(x)$evcent)
  # V(x)$n.closeness               <- normalize_fun(V(x)$closeness)
  V(x)$n.pagerank                <- normalize_fun(V(x)$page_rank)
  V(x)$n.auth_score              <- normalize_fun(V(x)$auth_score)
  V(x)$n.hub_score               <- normalize_fun(V(x)$hub_score)
  
  E(x)$n.edge_betweenness        <- normalize_fun(E(x)$betweenness)
  E(x)$n.edge_betweenness.strength<- normalize_fun(E(x)$betweenness.strength)
  E(x)$n.edge_weight             <- normalize_fun(as.numeric(E(x)$weight))
  E(x)$n.edge_strength           <- normalize_fun(as.numeric(E(x)$strength))
  E(x)$n.edge_cost               <- normalize_fun(as.numeric(E(x)$cost))
  return(x)
})

g <- gs[[1]]

##### Grafo geo-posicionado ####
library(mapdata)

maptheme <-
  theme_minimal(base_size = 20) %+replace% #Relative size of plot
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0, 0),
    legend.justification = c(0, 0),
    legend.background = element_blank(),
    # Remove overall border
    legend.key = element_blank(),
    # Remove border around each item
    panel.background = element_rect(fill = "#F2F2F2"),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm')
  )

country_shapes <-
  geom_polygon(
    aes(x = long, y = lat, group = group),
    data = map_data('world'),
    fill = "#CECECE",
    color = "#515151",
    size = 0.15
  )
mapcoords <-
  coord_fixed(xlim = c(-73.5, -70.5), ylim = c(-36, -31.5))

### ¿Qué paletas de colores para nodos y arcos quieres usar?
cbpalette_arcs_low <- "#87CEFF" # Light blue
cbpalette_arcs_high <- "#27408B" # Dark blue
cbpalette_arcs <- "GnBu"
cbpalette_nodes <- "YlOrRd"

### El color de las etiquetas de los nodos en el gráfico de la red.
label_color_1 <- "navy"

### Posición de la leyenda (arriba, abajo, derecha, izquierda)
legend_pos <- "right"


##### Un grafo con la fuerza de la conexión (probabilidad) para los arcos y el weigthed degree (strength) para los nodos #####
g <- gs[[1]]
ggraph(g,
       layout = "manual",
       x = V(g)$x,
       y = V(g)$y) +
  country_shapes +
  
  geom_edge_arc(
    aes(
      edge_color = strength,
      edge_alpha = strength,
      edge_width = strength
    ),
    strength = 0.6
  ) +
  
  scale_edge_colour_gradient(low = cbpalette_arcs_low,
                             high = cbpalette_arcs_high) +
  
  scale_edge_alpha(range = c(0.1, 0.7)) +
  
  scale_edge_width_continuous(range = c(0.05, 0.3)) +
  
  geom_node_point(aes(size = strength, color = strength)) +
  scale_color_gradientn(colours = brewer_pal(palette = cbpalette_nodes)(5)) +
  scale_size_continuous(range = c(0.1, 2)) +
  scale_alpha_continuous(range = c(0.4, 1)) +
  
  guides(
    size       = guide_legend("node strength"),
    color      = guide_legend("node strength"),
    edge_alpha = guide_legend("edge strength"),
    edge_width = guide_legend("edge strength"),
    edge_color = guide_legend("edge strength")
  ) +
  mapcoords +
  maptheme +
  theme(
    legend.position = "left",
    legend.text = element_text(size = 12, lineheight = 0.5),
    legend.key.height = unit(0.5, "cm")
  )

# Tema de discusión: ¿Qué zonas o latitudes podríamos considerar prioritarias en cuestiones de gestión de los recursos a partir de este grafo? Recordad que estamos refiriendonos a spp. que tengan un PLD == 20 y reclutamiento en invierno austral?

##### Un grafo con la centralidad de intermediación (betweenness) para los nodos y arcos #####
g <- gs[[1]]
ggraph(g,
       layout = "manual",
       x = V(g)$x,
       y = V(g)$y) +
  country_shapes +
  
  geom_edge_arc(
    aes(
      edge_color = n.edge_betweenness,
      edge_alpha = n.edge_betweenness,
      edge_width = n.edge_betweenness
    ),
    strength = 0.6
  ) +
  
  scale_edge_colour_gradient(low = cbpalette_arcs_low,
                             high = cbpalette_arcs_high) +
  
  scale_edge_alpha(range = c(0.1, 0.7)) +
  
  scale_edge_width_continuous(range = c(0.05, 0.3)) +
  
  geom_node_point(aes(size = n.betweenness, color = n.betweenness)) +
  scale_color_gradientn(colours = brewer_pal(palette = cbpalette_nodes)(5)) +
  scale_size_continuous(range = c(0.1, 2)) +
  scale_alpha_continuous(range = c(0.4, 1)) +
  
  guides(
    size       = guide_legend("node betweenness"),
    color      = guide_legend("node betweenness"),
    edge_alpha = guide_legend("edge betweenness"),
    edge_width = guide_legend("edge betweenness"),
    edge_color = guide_legend("edge betweenness")
  ) +
  mapcoords +
  maptheme +
  theme(
    legend.position = "left",
    legend.text = element_text(size = 12, lineheight = 0.5),
    legend.key.height = unit(0.5, "cm")
  )

# Recordad que hemos centrado nuestro análisis en una zona en concreto, teniendo en cuenta esta restricción ¿Qué podemos deducir de este grafo?