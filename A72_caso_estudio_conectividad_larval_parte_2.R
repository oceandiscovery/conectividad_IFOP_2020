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
# Importaremos un set de datos correspondientes a un ejercicio de simulación donde se acopló un modelo biológico basado en características individuales a un modelo hidrodinámico para la costa de Chile. El PLD de la especie es de 20 días que presenta comportamiento de migración vertical, con un periodo de competencia de 8 días y hemos filtrado para solo trabajar con el asentamiento en invierno austral.
data_ready <- readRDS("A72_data_PLD_20_DVM_WINTER.rds")


##### CONTINUACION DE A71 ####

# Buscamos el punto medio de cada zona especificada en el modelo, este sera el punto central de nuestro nodo o vertice.
mean_ini_pos <-
  ddply(
    data_ready,
    "zone_ini",
    summarise,
    lon = mean(longitude_ini),
    lat = mean(latitude_ini)
  )
mean_end_pos <-
  ddply(
    data_ready,
    "zone_end",
    summarise,
    lon = mean(longitude_end),
    lat = mean(latitude_end)
  )

# Ajuste para la zona final
join_pos = rbind(mean_ini_pos[, 2:3], mean_end_pos[404:405, 2:3])
saveRDS(join_pos, "A73_geocode_zones.rds")

### Creación del grafo ponderado usando la matriz potencial ####
cmat.pot <- readRDS("A72_potential_connectivity_matrix_from_PLD_20_DVM_WINTER.rds")

con.plot <-
  graph.adjacency(
    t(cmat.pot),
    weighted = TRUE,
    diag = TRUE,
    mode = c("directed")
  )

# Recordad que recortamos la matriz para evitar los bordes conflictivos
V(con.plot)$name         <- seq(3:404)
V(con.plot)$x            <- join_pos$lon[3:404]
V(con.plot)$y            <- join_pos$lat[3:404]

# Todo esto es porque igraph asume que los pesos o ponderaciones son costos
E(con.plot)$strength     <- E(con.plot)$weight
E(con.plot)$cost         <- mean(E(con.plot)$weight) / E(con.plot)$weight
E(con.plot)$weight       <- E(con.plot)$cost


V(con.plot)$degree       <- degree(con.plot, mode = "all")
V(con.plot)$in_degree    <- degree(con.plot, mode = "in")
V(con.plot)$out_degree   <- degree(con.plot, mode = "out")
V(con.plot)$strength     <- strength(con.plot,
                                     weights = E(con.plot)$strength,
                                     mode = "all", loops = T)
V(con.plot)$in_strength  <- strength(con.plot,
                                     weights = E(con.plot)$strength,
                                     mode = "in",loops = T)
V(con.plot)$out_strength <- strength(con.plot,
                                     weights = E(con.plot)$strength,
                                     mode = "out", loops = T)

# Aquí se calcula betweenness respecto al costo
V(con.plot)$betweenness          <- betweenness(con.plot)

# # Aquí se calcula betweenness respecto a la fuerza
# V(con.plot)$betweenness.strength <-
#   betweenness(con.plot, weights = E(con.plot)$strength)

# Aquí se calcula E. betweenness respecto al costo
E(con.plot)$betweenness          <- edge.betweenness(con.plot)

# # Aquí se calcula E. betweenness respecto a la fuerza
# E(con.plot)$betweenness.strength <-
#   edge.betweenness(con.plot, weights = E(con.plot)$strength)


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
  coord_fixed(xlim = c(-74.5, -70.9), ylim = c(-40, -31.3))

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
g <- con.plot
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
    legend.position = legend_pos,
    legend.text = element_text(size = 12, lineheight = 0.5),
    legend.key.height = unit(0.5, "cm")
  )

# Aquí no hay ningún misterio, es la misma información que nos brinda la matriz de conectividad

# Discutamos sobre esta representación...

# Ahora pasemos a usar una medida de centralidad, usaremos betweenness
##### Un grafo con la centralidad de intermediación (betweenness) para los nodos y arcos #####
g <- con.plot
ggraph(g,
       layout = "manual",
       x = V(g)$x,
       y = V(g)$y) +
  country_shapes +
  
  geom_edge_arc(
    aes(
      edge_color = betweenness,
      edge_alpha = betweenness,
      edge_width = betweenness
    ),
    strength = 0.6
  ) +
  
  scale_edge_colour_gradient(low = cbpalette_arcs_low,
                             high = cbpalette_arcs_high) +
  
  scale_edge_alpha(range = c(0.1, 0.7)) +
  
  scale_edge_width_continuous(range = c(0.05, 0.3)) +
  
  geom_node_point(aes(size = betweenness, color = betweenness)) +
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
    legend.position = legend_pos,
    legend.text = element_text(size = 12, lineheight = 0.5),
    legend.key.height = unit(0.5, "cm")
  )

# Como se puede ver hay algunos resultados de interés.

# Tema de discusión: ¿Cómo podemos aprovechar esta información en la gestión?

# ... pero este grafo es demasiado grande para un propósito didáctico, vamos a recortarlo...

V(con.plot)$id <- 3:404

gg2 <- induced.subgraph(con.plot,
                  which(V(con.plot)$id >= 221))

mapcoords <-
  coord_fixed(xlim = c(-73.5, -70.5), ylim = c(-36, -31.5))


g <- gg2
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
    legend.position = legend_pos,
    legend.text = element_text(size = 12, lineheight = 0.5),
    legend.key.height = unit(0.5, "cm")
  )

# Ahora bien recordad que no estamos analizando desde cero este segmento de costa, estamos arrastrando los valores de conectividad larval de la matriz original.

# Por lo tanto, deberíamos tratar el grafo o la matriz de conectividad de manera diferente para poder fiarnos de las diferentes métricas de conectividad.