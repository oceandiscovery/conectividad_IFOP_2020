#---------------------------------------------------------------------------
## II Curso Internacional "Uso de la teoría de grafos en la evaluación de la
## conectividad larval de poblaciones marinas"
## contacto: Andres Ospina-Alvarez (aospina.co@me.com)
## Organizadores(as) del curso: Catherine González (catherine.gonzalez@ifop.cl)
#---------------------------------------------------------------------------

# Título: Calcular diferentes medidas de centralidad para un grafo y representarlas gráficamente

rm(list = ls()) # Limpiar todos los objetos y variables en el entorno de trabajo

library(readxl)
library(igraph) # Cargar la librería igraph
library(ggraph)

g4.data <-
  read_excel("A31_caletas.xlsx") # Leer una matriz de adyacencia almacenada en un archivo MS Excel
head(g4.data)

# Aquí creamos un grafo a partir de la matriz de adyacencia
g4.empty.graph <-
  graph_from_adjacency_matrix(as.matrix(g4.data), mode = c("undirected"))

plot(g4.empty.graph)
# Puede verse como la visualización del grafo usando igraph no es de lo mejor, por tal razón es preferible usar la libreria ggraph, como haremos más adelante.

# Por comodidad crearemos una lista vacía, donde pondremos el grafo g4.empty.graph

gs <- list()
gs[[1]]      <- g4.empty.graph

# Y con la siguiente función calcularemos varias métricas para nuestro grafo
##### Cálculo de las medidas de centralidad mas comunes ####
gs <- lapply(gs, function(x) {
  # Centrality measures
  V(x)$degree       <- degree(x, mode = "total")
  V(x)$indegree     <- degree(x, mode = "in")
  V(x)$outdegree    <- degree(x, mode = "out")
  V(x)$betweenness  <- betweenness(x, normalized = TRUE)
  V(x)$evcent       <- evcent(x)$vector
  V(x)$closeness    <- closeness(x, normalized = TRUE)
  V(x)$strength     <- strength(x, mode = "all", loops = F)
  V(x)$in_strength  <- strength(x, mode = "in", loops = F)
  V(x)$out_strength <- strength(x, mode = "out", loops = F)
  V(x)$hub_score    <- hub.score(x)$vector
  V(x)$auth_score   <- authority.score(x)$vector
  V(x)$page_rank    <- page_rank(x)$vector
  
  E(x)$betweenness  <- edge.betweenness(x)
  return(x)
})

# Ahora podemos "deslistar" el grafo g4 y trabajar con el.
g4 <- gs[[1]]

# Veamos todo lo que contiene g4
g4

# Visualicemos o extraigamos los valores de degree, por ejemplo
V(g4)$degree
# Como vemos nos muestra una serie de valores, uno por cada nodo del grafo, pero no sabemos los nombres y es poco intuitivo

# Podemos obtener una mejor visualización de todos las medidas de centralidad calculadas para el grafo g4, vamos a ello...
get.data.frame(g4, what = 'vertices')


##### Crear un objeto dataframe con todas las medidas de centralidad calculadas ####
# Podemos guardarla como un objeto, por si fuese necesario exportar la lista para su uso mas adelante, lo mismo podemos hacer con la lista de enlaces.
g4.cent.measures.nodes <- get.data.frame(g4, what = 'vertices')
g4.cent.measures.edges <- get.data.frame(g4, what = 'edges')

write.csv(g4.cent.measures.nodes, "A41_g4.cent.measures.nodes.csv", row.names = F)

##### Figura destacando el valor de degree en los nodos ####
# Y ahora crearemos una visualización del grafo g4 usando ggraph donde destacaremos la centralidad de los nodos basada en degree
ggraph(g4, layout = "kk") +
  geom_edge_parallel0(width = 1,
                      edge_colour = "#D3ECF2") +
  
  geom_node_point(
    aes(fill = degree,
        size = degree),
    colour = "#000000",
    shape = 21,
    stroke = 0.3
  ) +
  scale_fill_gradient(low = "#ADBDF0",
                      high = "#F28383") +
  scale_size(range = c(6, 10)) +
  
  geom_node_text(aes(label = name),
                 colour = "black",
                 size = 4,
                 family = "serif") +
  
  
  guides(
    fill = guide_legend("Node degree"),
    size = guide_legend("Node degree")
  ) +
  
  theme_graph() +
  theme(legend.position = "right")

##### Figura destacando el valor de closeness en los nodos ####
# Destaquemos ahora la centralidad de los nodos basada en closeness
ggraph(g4, layout = "kk") +
  geom_edge_parallel0(width = 1,
                      edge_colour = "#D3ECF2") +
  
  geom_node_point(
    aes(fill = closeness,
        size = closeness),
    colour = "#000000",
    shape = 21,
    stroke = 0.3
  ) +
  scale_fill_gradient(low = "#ADBDF0",
                      high = "#F28383") +
  scale_size(range = c(6, 10)) +
  
  geom_node_text(aes(label = name),
                 colour = "black",
                 size = 4,
                 family = "serif") +
  
  guides(
    fill = guide_legend("Node normalized\ncloseness"),
    size = guide_legend("Node normalized\ncloseness")
  ) +
  
  theme_graph() +
  theme(legend.position = "right")

##### Figura destacando el valor de betweenness en los nodos ####
# Destaquemos ahora la centralidad de los nodos basada en betweenness
ggraph(g4, layout = "kk") +
  geom_edge_parallel0(width = 1,
                      edge_colour = "#D3ECF2") +
  
  geom_node_point(
    aes(fill = betweenness,
        size = betweenness),
    colour = "#000000",
    shape = 21,
    stroke = 0.3
  ) +
  scale_fill_gradient(low = "#ADBDF0",
                      high = "#F28383") +
  scale_size(range = c(6, 10)) +
  
  geom_node_text(aes(label = name),
                 colour = "black",
                 size = 4,
                 family = "serif") +
  
  guides(
    fill = guide_legend("Node normalized\nbetweenness"),
    size = guide_legend("Node normalized\nbetweenness")
  ) +
  
  theme_graph() +
  theme(legend.position = "right")

##### Figura destacando el valor de betweenness en los nodos y en los arcos ####
# También es posible destacar valores en los arcos. Hagámoslo usando el edge betweenness
ggraph(g4, layout = "kk") +
  geom_edge_parallel0(aes(
    #alpha = betweenness,
    width = betweenness,
    colour = betweenness
  )) +
  scale_edge_colour_gradient(low = "#FFFFFF",
                             high = "#00801A") +
  scale_edge_width(range = c(0.3, 2.4)) +
  #scale_edge_alpha(range = c(1,1)) +
  
  geom_node_point(
    aes(fill = betweenness,
        size = betweenness),
    colour = "#000000",
    shape = 21,
    stroke = 0.3
  ) +
  scale_fill_gradient(low = "#ADBDF0",
                      high = "#F28383") +
  scale_size(range = c(6, 10)) +
  
  geom_node_text(aes(label = name),
                 colour = "black",
                 size = 4,
                 family = "serif") +
  
  guides(
    fill = guide_legend("Node normalized\nbetweenness"),
    size = guide_legend("Node normalized\nbetweenness"),
    edge_alpha = guide_legend("Edge betweenness"),
    edge_width = guide_legend("Edge betweenness"),
    edge_color = guide_legend("Edge betweenness")
  ) +
  
  theme_graph() +
  theme(legend.position = "right")

##### Figura destacando el valor de eigenvector en los nodos ####
# Destaquemos ahora la centralidad de los nodos basada en eigenvector
ggraph(g4, layout = "kk") +
  geom_edge_parallel0(width = 1,
                      edge_colour = "#D3ECF2") +
  
  geom_node_point(
    aes(fill = evcent,
        size = evcent),
    colour = "#000000",
    shape = 21,
    stroke = 0.3
  ) +
  scale_fill_gradient(low = "#ADBDF0",
                      high = "#F28383") +
  scale_size(range = c(6, 10)) +
  
  geom_node_text(aes(label = name),
                 colour = "black",
                 size = 4,
                 family = "serif") +
  
  guides(
    fill = guide_legend("Node eigenvector"),
    size = guide_legend("Node eigenvector")
  ) +
  
  theme_graph() +
  theme(legend.position = "right")

##### Figura destacando el valor de eigenvector en los PageRank ####
# Destaquemos ahora la centralidad de los nodos basada en PageRank
ggraph(g4, layout = "kk") +
  geom_edge_parallel0(width = 1,
                      edge_colour = "#D3ECF2") +
  
  geom_node_point(
    aes(fill = page_rank,
        size = page_rank),
    colour = "#000000",
    shape = 21,
    stroke = 0.3
  ) +
  scale_fill_gradient(low = "#ADBDF0",
                      high = "#F28383") +
  scale_size(range = c(6, 10)) +
  
  geom_node_text(aes(label = name),
                 colour = "black",
                 size = 4,
                 family = "serif") +
  
  guides(
    fill = guide_legend("Node page rank"),
    size = guide_legend("Node page rank")
  ) +
  
  theme_graph() +
  theme(legend.position = "right")

