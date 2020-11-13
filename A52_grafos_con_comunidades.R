#---------------------------------------------------------------------------
## II Curso Internacional "Uso de la teoría de grafos en la evaluación de la
## conectividad larval de poblaciones marinas"
## contacto: Andres Ospina-Alvarez (aospina.co@me.com)
## Organizadores(as) del curso: Catherine González (catherine.gonzalez@ifop.cl)
#---------------------------------------------------------------------------

## Título: Proyectando un grafo con nodos categorizados por comunidades no-biológicos usando ggraph

rm(list = ls()) # Limpiar todos los objetos y variables en el entorno de trabajo

library(igraph) # Cargar la librería igraph
library(readxl)
library(ggraph)

# ------------------ #
#Normalize function
# ------------------ #
normalize_fun <- function(x) {
  a <- min(x)
  b <- max(x)
  (x - a)/(b - a)
}

g4.data <-
  read_excel("A31_caletas.xlsx") # Leer una matriz de adyacencia almacenada en un archivo MS Excel
head(g4.data)

g4.empty.graph <-
  graph_from_adjacency_matrix(as.matrix(g4.data), mode = c("undirected"))

gs <- list()
gs[[1]]      <- g4.empty.graph

# Y con la siguiente función calcularemos varias métricas para nuestro grafo
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



# Proyectamos un grafo con comunidades identificadas por colores usando el método que deseemos
##### Proyección de grafo con nodos de colores indicando la comunidad no-biológica hallada mediante optimización de la modularidad y un enfoque jerárquico ####
ggraph(g4, layout = "kk") +
  geom_edge_parallel0(width = 1,
                      edge_colour = "#D3ECF2") +
  
  geom_node_point(
    aes(fill = as.factor(cluster_louvain(g4)$membership), size = degree),
    colour = "#000000",
    shape = 21,
    stroke = 0.3
  ) +
  scale_fill_brewer(palette = "Set2", 
                    na.value = "gray53") + 
  scale_size(range = c(3, 8)) +
  
  geom_node_text(aes(label = name),
                 colour = "black",
                 size = 4,
                 family = "serif",
                 repel = TRUE) +
  
  
  guides(
    fill = guide_legend("Cluster Louvain"),
    size = guide_legend("Node degree")
  ) +
  
  theme_graph() +
  theme(legend.position = "right")


##### Proyección de grafo con nodos de colores indicando la comunidad no-biológica hallada mediante asignación de los nodos al mismo grupo que sus vecinos mas próximos ####
ggraph(g4, layout = "kk") +
  geom_edge_parallel0(width = 1,
                      edge_colour = "#D3ECF2") +
  
  geom_node_point(
    aes(fill = as.factor(cluster_infomap(g4)$membership), size = degree),
    colour = "#000000",
    shape = 21,
    stroke = 0.3
  ) +
  scale_fill_brewer(palette = "Set2", 
                    na.value = "gray53") + 
  scale_size(range = c(3, 8)) +
  
  geom_node_text(aes(label = name),
                 colour = "black",
                 size = 4,
                 family = "serif",
                 repel = TRUE) +
  
  
  guides(
    fill = guide_legend("Cluster Infomap"),
    size = guide_legend("Node degree")
  ) +
  
  theme_graph() +
  theme(legend.position = "right")

