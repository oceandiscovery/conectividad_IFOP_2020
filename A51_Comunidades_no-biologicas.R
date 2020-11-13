#---------------------------------------------------------------------------
## II Curso Internacional "Uso de la teoría de grafos en la evaluación de la
## conectividad larval de poblaciones marinas"
## contacto: Andres Ospina-Alvarez (aospina.co@me.com)
## Organizadores(as) del curso: Catherine González (catherine.gonzalez@ifop.cl)
#---------------------------------------------------------------------------

## Título: Detección de comunidades no-biológicas mediante distintos algoritmos

rm(list = ls()) # Limpiar todos los objetos y variables en el entorno de trabajo

library(igraph) # Cargar la librería igraph
library(readxl)
library(ggraph)
library(snahelper)

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

# Y ahora creemos una visualización del grafo g4 usando ggraph
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

# Tenemos clara la estructura topológica de nuestro grafo, pero deseamos encontrar agrupaciones de nodos, estas agrupaciones se suelen llamar comunidades en la teoría de grafos, pero no equivalen a comunidades biológicas, son agrupaciones de nodos en razón de sus características de conectividad.

# Empecemos con el paseo aleatorio, una de las técnicas explicadas en las clases teóricas.
##### Detección de comunidades no-biológicas mediante "Random walks" ####
wc <- cluster_walktrap(g4)
plot(wc,
  g4,
  layout              = layout_with_kk,
  main                = "Red de caletas",
  sub                 = "¡Atención! Red meramente hipotética",
  edge.curved         = TRUE,
  vertex.label.cex    = 1,
  vertex.label.dist   = -1,
  vertex.size         = 4,
  ylim                = c(-1, 1),
  xlim                = c(-1, 1),
  asp                 = 0
)

##### Detección de comunidades no-biológicas mediante optimización de la modularidad ####
fc <- cluster_fast_greedy(g4)
plot(fc,
     g4,
     layout              = layout_with_kk,
     main                = "Red de caletas",
     sub                 = "¡Atención! Red meramente hipotética",
     edge.curved         = TRUE,
     vertex.label.cex    = 1,
     vertex.label.dist   = -1,
     vertex.size         = 4,
     ylim                = c(-1, 1),
     xlim                = c(-1, 1),
     asp                 = 0,
)


##### Detección de comunidades no-biológicas mediante optimización de la modularidad y un enfoque jerárquico ####
lc <- cluster_louvain(g4)
plot(lc,
     g4,
     layout              = layout_with_kk,
     main                = "Red de caletas",
     sub                 = "¡Atención! Red meramente hipotética",
     edge.curved         = TRUE,
     vertex.label.cex    = 1,
     vertex.label.dist   = -1,
     vertex.size         = 4,
     ylim                = c(-1, 1),
     xlim                = c(-1, 1),
     asp                 = 0
)

##### Detección de comunidades no-biológicas mediante optimización de la matriz de modularidad y vectores propios (eigenvector) no negativos ####
# Esta función trata de encontrar subgráficos densamente conectados en un gráfico calculando el vector propio no negativo principal de la matriz de modularidad del gráfico.
ec <- cluster_leading_eigen(g4)
plot(ec,
     g4,
     layout              = layout_with_kk,
     main                = "Red de caletas",
     sub                 = "¡Atención! Red meramente hipotética",
     edge.curved         = TRUE,
     vertex.label.cex    = 1,
     vertex.label.dist   = -1,
     vertex.size         = 4,
     ylim                = c(-1, 1),
     xlim                = c(-1, 1),
     asp                 = 0
)

##### Detección de comunidades no-biológicas mediante intermediación de arcos y agrupación jerárquica ####
bc <- cluster_edge_betweenness(g4)
plot(bc,
     g4,
     layout              = layout_with_kk,
     main                = "Red de caletas",
     sub                 = "¡Atención! Red meramente hipotética",
     edge.curved         = TRUE,
     vertex.label.cex    = 1,
     vertex.label.dist   = -1,
     vertex.size         = 4,
     ylim                = c(-1, 1),
     xlim                = c(-1, 1),
     asp                 = 0
)

##### Detección de comunidades no-biológicas mediante paseos aleatorios y el flujo actual de la red ####
ic <- cluster_infomap(g4)
plot(ic,
     g4,
     layout              = layout_with_kk,
     main                = "Red de caletas",
     sub                 = "¡Atención! Red meramente hipotética",
     edge.curved         = TRUE,
     vertex.label.cex    = 1,
     vertex.label.dist   = -1,
     vertex.size         = 4,
     ylim                = c(-1, 1),
     xlim                = c(-1, 1),
     asp                 = 0
)

##### Detección de comunidades no-biológicas mediante spinglass ####
# Este método está basado en física, es un poco enrevesado:
# El modelo de vidrio de espín (spinglass) considera los enlaces de la red como acoplamientos atractivos y repulsivos, como el sistema de spinglass magnético en física. El modelo busca los espines y toma los eslabones perdidos como acoplamientos repulsivos. Los nodos densamente conectados se alinean en la misma dirección, mientras que los grupos de espines diferentes y vagamente interconectados se orientan en diferentes direcciones. verdaderamente modular
sc <- cluster_spinglass(g4)
plot(sc,
     g4,
     layout              = layout_with_kk,
     main                = "Red de caletas",
     sub                 = "¡Atención! Red meramente hipotética",
     edge.curved         = TRUE,
     vertex.label.cex    = 1,
     vertex.label.dist   = -1,
     vertex.size         = 4,
     ylim                = c(-1, 1),
     xlim                = c(-1, 1),
     asp                 = 0
)

##### Detección de comunidades no-biológicas mediante asignación de los nodos al mismo grupo que sus vecinos mas próximos ####
lpc <- cluster_label_prop(g4)
plot(lpc,
     g4,
     layout              = layout_with_kk,
     main                = "Red de caletas",
     sub                 = "¡Atención! Red meramente hipotética",
     edge.curved         = TRUE,
     vertex.label.cex    = 1,
     vertex.label.dist   = -1,
     vertex.size         = 4,
     ylim                = c(-1, 1),
     xlim                = c(-1, 1),
     asp                 = 0
)