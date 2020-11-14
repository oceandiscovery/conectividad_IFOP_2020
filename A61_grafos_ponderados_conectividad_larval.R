#---------------------------------------------------------------------------
## II Curso Internacional "Uso de la teoría de grafos en la evaluación de la
## conectividad larval de poblaciones marinas"
## contacto: Andres Ospina-Alvarez (aospina.co@me.com)
## Organizadores(as) del curso: Catherine González (catherine.gonzalez@ifop.cl)
#---------------------------------------------------------------------------

# Título: Calcular diferentes medidas de centralidad para un grafo ponderado y representarlas gráficamente

rm(list = ls()) # Limpiar todos los objetos y variables en el entorno de trabajo

# ------------------ #
#Normalize function
# ------------------ #
normalize_fun <- function(x) {
  a <- min(x)
  b <- max(x)
  (x - a) / (b - a)
}

library(readxl)
library(igraph) # Cargar la librería igraph
library(ggraph)

g5.data <-
  read_excel("A61_caletas.xlsx") # Leer una matriz de adyacencia almacenada en un archivo MS Excel
head(g5.data)

# Aquí creamos un grafo a partir de la matriz de adyacencia
g5.empty.graph <-
  graph_from_adjacency_matrix(as.matrix(g5.data), weighted=TRUE, mode = c("directed"))

### Crear una lista y poner g5.empty.graph ####
gs <- list()
gs[[1]] <- g5.empty.graph

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

g5 <- gs[[1]]

