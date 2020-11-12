#---------------------------------------------------------------------------
## II Curso Internacional "Uso de la teoría de grafos en la evaluación de la
## conectividad larval de poblaciones marinas"
## contacto: Andres Ospina-Alvarez (aospina.co@me.com)
## Organizadores(as) del curso: Catherine González (catherine.gonzalez@ifop.cl)
#---------------------------------------------------------------------------

# Título: Agrupación jerárquica y su representación como un dendrograma circular

# Nota 1: Este es un script experimental, aún lo estoy desarrollando, pero lo presento aquí por si llama la atención de alguno de vosotros para algún conjunto de datos específico que quieran ilustrar de esta manera.

# Nota 2: Este script no está comentado en castellano, incluso muchas secciones carecen de comentarios.

rm(list = ls()) # Limpiar todos los objetos y variables en el entorno de trabajo

#------------------
#Loading libraries
#------------------
library(igraph) # Cargar la librería igraph
library(readxl)
library(ggraph)
library(ape)
library(ggtree)
library(clustsig)
library(RColorBrewer)
library(dplyr)

# Hexadecimal color specification
brewer.pal(n = 10, name = "Paired")
display.brewer.pal(n = 10, name = 'Paired')

refcolours <- c(
  "DarkPurple"   = "#6A3D9A",
  "LihtgPurple"  = "#CAB2D6",
  "DarkRed"      = "#E31A1C",
  "LihtgRed"     = "#FB9A99",
  "DarkOrange"   = "#FF7F00",
  "LihtgOrange"  = "#FDBF6F",
  "DarkBlue"     = "#1F78B4",
  "LihtgBlue"    = "#A6CEE3",
  "DarkGreen"    = "#33A02C",
  "LihtgGreen"   = "#B2DF8A"
)

refcolours <- c(
  "Black"          = "#000000",
  "Dark Charcoal"  = "#333333",
  "Granite Gray"   = "#666666",
  "Spanish Gray"   = "#999999",
  "Chinese Silver" =  "#CCCCCC",
  "Bright Gray"    = "#EEEEEE"
)



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

g5 <-
  graph_from_adjacency_matrix(as.matrix(g4.data), mode = c("undirected"))

#---------------------------------------------
#### Making a circular hierarchical dendrogram
#---------------------------------------------
number_of_clusters <- 4
dgr <- as.matrix(degree(g5)) #Calculate degree stats
calc_n_nodes <-
  sort(dgr, decreasing = TRUE)[1:35] #Sort by degree
threshold_degree <- calc_n_nodes[length(calc_n_nodes)]
web1 <-
  induced.subgraph(g5, which(dgr >= threshold_degree)[1:35],
                   impl = "create_from_scratch")
web1 <- simplify(web1)

ys <- list()
ys[[1]] <- web1

ys <- lapply(ys, function(x) {
  # Centrality measures
  V(x)$degree         <- degree(x, mode = "total")
  V(x)$indegree       <- degree(x, mode = "in")
  V(x)$outdegree      <- degree(x, mode = "out")
  V(x)$betweenness    <-
    betweenness(x, normalized = TRUE) # Aquí se calcula betweenness respecto al costo
  V(x)$betweenness.strength <-
    betweenness(x, weights = E(x)$strength) # Aquí se calcula betweenness respecto a la fuerza
  # V(x)$closeness    <- closeness(x)
  E(x)$betweenness    <-
    edge.betweenness(x) # Aquí se calcula E. betweenness respecto al costo
  E(x)$betweenness.strength <-
    edge.betweenness(x, weights = E(x)$strength) # Aquí se calcula E. betweenness respecto a la fuerza
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
  
  #-------------------------------
  #Making a cluster dendrogram fit
  #-------------------------------
  adj.matrix <- as_adjacency_matrix(x)
  dis.matrix <-
    dist(as.matrix(adj.matrix[,]), method = "euclidean")
  res <-
    simprof(
      data = dis.matrix,
      num.expected = 1000,
      num.simulated = 999,
      method.cluster = "ward.D2",
      method.distance = "euclidean"
    )
  H.fit <- res[["hclust"]]
  groups <-
    cutree(H.fit, k = number_of_clusters) # cut tree into X clusters
  
  x$H.fit <- H.fit
  
  #-------------------------------------------
  # Assign membership using cluster dendrogram
  #-------------------------------------------
  V(x)$memb_hclust        <- as.numeric(groups)
  
  return(x)
})

vstats <- do.call('rbind', lapply(1:length(ys), function(x) {
  o <- get.data.frame(ys[[x]], what = 'vertices')
  o$network <- get.graph.attribute(ys[[x]], "name")
  o$time <- x
  return(o)
}))

mycolours <- c(
  "DarkPurple"     = "#6A3D9A",
  "LihtgPurple"    = "#CAB2D6",
  "DarkRed"        = "#E31A1C",
  "LihtgRed"       = "#FB9A99",
  "DarkOrange"     = "#FF7F00",
  "LihtgOrange"    = "#FDBF6F",
  "DarkBlue"       = "#1F78B4",
  "LihtgBlue"      = "#A6CEE3",
  "DarkGreen"      = "#33A02C",
  "LihtgGreen"     = "#B2DF8A",
  "Dark Charcoal"  = "#333333",
  "Granite Gray"   = "#666666",
  "Spanish Gray"   = "#999999",
  "Chinese Silver" =  "#CCCCCC"
)

names(mycolours) <-
  unique(vstats[order(vstats$degree, decreasing = TRUE), ]$memb_hclust)

hs <- ys[1]

lapply(hs, function(x) {
  # Convert network graph to phylo tree
  tree <- as.phylo(x$H.fit)
  # Extract nodes and group membership from hcluster
  nodes <- igraph::as_data_frame(x, what = "vertices")
  
  # Make group info avalaible to phylo tree
  groupInfo <-
    split(nodes$name, gsub("_\\w+", "", nodes$memb_hclust))
  tree <- groupOTU(tree, groupInfo)
  
  # Transform phylo tree to igraph and then to edgelist
  int_graph <- as.igraph(tree)
  edges = as_edgelist(int_graph, names = TRUE)
  colnames(edges) <- c("from", "to")
  edges <- as.data.frame(edges)
  
  # create a dataframe with connection between leaves (individuals)
  connect <- igraph::as_data_frame(x, what = "edges")
  colnames(connect)  <- c("from", "to", "value")
  
  # create a vertices data.frame. One line per object of our hierarchy
  vertices = data.frame(name = unique(c(
    as.character(edges$from), as.character(edges$to)
  )),
  degree = 0,
  betwee = 0)
  
  # Import the degree value for the leaf nodes
  for (i in 1:nrow(nodes)) {
    for (j in 1:nrow(vertices)) {
      if (nodes[i, 1] == vertices[j, 1]) {
        vertices[j, 2] <- nodes[i, which(colnames(nodes) == "degree")]
      }
    }
  }
  
  # Import the betweenness value for the leaf nodes
  for (i in 1:nrow(nodes)) {
    for (j in 1:nrow(vertices)) {
      if (nodes[i, 1] == vertices[j, 1]) {
        vertices[j, 3] <- nodes[i, which(colnames(nodes) == "betweenness")]
      }
    }
  }
  
  # Import the group membership value for the leaf nodes
  vertices$group <- 1 # dummy value
  # Let's add a column with the group of each name. It will be useful later to color points
  for (i in 1:nrow(nodes)) {
    for (j in 1:nrow(vertices)) {
      if (nodes[i, 1] == vertices[j, 1]) {
        vertices[j, 4] <- nodes[i, which(colnames(nodes) == "memb_hclust")]
      }
    }
  }
  vertices$group <- as.factor(vertices$group)
  
  #Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
  #calculate the ANGLE of the labels
  vertices$id = NA
  myleaves = which(is.na(match(vertices$name, edges$from)))
  nleaves = length(myleaves)
  vertices$id[myleaves] = seq(1:nleaves)
  vertices$hjust <- 0 # dummy value
  vertices$angle = -90 - 360 * vertices$id / nleaves
  
  # calculate the alignment of labels: right or left
  vertices$hjust <-
    ifelse(vertices$angle <= -190 & vertices$angle >= -350, 1, 0)
  
  # flip angle BY to make them readable
  vertices$angle <-
    ifelse(
      vertices$angle <= -190 &
        vertices$angle >= -350,
      vertices$angle - 90,
      vertices$angle + 90
    )
  
  vertices <- arrange(vertices, id)
  
  # Create a graph object
  mygraph <- graph_from_data_frame(edges, vertices = vertices)
  
  # The connection object must refer to the ids of the leaves:
  from = match(connect$from, vertices$name)
  to = match(connect$to, vertices$name)
  
  # Make a dendrogram plot with edge strength
  ggraph(mygraph, layout = 'dendrogram', circular = TRUE) +
    geom_conn_bundle(
      data = get_con(from = from, to = to),
      alpha = 0.1,
      width = 0.5,
      aes(colour = ..index..)
    ) +
    scale_edge_colour_distiller(palette = "RdPu") +
    geom_node_text(
      aes(
        x = x * 1.15,
        y = y * 1.15,
        filter = leaf,
        label = name,
        angle = angle - 150,
        hjust = hjust,
        colour = group
      ),
      size = 6,
      alpha = 1
    ) +
    geom_node_point(aes(
      filter = leaf,
      x = x * 1.07,
      y = y * 1.07,
      colour = group,
      size = degree,
      alpha = 0.2
    )) +
    
    scale_color_manual(values = mycolours) +
    scale_size_continuous(range = c(0.1, 7)) +
    theme_void() +
    theme_graph(
      plot_margin = margin(1, 1, 1, 1),
      base_family = "Arial Narrow",
      base_size = 20,
      background = "white"
    ) +
    theme(legend.position = "none",
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    expand_limits(x = c(-2.1, 2.1), y = c(-2.1, 2.1))
  
  #--------------------------
  #Saving previous graph plot
  #--------------------------
  # filename <- "clusters.v1.png"
  # ggsave(
  #   filename,
  #   width = 70,
  #   height = 70,
  #   units = "cm",
  #   dpi = 300
  # )
  
})
