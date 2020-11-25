#---------------------------------------------------------------------------
## II Curso Internacional "Uso de la teoría de grafos en la evaluación de la
## conectividad larval de poblaciones marinas"
## contacto: Andres Ospina-Alvarez (aospina.co@me.com)
## Organizadores(as) del curso: Catherine González (catherine.gonzalez@ifop.cl)
#---------------------------------------------------------------------------

rm(list = ls()) # Limpiar todos los objetos y variables en el entorno de trabajo

library(igraph)

# Note: if you do not already installed it, install it with:
# install.packages("leaflet")
library(leaflet)

# Background 1: NASA
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -70, lat = -31, zoom = 5) %>%
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")
m

# Background 2: World Imagery
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -70, lat = -31, zoom = 3) %>%
  addProviderTiles("Esri.WorldImagery")
m


# Background 3: Topo Map
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -70,
          lat = -31,
          zoom = 4) %>%
  addProviderTiles("Esri.WorldTopoMap")
m

# Background 4: Wikimedia
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -70,
          lat = -31,
          zoom = 4) %>%
  addProviderTiles("Wikimedia")
m


#### LOAD DATA ####
g <- readRDS("A81_grafo_g.rds")

# Prepare the text for the tooltip:
mytext <- paste(
  "Zone: ",
  V(g)$name,
  "<br/>",
  "degree: ",
  V(g)$degree,
  "<br/>",
  "in degree: ",
  V(g)$indegree,
  "<br/>",
  "out degree: ",
  V(g)$outdegree,
  "<br/>",
  "norm. strength: ",
  round(V(g)$n.strength, digits = 2),
  "<br/>",
  "norm. in-strength: ",
  round(V(g)$n.in_strength, digits = 2),
  "<br/>",
  "norm. out-strength: ",
  round(V(g)$n.out_strength, digits = 2),
  "<br/>",
  "norm. betweenness: ",
  round(V(g)$n.betweenness, digits = 2),
  sep = ""
) %>%
  lapply(htmltools::HTML)

g.edgelist <- as.data.frame(as_edgelist(g))

# # Ahora vamos a asignar los datos de latitud y longitud automáticamente
# join_pos <- readRDS("geocode_zones.rds")
# g.edgelist$lat_ini <- NaN
# g.edgelist$lon_ini <- NaN
# g.edgelist$lat_end <- NaN
# g.edgelist$lon_end <- NaN
# # Usamos un bucle
# for (i in 1:nrow(g.edgelist)){
#   g.edgelist$lat_ini[i]   <- join_pos[g.edgelist$V1[i],2]
#   g.edgelist$lon_ini[i]   <- join_pos[g.edgelist$V1[i],1]
#   g.edgelist$lat_end[i]   <- join_pos[g.edgelist$V2[i],2]
#   g.edgelist$lon_end[i]   <- join_pos[g.edgelist$V2[i],1]
# }

mytext_2 <- paste(
  paste("from", g.edgelist$V1, "to", g.edgelist$V2, sep = " "),
  "<br/>",
  "Weight: ",
  round(E(g)$weight, digits = 2),
  "<br/>",
  "norm. edge betweenness: ",
  round(E(g)$n.edge_betweenness, digits = 2),
  sep = ""
) %>%
  lapply(htmltools::HTML)


##### Organizando el grafo para representarlo en leafleft ####
library(sp)

V(g)$lon <- V(g)$x
V(g)$lat <- V(g)$y

gg <- get.data.frame(g, "both")
vert <- gg$vertices
coordinates(vert) <- ~lon+lat

edges <- gg$edges

edges <- lapply(1:nrow(edges), function(i) {
  as(rbind(vert[vert$name == edges[i, "from"], ], 
           vert[vert$name == edges[i, "to"], ]), 
     "SpatialLines")
})


for (i in seq_along(edges)) {
  edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
}

edges <- do.call(rbind, edges)


##### Create a color palette with handmade bins ####
mypalette <-
  colorNumeric("RdYlBu", 0:4200, na.color = "#808080", alpha = FALSE,
             reverse = TRUE)

##### Plot the interactive map ####
mg <- leaflet(vert) %>%
  addTiles()  %>%
  addProviderTiles("Esri.WorldTerrain", group = "ESRI Terrain") %>%
  addProviderTiles("Esri.WorldTopoMap", group = "ESRI Topo") %>%
  addProviderTiles("Esri.OceanBasemap", group = "ESRI Ocean") %>%
  addProviderTiles("CartoDB.Positron", group = "Gray") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreet") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner") %>%
  setView(lat = -34,
          lng = -72 ,
          zoom = 6) %>%
  addGraticule(interval = 2,
               style = list(color = "#FF0000", weight = 1)) %>%
  # addPolylines(data = edges,
  #              color = "blueviolet",
  #              weight = 0.02,
  #              opacity = 0.1,
  #              fillOpacity = 0.1,
  #              label = mytext_2,
  #              labelOptions = labelOptions(
  #                style = list("font-weight" = "normal", padding = "3px 8px"),
  #                textsize = "13px",
  #                direction = "auto"),
  #              group = "arcs"
  # ) %>%
addPolylines(data = edges,
             color = "blueviolet",
             weight = 0.02,
             opacity = 0.1,
             fillOpacity = 0.1,
             label = mytext_2,
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px"),
               textsize = "13px",
               direction = "auto"),
             group = "arcs"
) %>%
  addCircleMarkers(data = vert,
                   radius = ~3+8^V(g)$n.betweenness,
                   fillColor = ~ mypalette(betweenness),
                   fillOpacity = 0.7,
                   color = "white",
                   stroke = FALSE,
                   label = mytext,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "13px",
                     direction = "auto"
                   )
                   ) %>%
  addScaleBar() %>%
  addMiniMap() %>%
  addLabelOnlyMarkers(
    lng = ~ x - 0.01,
    lat = ~ y,
    label = ~ name,
    labelOptions = labelOptions(
      style = list(color = "deepskyblue"),
      noHide = T,
      textOnly = TRUE,
      direction = "left"
    ),
    group = "names"
  ) %>%
  addLayersControl(
    baseGroups = c("ESRI Terrain", "ESRI Topo", "ESRI Ocean", "Gray", "OpenStreet", "Toner"),
    overlayGroups = c("names", "arcs"),
    options = layersControlOptions(collapsed = F)
  ) %>%
  hideGroup(c("names", "arcs"))

mg


### http://leaflet-extras.github.io/leaflet-providers/preview/index.html


