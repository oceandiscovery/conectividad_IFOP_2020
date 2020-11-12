#---------------------------------------------------------------------------
## II Curso Internacional "Uso de la teoría de grafos en la evaluación de la
## conectividad larval de poblaciones marinas"
## contacto: Andres Ospina-Alvarez (aospina.co@me.com)
## Organizadores(as) del curso: Catherine González (catherine.gonzalez@ifop.cl)
#---------------------------------------------------------------------------

# Título: Núcleos de dispersión, manejo y representación gráfica.

# Vamos a establecer una semilla del generador de numeros aleatorios que nos permitirá reproducibilidad cuando trabajemos con simulaciones.
set.seed(1234)

# En la siguiente línea vamos a crear el objeto data.sim que simula ser un conjunto de datos con 400 entradas, con una distribución normal, una media de 400 y desviación estandar de 12.
# Asumiremos que este conjunto de datos representa distancias de dispersión para una sp. X
data.sim <- round(rnorm(n = 400, mean = 40, sd = 12))
# Revisemos nuestro conjunto de datos, si el valor mínimo es negativo es mejor que volvamos a generar un conjunto de datos diferente, pues no es muy intuitivo trabajar con distancias negativas.
summary(data.sim)

# Ahora estimaremos un núcleo de densidad para nuestra conjunto de datos simulado. El método por defecto lo hace con el núcleo y el ancho de banda especificado para observaciones univariantes.
dens <-
  density(
    data.sim,
    from = 0,
    to = 80,
    width = 7,
    kernel = "gaussian"
  )

# Y ahora generamos una figura de ese kernel de dispersión.
plot(dens)
# Tema de discusión: ¿Cómo podemos interpretar y usar esta figura?

# Si integramos el área bajo la curva de densidad podemos obtener la probabilidad de que la sp. X se disperse una distancia con un mínimo "l" y un máximo "u". Pongámoslo en práctica.
integrate(approxfun(dens), lower = 35, upper = 45)

# Probablemente nos interesará mas el presentar esta información como un histograma de frecuencias. Vamos a usar la librería ggplot2
library(ggplot2)

# ggplot necesita como entrada objetos del tipo dataframe
# ¿Es nuestro objeto data.sim un dataframe)
is.data.frame(data.sim)
class(data.sim)

# Crearemos un objeto dataframe "df.0" a partir de nuestro objeto data.sim
df.0 <- as.data.frame(data.sim)

##### Figura con histograma de frecuencias y kernel de dispersión ####
# Y ahora creamos nuestro histograma de frecuencias, pero además mantendremos nuestro kernel de dispersión, todo en la misma figura.
ggplot(df.0, aes(data.sim, colour = 1, fill = 1)) +
  geom_histogram(
    aes(y = 2 * (..density..) / sum(..density..)),
    breaks = seq(0, 75, 4),
    alpha = 0.5,
    position = "identity",
    lwd = 0.2
  ) +
  geom_density(aes(y = 6 * (..density..)),
               alpha = .1,
               colour = 2,
               fill = 2) +
  scale_x_continuous(breaks = seq(from = 0, to = 90, by = 10)) +
  scale_y_continuous(
    labels = scales::percent,
    sec.axis = sec_axis(
      trans = ~ . / 6,
      name = "densidad",
      breaks = seq(from = 0, to = 0.1, by = 0.01)
    )
  ) +
  # ggtitle("Kernel density and probability estimation for the\n distance from natal origin") +
  xlab("Distancia (km)") + ylab("probabilidad (%)") +
  theme_classic(base_size = 10) +
  theme(legend.position = "") +
  theme(
    axis.line.y.right = element_line(color = 2),
    axis.ticks.y.right = element_line(color = 2),
    axis.text.y.right = element_text(color = 2),
    axis.title.y.right = element_text(color = 2)
  )

##### Comparando el kernel de dispersión para dos especies (loco y lapa) ####

# Vamos a crear un dataframe de datos simulados, 200 entradas para loco y 200 entradas para lapa. Las primeras con media 50 km y desviación estandar 10 km y las segundas con media 20 km y desviación estandar 6 km.
df <- data.frame(spp = factor(rep(c("Loco", "Lapa"), each = 200)),
                 distance = round(c(
                   rnorm(200, mean = 50, sd = 10),
                   rnorm(200, mean = 20, sd = 6)
                 )))
summary(df)

# Y ahora de manera similar a la figura anterior haremos una figura con el histograma de frecuencia y el kernel de dispersión para cada una de las dos especies
ggplot(df, aes(distance, colour = spp, fill = spp)) +
  geom_histogram(aes(y = ..density..), alpha = 0.5,
                 position = "identity") +
  geom_density(alpha = .2) +
  ggtitle("Kernel density and probability estimation for the\n distance from natal origin") +
  xlab("Distance (km)") + ylab("Density") +
  scale_x_continuous(breaks = seq(from = 0, to = 80, by = 10)) +
  theme_bw()

# He dejado el eje secundario fuera de la figura anterior a propósito, solo para que podais comparar las dos instruciones y poder entender un poco mejor el código necesario para obtener ese segundo eje.

# Comparando el kernel de dispersión para loco y lapa
ggplot(df, aes(distance, colour = spp, fill = spp)) +
  geom_histogram(
    aes(y = 2 * (..density..) / sum(..density..)),
    breaks = seq(0, 90, 5),
    alpha = 0.5,
    position = "identity",
    lwd = 0.2
  ) +
  geom_density(aes(y = 4 * (..density..)), alpha = .2, color = "darkgreen") +
  scale_x_continuous(breaks = seq(from = 0, to = 80, by = 10)) +
  scale_y_continuous(
    labels = scales::percent,
    sec.axis = sec_axis(
      trans = ~ . / 4,
      name = "density",
      breaks =
        seq(from = 0, to = 0.1, by = 0.01)
    )
  ) +
  ggtitle("Kernel density and probability estimation for the\n distance from natal origin") +
  xlab("Distance (km)") + ylab("probability (%)") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(legend.direction = "horizontal") +
  theme(
    axis.line.y.right = element_line(color = "darkgreen"),
    axis.ticks.y.right = element_line(color = "darkgreen"),
    axis.text.y.right = element_text(color = "darkgreen"),
    axis.title.y.right = element_text(color = "darkgreen")
  )

# Tema de discusión: Si esta figura estuviese basada en datos reales ¿Qué conclusiones podríamos generar para la gestión de los recursos?