#---------------------------------------------------------------------------
## II Curso Internacional "Uso de la teoría de grafos en la evaluación de la
## conectividad larval de poblaciones marinas"
## contacto: Andres Ospina-Alvarez (aospina.co@me.com)
## Organizadores(as) del curso: Catherine González (catherine.gonzalez@ifop.cl)
#---------------------------------------------------------------------------

# Un recordatorio de R


# Limpiar todos los objetos y variables en el entorno de trabajo
rm(list = ls()) 

# Cargar datos
motorcars <- read.csv("A00_motorcars.csv", stringsAsFactors = FALSE)

# Ver nombre de las columnas de la tabla de datos
names (motorcars)

# Resumen de los datos
summary(motorcars)
# Este comando muestra: valor minimo, 1r y 3r cuantil, mediana, media, y valor maximo 

mean (motorcars$mpg) # media
sd (motorcars$mpg) # Desviacion estandard
min (motorcars$mpg) # valor minimo
max (motorcars$mpg) # valor maximo
median (motorcars$mpg) # mediana
range (motorcars$mpg) # Rango (min, max)
quantile (motorcars$mpg) # cuantiles
var(motorcars$mpg)

cor(motorcars$mpg,motorcars$hp, method = "spearman")

#### Grafico de dispersion con lineas de tendencia ####
plot(x = motorcars$mpg, y = motorcars$hp, main = "Fuerza vs. MPG", xlab = "Caballos de fuerza", ylab = "Millas por Galón")
# Un gráfico sencillo para visualizar dos variables con posible relación

# Un modelo lineal para probar la hipótesis nula de la dependencia de los caballos de fuerza de un motor y el consumo medio por milla recorrida
modelo.lineal.1 <- lm(formula = mpg ~ hp, data = motorcars)
summary(modelo.lineal.1)
# Caracteristicas del modelo, con estos datos se puede escribir la formula y ver el R cuadrado

# Recordando que una anova es un tipo de modelo lineal
anova.1 <- aov(formula = mpg ~ hp, data = motorcars, projections = TRUE, qr = TRUE)
anova.1
summary(anova.1)

par(mfrow = c(2,2)) # 4 figuras en un mismo panel de 2 x 2
plot(anova.1)

par(mfrow = c(1,2)) # 4 figuras en un mismo panel de 2 x 2
# el boxplot representa la mediana y las barras de error
boxplot(mpg~as.factor(cyl), data = motorcars)
# Si las muescas de dos cajas no se superponen, esto es una "prueba contundente" de que las dos medianas difieren (Chambers et al, 1983, p. 62)
boxplot(mpg~as.factor(cyl), data = motorcars, notch = TRUE,  col = "blue", outline = FALSE) 


par(mfrow = c(1,3)) # Tres figuras en un mismo panel
hist(motorcars$mpg, main = "MPG", xlab = "Millas por Galón")
hist(motorcars$hp, main = "Caballos de fuerza", xlab = "Caballos de fuerza")
plot(x = motorcars$mpg, y = motorcars$hp, main = "Fuerza vs. MPG", xlab = "Caballos de fuerza", ylab = "Millas por Galón")

# Cómo cargar librerias?
library(plyr)
# Usemos plyr para agrupar y manipular los datos originales 
ddply(motorcars, .(cyl, gear), summarise, mean = mean(mpg),se = sd(mpg) / sqrt(length(mpg))) 


# Cómo hacer figuras mas atractivas? Usando ggplot2
library(ggplot2)
ggplot(motorcars, aes(x=factor(cyl), y= mpg , fill = factor(gear))) + 
  geom_violin() +
  geom_boxplot(width=.1, fill="lightgrey", outlier.colour=NA) + stat_summary(fun=median, geom="point", fill="white", shape=21, size=2.5)+
  xlab("Cilindros") +
  ylab("MPG") +
  guides(fill = guide_legend("Marchas")) +
  theme_light()


# Cómo usar "pipes"?
library(dplyr)
# Usamos pipes para agilizar la escritura del código
motorcars %>% group_by(cyl, gear) %>% summarise(mean = mean(mpg))

# Crear un nuevo set de datos
new.data <- motorcars %>% group_by(cyl, gear) %>% summarise(mean = mean(mpg),
                                                            se = sd(mpg) / sqrt(length(mpg)))

# verificando que tipo de objeto es:
class(new.data)
# convirtiendo a un uevo tio de objeto:
new.data <- as.data.frame(new.data)
class(new.data)

# Usando el nuevo set de datos para generar una nueva figura
ggplot(new.data, aes(x=factor(gear), y= mean , fill = factor(cyl))) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use barras con contorno negro,
           size=.3) +     # Usar bordes delgados para las barras
  geom_errorbar(aes(ymin= mean -se, ymax= mean +se),
                size=.3,    # Usar barras de error delgadas
                width=.2,
                position=position_dodge(.9)) +
  xlab("Marchas") +
  ylab("MPG") +
  guides(fill = guide_legend("Cilindros")) +
  theme_light()



#### Instalando paquetes o librerias nuevas
# install.packages("corrplot")

# Usando corrplot para hacer una matriz de correlaciones
library(corrplot)

mcor <- cor(motorcars[2:12])
# Print mcor and round to 2 digits
round(mcor, digits=2)

library(corrplot)
par(mfrow = c(1,1))
corrplot(mcor)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black", tl.srt=45)
