# JORGE ESCALERA_Trabajo.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)
datos <- read.csv("datos_biomed.csv", header = TRUE, sep = ",")

# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)
head(datos)
summary(datos)
dim(datos)
str(datos)
n_vars <- ncol(datos)
n_trat <- length(unique(datos$Tratamiento))
n_vars
n_trat

#hay 5 variables; ID, tratamiento, glucosa, presión y colesterol
#hay 3 tratamientos, Farmacos A y B, y un placebo

# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)
datos_long <- datos %>%
  pivot_longer(cols = c(Glucosa, Presion, Colesterol),
               names_to = "Variable",
               values_to = "Valor")
ggplot(datos_long, aes(x = Tratamiento, y = Valor)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free") +
  theme_bw() +
  labs(title = "Boxplot de los parámetros según el tratamiento", x = "Tratamiento", y = "Valor")


# 4. Realiza un violin plot (investiga qué es). (1 pt)
library(ggplot2)
library(tidyr)
library(dplyr)
datos_long <- datos %>%
  pivot_longer(cols = -Tratamiento, names_to = "Variable", values_to = "Valor") %>%
  filter(Variable != "ID")
ggplot(datos_long, aes(x = Tratamiento, y = Valor, fill = Tratamiento)) +
  geom_violin(trim = FALSE, alpha = 0.7) +       
  geom_boxplot(width = 0.1, color = "black") +   
  facet_wrap(~Variable, scales = "free") +       
  labs(
    title = "Violin plot de variables biomédicas por tratamiento",
    x = "Tratamiento",
    y = "Valor"
  ) +
  theme_minimal()

# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)

plot(datos$Glucosa, datos$Presion,
     xlab = "Glucosa",
     ylab = "Presión",
     col = as.factor(datos$Tratamiento), 
     pch = 16)

legend("bottomright",
       legend = levels(as.factor(datos$Tratamiento)),
       col = 1:length(levels(as.factor(datos$Tratamiento))),
       pch = 16)

# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)

library(ggplot2)

ggplot(datos, aes(x = Presion, y = Colesterol)) +
  geom_point() +
  facet_grid(. ~ Tratamiento) +  
  theme_minimal()
# 7. Realiza un histogramas para cada variable. (0.5 pts)

hist(datos$Glucosa, main="Histograma de Glucosa", xlab="Glucosa", col="skyblue")
hist(datos$Presion, main="Histograma de Presión", xlab="Presión", col="salmon")
hist(datos$Colesterol, main="Histograma de Colesterol", xlab="Colesterol", col="lightgreen")

# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)

datos$Tratamiento <- factor(datos$Tratamiento)
levels(datos$Tratamiento)

# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)

aggregate(Glucosa ~ Tratamiento, data=datos, FUN=mean)
aggregate(Glucosa ~ Tratamiento, data=datos, FUN=sd)

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)
placebo <- subset(datos, Tratamiento == "Placebo")
tratamiento1 <- subset(datos, Tratamiento == "FarmacoA")
tratamiento2 <- subset(datos, Tratamiento == "FarmacoB")

# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)
variables <- c("Glucosa", "Presion", "Colesterol")
for (v in variables) {
  cat("\nShapiro-Wilk para", v, "\n")
  print(by(datos[[v]], datos$Tratamiento, function(x) shapiro.test(na.omit(x))$p.value))
}
anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos)
summary(anova_glucosa)

anova_presion <- aov(Presion ~ Tratamiento, data = datos)
summary(anova_presion)

anova_colesterol <- aov(Colesterol ~ Tratamiento, data = datos)
summary(anova_colesterol)
n_trats <- length(levels(datos$Tratamiento))
if(n_trats == 2) {
  grupos <- levels(datos$Tratamiento)
  g1 <- subset(datos, Tratamiento == grupos[1])$Glucosa
  g2 <- subset(datos, Tratamiento == grupos[2])$Glucosa
  t.test(g1, g2)
} else {
  anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos)
  summary(anova_glucosa)
  TukeyHSD(anova_glucosa)
  kruskal.test(Glucosa ~ Tratamiento, data = datos)
}

# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)

anova_glucosa <- aov(Glucosa ~ Tratamiento, data=datos)
summary(anova_glucosa)

