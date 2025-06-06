# Ejercicio: Modelos lineales aplicados a ecología de humedales
# En un estudio de humedales altoandinos, se registró la abundancia de
# flamencos en función de la proporción de algas diatomeas en muestras de agua.
# El muestreo se realizó en 5 humedales distintos, con 10 registros por humedal.
# Usando el archivo datos_flamencos.csv, realizá los siguientes pasos:
#  a.	Exploración inicial: Visualizá la relación entre la proporción de algas y 
# la abundancia de flamencos usando plot(). ¿La relación parece lineal?
#  b.	Modelo lineal simple (lm()): Ajustá un modelo lineal. Evaluá los supuestos
# (normalidad y homogeneidad de varianzas). ¿Son adecuados estos supuestos para
# conteos?
#  c.	Modelo Poisson (glm(family = "poisson")): Ajustá un modelo con 
# distribución de Poisson. Evaluá los supuestos y si hay sobredispersión.
#  d.	Modelo mixto Poisson (glmmTMB(family = poisson)): Ajustá un modelo con 
# efecto aleatorio de humedal. 
#  e.	Compará los tres modelos usando AIC() ¿Cúal es el mejor?
#  f.	Visualización: Graficá los resultados para el mejor modelo. Si el mejor
# modelo es un GLMM ajusta la curva general. 
#  g.	Ejercicio extra optativo: Si el mejor modelo es un GLMM graficá las curvas
# para cada humedal.

# Cargar librerías necesarias
library(ggplot2)
library(glmmTMB)
library(DHARMa)

# Cargar datos
path <- "./data/datos_flamencos.csv"
datos <- read.table(path, header=T, dec = ".", sep = ",")
datos$humedal <- as.factor(datos$humedal)

# 1. Exploración inicial --------------------------------------------------
# Graficar relación entre proporción de algas y flamencos
plot(flamencos~prop_algas, data = datos)

# 2. Modelo lineal simple --------------------------------------------------
modelo_lm <- lm(flamencos ~ prop_algas, data = datos)
summary(modelo_lm)

# Evaluación de supuestos
par(mfrow = c(2, 2))
plot(modelo_lm)
par(mfrow = c(1, 1))


# 3. Modelo Poisson (glm) --------------------------------------------------
modelo_glm <- glm(flamencos ~ prop_algas, family = poisson, data = datos)
summary(modelo_glm)

# Evaluar supuestos y sobredispersión con DHARMa
res_glm <- simulateResiduals(modelo_glm)
plot(res_glm)

testDispersion(res_glm)
# 4. Modelo mixto (glmmTMB)--------------------------------------------------
modelo_glmm <- glmmTMB(flamencos ~ prop_algas + (1 | humedal), family = poisson, data = datos)
summary(modelo_glmm)

# Diagnóstico
res_glmm <- simulateResiduals(modelo_glmm)
plot(res_glmm)


# 5. Comparación de modelos--------------------------------------------------
AIC(modelo_lm, modelo_glm, modelo_glmm)


# 6. Visualización de predicciones --------------------------------------------------

# Predicciones sin efecto aleatorio

algas <- data.frame(
  prop_algas = seq(min(datos$prop_algas), max(datos$prop_algas),
                       length.out = 100),
  campo = NA  # NA para ignorar efecto aleatorio
)
# Predicciones
# Predicción de efectos fijos
algas$pred <- predict(modelo_glmm, newdata = algas, type = "response",
                           re.form = NA)

# Grafico de curva general
plot(flamencos ~ prop_algas, data = datos,
     col = "grey50",
     pch = 19, 
     cex= 1,
     xlab = "Proporción de diatomeas en muestra",
     ylab = "Abundancia de flamencos",
     cex.lab=1,
     cex.axis=1)

# Línea de predicción general (efectos fijos)
lines(algas$prop_algas, algas$pred, col = "firebrick", lwd = 3)

# EXTRA: Agregar predicciones por humedal si lo desean

# Crear un data frame nuevo para predecir con el modelo
algas_humedal <- expand.grid(
  prop_algas = seq(min(datos$prop_algas), max(datos$prop_algas),
                       length.out = 100),
  humedal = unique(datos$humedal)
)

algas_humedal$pred <- predict(modelo_glmm, newdata = algas_humedal,
                                 type = "response") # agregamos los predichos al data frame flores_seq_grupo


# Asegurarse de que los colores por campo sean únicos
colores <- c("#d55e00", "#cc79a7", "#0072b2", "#f0e442", "#009e73")
niveles_humedal <- levels(datos$humedal)
colores_por_humedal <- setNames(colores, niveles_humedal)

# Creamos un gráfico vacío con los ejes ajustados
plot(datos$prop_algas, datos$flamencos,
     type = "n",
     xlab = "Proporción de diatomeas",
     ylab = "Abundancia de flamencos",
     main = "Curvas por humedal (con efectos aleatorios)",
     cex.lab=1,
     cex.axis=1,
     cex.main= 1.2)

for (id in niveles_humedal) {
  datos_humedal <- subset(datos, datos$humedal == id)
  points(datos_humedal$prop_algas, datos_humedal$flamencos,
         col = colores_por_humedal[id],pch= 19, cex=1)
  
  pred_humedal <- subset(algas_humedal, algas_humedal$humedal == id)
  lines(pred_humedal$prop_algas, pred_humedal$pred,
        col = colores_por_humedal[id], lwd = 2)
}

# Agregar leyenda
legend("topleft", legend = niveles_humedal, col = colores, pch = 19, cex=1,
       title = "Humedal")


