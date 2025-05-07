################################################################################
######                                                                    ######
######               TRABAJO ANÁLISIS DE DATOS CATEGÓRICOS                ######
######                                                                    ######
######                  PREDICCIÓN DE RIESGOS BANCARIOS                   ######
######                                                                    ######
######                       MIGUEL BANDE RODRÍGUEZ                       ######
######                                                                    ######
################################################################################



################################################################################
######  1. IMPORTAMOS LAS LIBRERÍAS NECESARIAS                            ######
################################################################################

library(readr)
library(dplyr)
library(mice)
library(forecast)
library(ggplot2)
library(patchwork)
library(gmodels)
library(epitools)
library(PropCIs)
library(caret)
library(ROCR)
library(MASS)


################################################################################
######  2. IMPORTAMOS LOS DATOS.                                          ######
################################################################################

credit_data <- read_csv("~credit_data.csv", 
                        col_types = cols(rownames = col_skip(), 
                                         Status = col_factor(levels = c("bad", "good")), 
                                         Home = col_factor(levels = c("ignore", "other", "owner", "parents", "priv", "rent")), 
                                         Marital = col_factor(levels = c("divorced", "married", "separated", "single", "widow")), 
                                         Records = col_factor(levels = c("yes", "no")), 
                                         Job = col_factor(levels = c("freelance", "fixed", "others", "partime"))))

# Podemos visualizar la base de datos:
#View(credit_data)

# La base de datos credit_data contiene 4,454 registros y 14 variables.

# Status: Estado del crédito ("good" o "bad"), indicando si el crédito fue pagado o incumplido.
# Seniority: Antigüedad en años del solicitante en su empleo actual.
# Home: Tipo de vivienda del solicitante ("rent", "owner", etc.).
# Time: Duración del préstamo en meses.
# Age: Edad del solicitante en años.
# Marital: Estado civil ("single", "married", "widow", etc.).
# Records: Historial crediticio ("yes" si tiene antecedentes negativos, "no" si no los tiene).
# Job: Tipo de empleo ("fixed", "freelance", etc.).
# Expenses: Gastos mensuales del solicitante.
# Income: Ingresos mensuales del solicitante (hay algunos valores faltantes).
# Assets: Valor de los activos del solicitante (propiedades, ahorros, etc.).
# Debt: Deuda actual del solicitante.
# Amount: Monto solicitado en el préstamo.
# Price: Precio del bien que se quiere adquirir con el préstamo.

# Mostramos la estructura de los datos en R
str(credit_data)

# Ver las primeras filas
head(credit_data)


################################################################################
######  3. PREPROCESADO DE LOS DATOS.                                     ######
################################################################################


###  3. 1 Tratamiento de valores nulos.

# En primer lugar, comprobemos si hay valores nulos en la base de datos:
sum(is.na(credit_data)) # Vemos que hay 455 valores nulos en la base de datos.

cat("El número de valores nulos es:\n", sum(is.na(credit_data)), "\n")

# Identifiquemos dichos valores nulos.
na_positions <- as.data.frame(which(is.na(credit_data), arr.ind = TRUE))
na_positions
# Podemos ver que la mayor parte de valores nulos están en la columna 10 (income)
# aunque también hay en la columna 3 (home), 6 (marital), 8 (job) y en las 
# columnas 11 y 12 (assets y debt).

plot(na_positions,
     col = "blue", pch = 19, cex = 0.5, 
     main = "Valores nulos en la base de datos", 
     xlab = "Índice", ylab = " ",
     yaxt = "n")  # Quitamos el eje Y por defecto

# Agregar nombres de variables en el eje Y
axis(2, at = 1:ncol(credit_data), labels = colnames(credit_data), las = 2)


# Cambiar los nombres de las columnas para mejor comprensión
colnames(na_positions) <- c("Fila", "Columna")

# Agrupar por filas y contar cuántos NA hay en cada fila
na_count_per_row <- na_positions %>%
  group_by(Fila) %>%
  summarise(Cantidad_NA = n())

# Mostrar el resultado
print(na_count_per_row)

borrar <- na_count_per_row$Fila[na_count_per_row$Cantidad_NA > 1]
borrar
# Podemos ver las entradas que tienen más de un valor nulo. Eliminémolas del dataframe

credit_data <- credit_data[-borrar, ]

# Estudiemos de nuevo los valores nulos
na_positions_1 <- as.data.frame(which(is.na(credit_data), arr.ind = TRUE))
na_positions_1


plot(na_positions_1,
     col = "blue", pch = 19, cex = 0.5, 
     main = "Valores nulos en la base de datos", 
     xlab = "Índice", ylab = " ",
     yaxt = "n")  # Quitamos el eje Y por defecto

# Agregar nombres de variables en el eje Y
axis(2, at = 1:ncol(credit_data), labels = colnames(credit_data), las = 2)

# Vemos que solamente nos queda un NA en marital, nos siguen quedando NA en income y en assets.
# Veamos la distribución de Income y de assets para ver cómo podemos imputar los valores nulos.
par(mfrow = c(1, 2))
hist(credit_data$Income, col = "blue", main = "Distribución de ingresos", xlab = "Ingresos", breaks = 20, freq = FALSE)
# Podemos ver que los ingresos parece que siguen una distribución similar a una Chi-cuadrado.

hist(credit_data$Assets, col = "blue", main = "Distribución de activos", xlab = "Activos", breaks = 20)
# Los activos no podemos ver que sigan una distribución conocida.
par(mfrow = c(1, 1))

# Es por esto que vamos a eliminar las filas con valores nulos en assets y en marital
borrar <- na_positions_1$row[na_positions_1$col == 6 | na_positions_1$col == 11]
borrar

credit_data <- credit_data[-borrar, ]

na_positions_2 <- as.data.frame(which(is.na(credit_data), arr.ind = TRUE))
na_positions_2

plot(na_positions_2,
     col = "blue", pch = 19, cex = 0.5, 
     main = "Valores nulos en la base de datos", 
     xlab = "Índice", ylab = " ",
     yaxt = "n")  # Quitamos el eje Y por defecto

# Agregar nombres de variables en el eje Y
axis(2, at = 1:ncol(credit_data), labels = colnames(credit_data), las = 2)


# Vamos a imputar los valores de income por máxima verosimilitud.

# Para ello, vamos a utilizar la función MICE (Multiple Imputation by Chained Equations) del paquete mice.


# No es necesario que indiquemos que queremos imputar en la variable income, pues ya es la única que presenta nulos
mice_model <- mice(credit_data, method = "midastouch", m = 5)


# Obtener el dataset imputado
credit_data_imputed <- complete(mice_model)

str(credit_data_imputed)

# Vemos que ya no hay valores nulos en credit_data_imputed y que además los factores se han mantenido de todos modos.

hist(credit_data_imputed$Income, col = "blue", main = "Distribución de ingresos", xlab = "Ingresos", breaks = 20, freq = FALSE)


###  3. 2 Tratamiento de valores atípicos

# Grafiquemos los boxplots de las variables numéricas para ver si hay valores atípicos.

par(mfrow = c(3, 3))

boxplot(credit_data_imputed$Seniority, col = "blue", main = "Antigüedad en años")
boxplot(credit_data_imputed$Time, col = "blue", main = "Duración del préstamo")
boxplot(credit_data_imputed$Age, col = "blue", main = "Edad")
boxplot(credit_data_imputed$Expenses, col = "blue",main = "Gastos mensuales")
boxplot(credit_data_imputed$Amount, col = "blue", main = "Monto solicitado")
boxplot(credit_data_imputed$Price, col = "blue", main = "Precio del bien")
boxplot(credit_data_imputed$Income, col = "blue", main = "Ingresos")
boxplot(credit_data_imputed$Assets, col = "blue", main = "Activos")
boxplot(credit_data_imputed$Debt, col = "blue", main = "Deuda")

# Podemos ver que excepto en duraación del préstamo y en edad, tenemos valores atípicos en el resto de variables
# De todos modos, no vamos a eliminarlos, pues podría ser importante para el análisis tener en cuenta a personas que, 
# por ejemplo, tengan muchos activos o mucha deuda.
par(mfrow = c(1, 1))


###  3. 3 Estudiemos si están balanceadas las clases categóricas

par(mfrow = c(2, 3))
plot(credit_data_imputed$Status, col = "blue", main = "Variable Status", xlab = "Estado del crédito", ylab = "Frecuencia")
plot(credit_data_imputed$Home, col = "blue", main = "Variable Home", xlab = "Tipo de vivienda", ylab = "Frecuencia")
plot(credit_data_imputed$Marital, col = "blue", main = "Variable Marital", xlab = "Estado civil", ylab = "Frecuencia")
plot(credit_data_imputed$Records, col = "blue", main = "Variable Records", xlab = "Historial crediticio", ylab = "Frecuencia")
plot(credit_data_imputed$Job, col = "blue", main = "Variable Job", xlab = "Tipo de empleo", ylab = "Frecuencia")
par(mfrow = c(1, 1))

# Podemos ver que las clases están desbalanceadas, especialmente en la variable Status, que será nuestra variable objetivo.

# En la clase de nuestra variable objetivo, podemos ver que hay un 72% de créditos pagados y un 28% de créditos no pagados.
credit_data_imputed$Status %>%
  table() %>%
  prop.table()

# Cuando vayamos a entrenar nuestro modelo, tendremos que tener en cuenta este desbalanceo de clases.

# El resto de variables categóricas las vamos a agrupar, dado que hay categorías que están muy desbalanceadas.
# Por ejemplo, en la variable Job, vamos a quedarnos con fixed, y tanto freelance como partime como others los vamos a agrupar en una nueva categoría llamada "others_j".
# En la variable Home, vamos a quedarnos con owner y vamos a agrupar el resto en una nueva categoría llamada "others_h".
# En la variable Marital vamos a quedarnos con married y vamos a agrupar las demás en una nueva categoría llamada "others_m"


credit_data_imputed$Job <- as.character(credit_data_imputed$Job)
credit_data_imputed$Job[credit_data_imputed$Job == "freelance" | credit_data_imputed$Job == "partime" | credit_data_imputed$Job == "others"] <- "others_j"
credit_data_imputed$Job <- as.factor(credit_data_imputed$Job)

credit_data_imputed$Home <- as.character(credit_data_imputed$Home)
credit_data_imputed$Home[credit_data_imputed$Home != "owner"] <- "others_h"
credit_data_imputed$Home <- as.factor(credit_data_imputed$Home)

credit_data_imputed$Marital <- as.character(credit_data_imputed$Marital)
credit_data_imputed$Marital[credit_data_imputed$Marital != "married"] <- "others_m"
credit_data_imputed$Marital <- as.factor(credit_data_imputed$Marital)

credit_data_imputed %>%
  str()

par(mfrow = c(2, 3))
plot(credit_data_imputed$Status, col = "blue", main = "Variable Status", xlab = "Estado del crédito", ylab = "Frecuencia")
plot(credit_data_imputed$Home, col = "blue", main = "Variable Home", xlab = "Tipo de vivienda", ylab = "Frecuencia")
plot(credit_data_imputed$Marital, col = "blue", main = "Variable Marital", xlab = "Estado civil", ylab = "Frecuencia")
plot(credit_data_imputed$Records, col = "blue", main = "Variable Records", xlab = "Historial crediticio", ylab = "Frecuencia")
plot(credit_data_imputed$Job, col = "blue", main = "Variable Job", xlab = "Tipo de empleo", ylab = "Frecuencia")
par(mfrow = c(1, 1))


### 3.4 Transformaciones de las variables numéricas.

# Hay algunas variables numércias que se puede ver en los diagramas de caja y 
# bigotes en los cuales la distribución está sesgada. Vamos a aplicar transfor-
# maciones logarítmicas con el fin de ver si podemos mitigar dicho sesgo.

# Para ello, no vamos a sobreescribir las variables en nuestra base de datos, 
# sino que vamos a crear nuevas variables con las transformaciones.

# Las variables a transformar son: Seniority, Expenses, amount, price, income, 
# assets y debt.

credit_data_imputed$log_Seniority <- log(credit_data_imputed$Seniority + 1)
credit_data_imputed$log_Expenses <- log(credit_data_imputed$Expenses + 1)
credit_data_imputed$log_Amount <- log(credit_data_imputed$Amount + 1)
credit_data_imputed$log_Price <- log(credit_data_imputed$Price + 1)
credit_data_imputed$log_Income <- log(credit_data_imputed$Income + 1)
credit_data_imputed$log_Assets <- log(credit_data_imputed$Assets + 1)
credit_data_imputed$log_Debt <- log(credit_data_imputed$Debt + 1)

par(mfrow = c(3, 3))
boxplot(credit_data_imputed$log_Seniority, col = "blue", main = "Antigüedad en años")
boxplot(credit_data_imputed$Time, col = "blue", main = "Duración del préstamo")
boxplot(credit_data_imputed$Age, col = "blue", main = "Edad")
boxplot(credit_data_imputed$log_Expenses, col = "blue",main = "Gastos mensuales")
boxplot(credit_data_imputed$log_Amount, col = "blue", main = "Monto solicitado")
boxplot(credit_data_imputed$log_Price, col = "blue", main = "Precio del bien")
boxplot(credit_data_imputed$log_Income, col = "blue", main = "Ingresos")
boxplot(credit_data_imputed$log_Assets, col = "blue", main = "Activos")
boxplot(credit_data_imputed$log_Debt, col = "blue", main = "Deuda")
par(mfrow = c(1, 1))

# Podemos ver que en muchas de las variables, como en antigüedad en años, los
# valores atípicos quedan mitigados, así como en gastos mensuales o en activos.
# En otras variables, como ingresos, cantidad del préstamo o en precio del bien
# vemos que la distribución deja de estar sesgada.

# Por la contra, la variable deuda, sigue presentando un montón de valores 
# atípicos, por lo que no vamos a utilizar la transformación logarítmica en 
# dicha variable.

# Como tampoco tendría sentido aplicar una transformación Box-Cox (pues dicha
# transformación solo está definida para valores estrictamente mayores a 0), 
# vamos a categorizar dicha variable, de manera que los valores que sean 0 los
# que se correspondan con no ha habido deuda nunca, y los valores 1 se 
# correspondan con que ha habido deuda alguna vez.

fun_cat_deuda <- function(x){
  if(x > 0) {return(1)}
  else {return(0)}
}

credit_data_imputed$Debt_cat <- credit_data_imputed$Debt %>%
  sapply(fun_cat_deuda) %>%
  as.factor()

par(mfrow = c(1, 1))
plot(credit_data_imputed$Debt_cat, col = "blue", main = "Variable Debt_cat", xlab = "Deuda", ylab = "Frecuencia")

# Aunque vemos que dicha clase está bastante desbalanceada, vamos a dejarla así,
# pues es una variable que puede ser importante para el análisis a la hora de 
# predecir si un crédito es bueno o malo (es decir, si se va o no a pagar).


################################################################################
######  4. TAREA 1: TABLAS DE CONTINGENCIA                                ######
################################################################################

# La tarea de este apartado del estudio engloba las siguientes subtareas.

# 1.1. Plantear el objetivo del estudio. Construier las correspondientes tablas
# de contingencia. Comparar grupos empleando las diferentes medidas estudiadas
# (DP, RR, OR) junto con los correspondientes intervalos de confianza (95%).

# 1.2. Valorar la influencia de una tercera variable como posible variable de 
# confusión o interacción.

# 1.3 Analizar la hipótesis de independencia entre las variables aplicando el 
# test que corresponda según los datos de la tabla.

# 1.4. Interpretar los resultados obtenidos.


### 4.1.1. Objetivos del estudio. Tablas de contingencia.

# El objetivo del estudio es analizar si la variable Status (Estado del crédito)
# está relacionada con las variables Home, Marital, Records, Job y Debt_cat.
# Entender estas relaciones puede ser fundamental para saber si conceder un 
# crédito a una persona o no.

tabla_home <- xtabs(~ Home + Status , credit_data_imputed)
tabla_marital <- xtabs(~ Marital + Status, credit_data_imputed)
tabla_records <- xtabs(~ Records + Status, credit_data_imputed)
tabla_job <- xtabs(~ Job + Status, credit_data_imputed)
tabla_debt <- xtabs(~ Debt_cat + Status, credit_data_imputed)

# Vamos a visualizar las tablas de contingencia
print(tabla_home)
print(tabla_marital)
print(tabla_records)
print(tabla_job)
print(tabla_debt)

plots <- lapply(c("Home", "Marital", "Records", "Job", "Debt_cat"), function(var) {
  ggplot(credit_data_imputed, aes_string(x = var, fill = "Status")) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c("good" = "blue", "bad" = "darkgoldenrod1")) +
    labs(x = var, y = "Proporción", fill = "Pagado") +
    theme_minimal()
})

# Unir los gráficos
wrap_plots(plots, ncol = 3)


# Hagamos ahora el cálculo de las diferencias de proporciones, el riesgo 
# relativo y el odds ratio para cada una de las variables.
# Para ello creamos la siguiente función, que analiza las tablas de contingencia

# Function from Laura A. Thompson
Wald.ci = function (Table , aff.response , alpha =.05 ){
  # Gives two - sided Wald CI 's for odds ratio ,
  # difference in proportions and relative risk.
  # Table is a 2x2 table of counts with rows giving
  # the treatment populations
  # aff.response is a string like "c (1 ,1)" giving the cell
  # of the beneficial response and the treatment category
  # alpha is significance level
  pow = function (x, a= -1) x^a
  z.alpha = qnorm (1- alpha /2)
  if( is.character ( aff.response ))
    where = eval ( parse ( text = aff.response ))
  else where = aff.response
  Next = as.numeric ( where ==1) + 1
  
  # OR
  odds.ratio = Table [ where [1] , where [2]] * Table [ Next [1] , Next [2]] /
    ( Table [ where [1] , Next [2]] * Table [ Next [1] , where [2]])
  se.OR = sqrt (sum(pow( Table )))
  ci.OR = exp(log( odds.ratio ) + c( -1 ,1)* z.alpha * se.OR )
  
  # difference of proportions
  p1 = Table [ where [1] , where [2]] /(n1= Table [ where [1] , Next [2]] +
                                          Table [ where [1] , where [2]])
  p2= Table [ Next [1] , where [2]] /(n2= Table [ Next [1] , where [2]] +
                                        Table [ Next [1] , Next [2]])
  se.diff = sqrt (p1*(1- p1)/n1 + p2*(1- p2)/n2)
  ci.diff = (p1 -p2) + c( -1 ,1)* z.alpha * se.diff
  
  # relative risk
  RR = p1/p2
  se.RR = sqrt ((1 - p1)/(p1*n1) + (1- p2)/(p2*n2 ))
  ci.RR = exp(log(RR) + c( -1 ,1)* z.alpha * se.RR )
  list (OR= list ( odds.ratio = odds.ratio , CI= ci.OR ),
        proportion.difference = list ( diff =p1 -p2 , CI= ci.diff ),
        relative.risk = list ( relative.risk =RR , CI= ci.RR ))
}


analizar_tabla <- function(tabla, nombre_var) {
  cat("\n========================================\n")
  cat("🔍 Variable:", nombre_var, "\n")
  print(tabla)
  
  # Extraer frecuencias
  a <- tabla[1, 1]
  b <- tabla[1, 2]
  c <- tabla[2, 1]
  d <- tabla[2, 2]
  
  # Wald.ci (como resumen completo)
  cat("\n📌 Medidas con función Wald.ci:\n")
  print(Wald.ci(matrix(c(a, b, c, d), nrow = 2, byrow = TRUE), c(1, 1)))
}


# Ahora aplicamos la función a cada una de nuestras tablas. Para ello usamos la 
# función lapply

tablas <- list(
  Home = tabla_home,
  Marital = tabla_marital,
  Records = tabla_records,
  Job = tabla_job,
  Debt_cat = tabla_debt
)

invisible(lapply(names(tablas), function(var) {
  analizar_tabla(tablas[[var]], var)
}))

# Ahí tenemos nuestros resultados.

### 4.1.2. Influencia de una tercera variable de confusión.

## Confusión:
# Por ejemplo, si parece que los que tienen Records tienen más impagos (Status 
# = bad), pero ese efecto desaparece cuando controlamos por Job, entonces Job es 
# una variable de confusión.

## Interacción:
# Por ejemplo, si el efecto de Records sobre Status es mucho más fuerte en
# personas sin trabajo fijo, entonces Job es un modificador de efecto.

# Para ver esto, vamos a crear tablas 2x2 en las que aparezca la tercera 
# variable

# Veamos la relación entre Records y Status, estratificando por Job
tabla_estratificada <- xtabs(~ Records + Status + Job, data = credit_data_imputed)
tabla_estratificada

# Para Job = fixed (estrato 1)
Wald.ci(tabla_estratificada[, , 1], c(1, 1))  

# Para Job = others_j (estrato 2)
Wald.ci(tabla_estratificada[, , 2], c(1, 1))

# Al analizar la asociación entre la variable Records (antecedentes de crédito 
# negativos) y el estado del crédito Status, se observa un efecto significativo 
# en ambos estratos de la variable Job.

# En personas con trabajo fijo, la odds ratio es 5.38 (IC 95%: 4.34–6.67), lo 
# que indica que tener antecedentes negativos multiplica por más de 5 la 
# probabilidad de impago respecto a quienes no los tienen.
# En personas con trabajo no fijo, la OR también es significativa pero menor: 
# 3.48 (IC 95%: 2.68–4.52).

# Asimismo, el riesgo relativo (RR) y la diferencia de proporciones (DR) son 
# mayores en el grupo con empleo fijo.
# Esta diferencia entre estratos sugiere que la variable Job podría estar 
# actuando como un modificador del efecto de Records sobre el impago (Status). 
# Es decir, el efecto de tener antecedentes negativos varía según el tipo de 
# empleo del solicitante.

# Por tanto, no se trataría únicamente de una variable confusora, sino de una 
# posible interacción, lo cual debe considerarse en el modelo predictivo o en 
# las decisiones de análisis de riesgo.


# Estratificamos por Marital
tabla_marital_estrat <- xtabs(~ Records + Status + Marital, data = credit_data_imputed)
tabla_marital_estrat

# Estrato 1: Marital = married
Wald.ci(tabla_marital_estrat[, , 1], c(1, 1))

# Estrato 2: Marital = others_m
Wald.ci(tabla_marital_estrat[, , 2], c(1, 1))


# La variable Marital (estado civil) como tercera variable en la relación entre
# Records (antecedentes negativos) y Status (estado del crédito).

# En personas casadas, la odds ratio (OR) es de 4.57 (IC 95%: 3.78–5.52), y el 
# riesgo relativo (RR) es 2.69 (IC 95%: 2.41–3.01).

# En personas no casadas, la OR es 4.35 (IC 95%: 3.12–6.06), y el RR es
# 2.19 (IC 95%: 1.90–2.53).

# Aunque ambas asociaciones son fuertes y estadísticamente significativas, 
# las diferencias entre los estratos son pequeñas. Esto indica que la variable 
# Marital no modifica sustancialmente el efecto de Records sobre el estado del crédito.

# Por tanto, en este caso, Marital podría actuar como una variable confusora 
# leve, pero no como una variable de interacción.



### 4.1.3. Test de independencia.

# Para ello vamos a aplicar el test de la Chi-Cuadrado a cada tabla de las que
# hemos construido anteriormente

for (var in names(tablas)) {
  cat("\n===================================\n")
  cat("Variable:", var, "\n")
  tabla <- tablas[[var]]
  print(tabla)
  
  # Aplicar test de chi-cuadrado
  test <- chisq.test(tabla, correct = FALSE)
  
  # Hipótesis
  cat("\n   Hipótesis:\n")
  cat("H₀: Las variables", var, "y Status son independientes.\n")
  cat("H₁: Las variables", var, "y Status no son independientes (hay asociación).\n")
  
  # Resultado del test
  cat("\n   Resultado del test de chi-cuadrado:\n")
  cat("Estadístico X²:", round(test$statistic, 4), "\n")
  cat("p-valor:", round(test$p.value, 5), "\n")
  
  # Decisión
  if (test$p.value < 0.05) {
    cat("❗ Se rechaza H₀ → Hay evidencia de asociación entre", var, "y Status.\n")
  } else {
    cat("✅ No se rechaza H₀ → No hay evidencia suficiente de asociación.\n")
  }
}

# Se aplicó el test de independencia de chi-cuadrado entre la variable Status y 
# cinco variables explicativas.
# En todos los casos, excepto Debt_cat, se obtuvo un p-valor muy bajo (< 0.001),
# lo cual indica que existe una asociación estadísticamente significativa entre 
# dichas variables (Home, Marital, Records, Job) y el estado del crédito.

# Por el contrario, en el caso de Debt_cat, el p-valor fue 0.226, por lo que no 
# se rechaza la hipótesis de independencia, y no hay evidencia suficiente de que
# esa variable esté asociada a Status.



################################################################################
######  4. TAREA 2: REGRESIÓN LOGÍSTICA                                   ######
################################################################################

# La tarea de este apartado del estudio engloba las siguientes subtareas.

# 2.1. Establece una pregunta de investigación (variable respuesta: binaria) a 
# la que puedas responder con los datos que elegiste y sobre los que trabajaste 
# en la Tarea 1.

# 2.2. Describe brevemente las variables que figuran en la base de datos elegida

# 2.3. En función de tu pregunta de investigación, explica si aplicarás un 
# modelo predictivo o un modelo explicativo.

# 2.4. Aplica un modelo de Regresión Logística.
# Para ello, deberás describir el procedimiento seguido en la elección de las 
# variables incluidas en el modelo máximo inicial (cribado previo: tablas de 
# contingencia, modelos univariantes... o variable con justificación 
# teórica/práctica). Si descartas alguna variable desde el principio, también 
# deberás justificarlo.

# 2.5. Describe los pasos seguidos hasta alcanzar el modelo final e interpreta 
# los resultados del mismo (en base a tu pregunta de investigación).
# En caso de que sea un modelo predictivo, describe su ajuste y potencia 
# predictiva, y lleva a cabo una predicción que te resulte de interés.


### 4.2.1. Pregunta de investigación.

# El objetivo de este análisis es predecir si un crédito será pagado o no, 
# es decir, si el cliente caerá en impago, utilizando como variable respuesta 
# Status (binaria: "good" o "bad").

# Las variables explicativas que se utilizarán en el modelo han sido previamente 
# analizadas en la Tarea 1 e incluyen características del solicitante como:

# - Condición de vivienda (Home)
# - Estado civil (Marital)
# - Historial de crédito (Records)
# - Tipo de empleo (Job)
# - Nivel de deuda categorizado (Debt_cat)
# - Y variables numéricas como Income, Assets, Debt, etc., ya imputadas y 
    # preprocesadas.

# Este análisis permitirá identificar los factores que más influyen en el 
# cumplimiento del pago de créditos y contribuirá a la toma de decisiones en la 
# concesión de los mismos.


### 4.2.2. Descripción de las variables.

# La descripción de las variables ya ha sido realizada en el preprocesado de los
# datos, al principio del documento.


### 4.2.3. Modelo predictivo o explicativo.

# En función de la pregunta de investigación planteada, el enfoque de este 
# modelo es predictivo.

# El objetivo es anticipar si un cliente pagará o no su crédito, basándonos en 
# sus características personales y financieras.

# Por tanto, aplicaremos un modelo predictivo de regresión logística, que nos 
# permita:

# - Estimar la probabilidad de impago (Status = "bad")
# - Clasificar correctamente a los clientes en función del riesgo
# - Evaluar el rendimiento del modelo con medidas como la matriz de confusión o 
    # el AUC-ROC.

# Aunque también se podrá interpretar la influencia de las variables 
# (coeficientes), el foco principal es la capacidad de predicción.


### 4.2.4. Modelo de Regresión Logística.

# En base al análisis previo de tablas de contingencia y justificación teórica,
# se incluyeron en el modelo las variables: Home, Marital, Records, Job, 
# log_Seniority, Time, Age, log_Expenses, log_Income, log_Assets, log_Amount y 
# log_Price.

# Se descartó la variable Debt_cat por no mostrar asociación significativa con 
# Status.

# Inicialmente se ajustó un modelo sin interacciones. Posteriormente, se valora
# la inclusión de términos de interacción basados en resultados exploratorios 
# (por ejemplo, la posible interacción entre Records y Job).




# Para ajustar el modelo de regresión logística con el fin de predecir, se ha
# decidido tanto balancear el dataset en función a la variable Status (que es la
# variable a predecir), así como dividir el dataset en dos subconjuntos: uno de 
# entrenamiento y otro de test.

# Fijemos en primer lugar una semilla para la reproducibilidad del estudio.
set.seed(1)

n <- nrow(credit_data_imputed)
idx <- sample(1:n)

# Dividimos el dataset en entrenamiento y test
n_train <- round(0.7 * n)
n_valid <- round(0.2 * n)

train_data <- credit_data_imputed[idx[1:n_train], ]
valid_data <- credit_data_imputed[idx[(n_train + 1):(n_train + n_valid)], ]
test_data  <- credit_data_imputed[idx[(n_train + n_valid + 1):n], ]


# Balanceamos solo el conjunto de entrenamiento
# library(caret)

train_bal <- upSample(x = train_data[, -which(names(train_data) == "Status")],
                      y = train_data$Status,
                      yname = "Status")

# Reordenar niveles: que "good" sea el 0 y "bad" el 1
train_bal$Status <- relevel(factor(train_bal$Status), ref = "good")
valid_data$Status <- relevel(factor(valid_data$Status), ref = "good")
test_data$Status  <- relevel(factor(test_data$Status), ref = "good")


table(train_bal$Status)  # Podemos ver claramente que está balanceado
# Tomamos la decisión de hacer sobremuestreo.
# Esto significa que se han replicado observaciones de la clase minoritaria para 
# igualar el número de casos entre clases.
# Dado que contamos con un número suficiente de datos, este enfoque no introduce
# un exceso de varianza ni riesgo de sobreajuste significativo.

# Ahora, en primer lugar, vamos a probar los modelos univariantes con el fin de
# estudiar los p-valores para ver qué características son relevantes.

# Lista de predictores a evaluar
vars_univariantes <- c("Home", "Marital", "Records", "Job",
                       "log_Seniority", "Time", "Age",
                       "log_Expenses", "log_Income", "log_Assets",
                       "log_Amount", "log_Price")

# Creamos un dataframe para guardar resultados
resultados_uni <- data.frame(
  Variable = character(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)


# Iteramos sobre las variables
for (v in vars_univariantes) {
  # Creamos la fórmula
  form <- as.formula(paste("Status ~", v))
  
  # Ajustamos el modelo
  m <- glm(form, family = binomial(link = "logit"), data = train_bal)
  
  # Extraemos coeficiente (si es numérica, solo 1; si es categórica, puede haber más)
  coefs <- summary(m)$coefficients
  
  # Para simplicidad, tomamos solo la fila 2 (la primera después del intercepto)
  if (nrow(coefs) > 1) {
    OR <- exp(coefs[2, 1])
    p <- coefs[2, 4]
  } else {
    OR <- NA
    p <- NA
  }
  
  # Guardamos
  resultados_uni <- rbind(resultados_uni,
                          data.frame(Variable = v,
                                     p_value = round(p, 4)))
}

# Mostramos resultados ordenados por p-value
resultados_uni[order(resultados_uni$p_value), ]

# Podemos ver que en base a los p-valores obtenidos, se consideran candidatas
# para el modelo múltiple aquellas variables con p < 0.05. 
# No obstante, variables con relevancia teórica (como log_Price o log_Expenses) 
# se mantendrán para comprobar su comportamiento en un modelo ajustado con más
# predictores.


# Ajustamos ahora el modelo maximal de regresión logística:
modelo_max <- glm(Status ~ Home + Marital + Records + Job +
                    log_Seniority + Time + Age +
                    log_Expenses + log_Income + log_Assets +
                    log_Amount + log_Price,
                  family = binomial(link = "logit"),
                  data = train_bal)


summary(modelo_max)


library(MASS)
modelo_step <- stepAIC(modelo_max, direction = "backward", trace = FALSE)

summary(modelo_step)



## Probamos modelos con interacciones

modelo_interacciones_max <- glm(Status ~ Home + Marital + Records + Job +
                              log_Seniority + Time + Age +
                              log_Expenses + log_Income + log_Assets +
                              log_Amount + log_Price +
                              Records:Job + Marital:Home +
                              log_Income:log_Expenses,
                            family = binomial(link = "logit"),
                            data = train_bal)

summary(modelo_interacciones_max)

modelo_inter_step <- stepAIC(modelo_interacciones_max, direction = "backward", trace = FALSE)

summary(modelo_inter_step)

## Ahora vamos a comparar nuestros modelos, para ello construyamos una función
# que en base al conjunto de validación nos devuelva las métricas de evaluación
# así como la curva roc (AUC-ROC)

evaluar_modelo <- function(modelo, datos_valid, color = "black", add = FALSE, nombre = "Modelo") {
  # Probabilidades predichas
  probs <- predict(modelo, newdata = datos_valid, type = "response")
  real <- ifelse(datos_valid$Status == "bad", 1, 0)
  
  # Curva ROC
  pred <- prediction(probs, real)
  perf <- performance(pred, "tpr", "fpr")
  plot(perf, col = color, lwd = 2, add = add, main = ifelse(!add, "Curvas ROC - Validación", ""))
  
  # AUC
  auc <- performance(pred, "auc")@y.values[[1]]
  cat("\n=====================================\n")
  cat("", nombre, "\n")
  cat("AUC:", round(auc, 4), "\n")
  
  # Predicciones binarias
  pred_clase <- ifelse(probs > 0.5, "bad", "good")
  real_clase <- datos_valid$Status
  
  # Matriz de confusión
  cm <- confusionMatrix(factor(pred_clase, levels = c("bad", "good")),
                        factor(real_clase, levels = c("bad", "good")))
  
  print(cm$table)
  cat("Accuracy:", round(cm$overall["Accuracy"], 4), "\n")
  cat("Sensibilidad (Recall bad):", round(cm$byClass["Sensitivity"], 4), "\n")
  cat("Especificidad:", round(cm$byClass["Specificity"], 4), "\n")
  cat("Precisión (PPV):", round(cm$byClass["Pos Pred Value"], 4), "\n")
  cat("=====================================\n")
  
  return(auc)
}

auc_max <- evaluar_modelo(modelo_max, valid_data, color = "blue", nombre = "Modelo Maximal")
auc_step <- evaluar_modelo(modelo_step, valid_data, color = "red", add = TRUE, nombre = "Modelo StepAIC")
auc_inter_max <- evaluar_modelo(modelo_interacciones_max, valid_data, color = "orange", add = TRUE, nombre = "Modelo Interacciones Max")
auc_inter_step <- evaluar_modelo(modelo_inter_step, valid_data, color = "darkgreen", add = TRUE, nombre = "Modelo Interacciones Step")

legend("bottomright", legend = c("Maximal", "StepAIC", "Maximal Interacciones", "Step Interacciones"),
       col = c("blue", "red", "orange", "darkgreen"), lwd = 2)



# Tras comparar diferentes modelos (maximal, stepwise, con y sin interacciones)
# en el conjunto de validación, observamos que todos obtienen un AUC muy similar
# (~0.83), lo cual indica una buena capacidad discriminativa general del modelo
# para distinguir entre créditos pagados y no pagados.

# No obstante, dado que el objetivo del análisis es prevenir impagos,
# nos interesa priorizar métricas que reflejen esa capacidad:
# concretamente, la sensibilidad (% impagos detectados correctamnente) y la 
# precisión (qué proporción de las predicciones de impago fueron correctas).

# Si la sensibilidad es baja, se nos escaparían los malos pagadores.

# Aunque la precisión fue moderada (~0.53), esto puede explicarse por el 
# desequilibrio en el conjunto de validación (mayoría de casos "good"), lo cual 
# es esperable y refleja la situación real del problema.

# En este contexto, la precisión cobra especial relevancia, ya que si 
# es demasiado baja, podríamos estar rechazando muchos clientes buenos (falsos 
# positivos), lo que conllevaría una pérdida de oportunidades para la entidad 
# financiera.

# Teniendo en cuenta la combinación de:
# - AUC elevado
# - Sensibilidad alta (~0.79)
# - Precisión razonable (~0.53)
# - Y simplicidad del modelo

# Se selecciona como modelo final el modelo reducido con interacciones
# (`modelo_inter_step`), que ofrece el mejor equilibrio entre interpretabilidad
# y capacidad predictiva en validación.

# A continuación, evaluamos este modelo en el conjunto de test para obtener una
# estimación más precisa de su rendimiento en datos no vistos.

evaluar_modelo(modelo_inter_step, test_data, color = "blue", nombre = "Modelo Final")

# El modelo final (`modelo_inter_step`) ha sido evaluado sobre el conjunto de 
# test, mostrando un rendimiento satisfactorio:

# - AUC: 0.839 → Excelente capacidad discriminativa
# - Accuracy: 79.1%
# - Sensibilidad (Recall de impagos): 73.6% → el modelo detecta correctamente la
    # mayoría de los impagos
# - Especificidad: 80.9%
# - Precisión (PPV): 56.2% → más de la mitad de las predicciones de impago  
    # fueron correctas

# Estos resultados confirman que el modelo mantiene su rendimiento en datos 
# nuevos,y que puede ser una herramienta útil para apoyar la toma de decisiones 
# sobre la concesión de créditos, ayudando a prevenir impagos sin rechazar 
# excesivamente a buenos pagadores.

# Vamos a realizar un par de predicciones que nos resulten de interés:

nuevo_cliente <- data.frame(
  Home = "owner",
  Marital = "married",
  Records = "no",
  Job = "fixed",
  log_Seniority = log(20+1),
  Time = 60,
  Age = 39,
  log_Expenses = log(60+1),
  log_Income = log(68+1),
  log_Assets = log(13000+1),
  log_Amount = log(900+1),
  log_Price = log(975+1)
)

# Aseguramos que los factores tengan los mismos niveles
nuevo_cliente$Home <- factor(nuevo_cliente$Home, levels = levels(train_bal$Home))
nuevo_cliente$Marital <- factor(nuevo_cliente$Marital, levels = levels(train_bal$Marital))
nuevo_cliente$Records <- factor(nuevo_cliente$Records, levels = levels(train_bal$Records))
nuevo_cliente$Job <- factor(nuevo_cliente$Job, levels = levels(train_bal$Job))


prob_impago_ej_1 <- predict(modelo_inter_step, newdata = nuevo_cliente, type = "response")

evaluar <- function(prob) {
  cat("Probabilidad de impago estimada:", round(prob, 4), "\n")
  if (prob > 0.5) {
    cat("❌ Crédito NO recomendado (riesgo elevado de impago)\n")
  } else {
    cat("✅ Crédito aprobado (riesgo bajo de impago)\n")
  }
}

evaluar(prob_impago_ej_1) # El resultado es que concedemos el crédito



# Ejemplo 2:

cliente_riesgoso <- data.frame(
  Home = "others_h",
  Marital = "others_m",
  Records = "no",
  Job = "others_j",
  log_Seniority = log(1+1),
  Time = 60,
  Age = 21,
  log_Expenses = log(41+1),
  log_Income = log(60+1),
  log_Assets = log(1),    
  log_Amount = log(850+1),
  log_Price = log(930+1)
)

# Aseguramos que los factores tengan los mismos niveles
cliente_riesgoso$Home <- factor(cliente_riesgoso$Home, levels = levels(train_bal$Home))
cliente_riesgoso$Marital <- factor(cliente_riesgoso$Marital, levels = levels(train_bal$Marital))
cliente_riesgoso$Records <- factor(cliente_riesgoso$Records, levels = levels(train_bal$Records))
cliente_riesgoso$Job <- factor(cliente_riesgoso$Job, levels = levels(train_bal$Job))

prob_impago_ej_2 <- predict(modelo_inter_step, newdata = cliente_riesgoso, type = "response")

evaluar(prob_impago_ej_2) # El resultado es que NO concedemos el crédito

#### Vamos a guardar el modelo que hemos realizado:
saveRDS(modelo_inter_step, "modelo_inter_step.rds")



################################################################################
######  4. TAREA 3: REGRESIÓN LOGÍSTICA MULTINOMIAL Y ORDINAL             ######
################################################################################

# La tarea de este apartado del estudio engloba las siguientes subtareas:

# 3.1. Establece una pregunta de investigación

# 3.2. Aplica un modelo de regresión logística multinomial u ordinal en alguna 
# las variables de la base de datos.

# 3.3. Describe los pasos seguidos hasta alcanzar el modelo final e interpreta
# los resultados del mismo (en base a tu pregunta de investigación).


### 4.3.1. Pregunta de investigación.
# La pregunta que se pretende responder es: 
# ¿Las variables explicativas (edad, tipo de empleo, estado civil, etc.) están
# asociadas a que un solicitante de crédito pertenezca a un nivel bajo, medio o
# alto de ingresos?

# Para responder a esta pregunta, se aplicará un modelo de regresión logística
# ordinal, donde la variable respuesta será la variable ingresos categorizada en 
# tres niveles: bajo, medio y alto.


### 4.3.2. Modelo de regresión logística ordinal.

# En primer lugar, categoricemos la variable Income en tres niveles: bajo, medio
# y alto. En este caso, vamos a categorizar directamente la variable logaritmo 
# de Income.

credit_data_imputed$Income_cat <- cut(credit_data_imputed$log_Income,
                                          breaks = quantile(credit_data_imputed$log_Income, 
                                                            probs = c(0, 1/3, 2/3, 1)),
                                          labels = c("bajo", "medio", "alto"), 
                                          include.lowest = TRUE)

# Comprobamos la distribución de la variable
table(credit_data_imputed$Income_cat)
plot(credit_data_imputed$Income_cat, col = "blue", main = "Distribución de la variable Income_cat")

# Ahora tenemos que convertir a factor (ordenado) dicha variable:
credit_data_imputed$Income_cat <- factor(credit_data_imputed$Income_cat, 
                                             levels = c("bajo", "medio", "alto"),
                                             ordered = TRUE)

# Vamos a ajustar un modelo de regresión logística ordinal para predecir el 
# nivel de ingresos de los solicitantes de crédito, categorizado en bajo, medio 
# y alto.

# Las variables incluidas en el modelo son: tipo de empleo (Job), estado civil 
# (Marital), edad (Age), historial crediticio (Records) y tipo vivienda (Home).

modelo_ordinal <- polr(Income_cat ~ Job + Marital + Age + Records + Home,
                       data = credit_data_imputed,
                       method = "logistic")

summary(modelo_ordinal)

# OR y IC 95%
exp(coef(modelo_ordinal))            # OR (odds de estar en un nivel superior)
exp(confint(modelo_ordinal))         # IC 95% 

# Prueba de razón de verosimilitudes (modelo vs. nulo)
modelo_nulo <- polr(Income_cat ~ 1, data = credit_data_imputed)
anova(modelo_ordinal, modelo_nulo, test = "Chisq")



# - Job (others_j): OR = 0.74, IC 95% [0.66, 0.83]
# Los solicitantes sin trabajo fijo tienen 26% menos odds de estar en un nivel  
# superior de ingresos respecto a quienes tienen empleo fijo.

# - Marital (others_m): OR = 0.59, IC 95% [0.51, 0.67]
# Las personas no casadas tienen menor probabilidad de pertenecer a niveles 
# altos de ingresos en comparación con las casadas.

# - Age: OR = 1.01, IC 95% [1.007, 1.019]
# La probabilidad de pertenecer a un nivel superior de ingresos aumenta 
# ligeramente con la edad.

# - Records (no): OR = 0.82, IC 95% [0.71, 0.95]
# Quienes no tienen antecedentes negativos presentan ligeramente menores odds de
# estar en niveles altos de ingresos. Esto puede indicar la presencia de otros
# factores subyacentes.

# - Home (owner): OR = 1.34, IC 95% [1.19, 1.51]
# Ser propietario de la vivienda se asocia significativamente con una mayor
# probabilidad de ingresos altos.


# Además, el modelo completo fue comparado con un modelo nulo mediante una 
# prueba de razón de verosimilitud, obteniéndose:
# Chi² = 228.84, gl = 5, p < 0.001

# Lo que indica que el modelo con predictores explica significativamente mejor 
# la variabilidad en los niveles de ingresos.

# Podemos concluir entonces que las variables sociodemográficas como el tipo de 
# empleo, estado civil, edad, situación habitacional y antecedentes crediticios
# están asociadas con el nivel de ingresos de los solicitantes de crédito.

# Estos resultados pueden ser útiles para entender el perfil económico de los 
# clientes y orientar estrategias de segmentación o riesgo.





