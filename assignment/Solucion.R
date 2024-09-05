##########################PUNTO 1 ##########################################

#P1 n=1(−1)n tanh n
# Definir el número de términos a calcular
num_terms <- 600

# Calcular la suma parcial de la serie
suma_parcial <- sapply(1:num_terms, function(n) (-1)^n * tanh(n))
suma_acumulada <- cumsum(suma_parcial)

# Graficar la suma acumulada
plot(1:num_terms, suma_acumulada, type = "l", col = "blue", 
     xlab = "Número de términos (n)", ylab = "Suma acumulada", 
     main = "Comportamiento de la serie sum((-1)^n * tanh(n))")
grid()


# Personalizar los ticks del eje x para que vayan de 0.5 en 0.5
axis(1, at = seq(0, num_terms, by = 1))
# Imprimir el último valor de la suma acumulada
cat("Suma acumulada después de", num_terms, "términos:", suma_acumulada[num_terms], "\n")

#R// Debido al factor ((-1)^n), los términos de la serie alternan en signo por lo que se forma un aspecto de arco
#    y no converge a un solo punto en la grafica

##########################Punto 1.2 ############################################
# Definir el número de términos a calcular
num_terms <- 2000

# Calcular la suma parcial de la serie
suma_parcial <- sapply(1:num_terms, function(n) (atan(n))^2 / (n^2 + 1))
suma_acumulada <- cumsum(suma_parcial)


# Graficar la suma acumulada
plot(1:num_terms, suma_acumulada, type = "l", col = "blue", 
     xlab = "Número de términos (n)", ylab = "Suma acumulada", 
     main = "Comportamiento de la serie sum((atan(n))^2 / (n^2 + 1))")


grid()

# Personalizar los ticks del eje x para que vayan de 0.5 en 0.5
axis(1, at = seq(0, num_terms, by = 0.5))

# Imprimir el último valor de la suma acumulada
cat("Suma acumulada después de", num_terms, "términos:", suma_acumulada[num_terms], "\n")


>>>>> #R// Se comprueba la convergencia de la serie mediante una Suma acumulada después de 2000 términos: 1.267388 




#######################PUNTO 1.3###################################

# Definir el número de términos a calcular
num_terms <- 1000

# Calcular la suma parcial de la serie
suma_parcial <- sapply(2:(num_terms + 1), function(n) log(factorial(n)) / (n^3))
suma_acumulada <- cumsum(suma_parcial)

# Graficar la suma acumulada
plot(2:(num_terms + 1), suma_acumulada, type = "l", col = "blue", 
     xlab = "Número de términos (n)", ylab = "Suma acumulada", 
     main = "Comportamiento de la serie sum(log(n!) / n^3)")
grid()

# Personalizar los ticks del eje x para que vayan de 1 en 1
axis(1, at = seq(2, num_terms + 1, by = 1))

# Imprimir el último valor de la suma acumulada
cat("Suma acumulada después de", num_terms, "términos:", suma_acumulada[num_terms], "\n")

>>>>>> #R// Se comprueba la convergencia de la serie mediante una Suma acumulada después de 1000 términos: 0.5384836


#########################PUTNO 1.4 #################################################

# Definir la función para calcular la serie de Taylor para sin(x)
taylor_series_sin <- function(x, n_terms) {
  sin_approx <- 0
  for (n in 0:(n_terms - 1)) {
    term <- ((-1)^n * x^(2*n + 1)) / factorial(2*n + 1)
    sin_approx <- sin_approx + term
  }
  return(sin_approx)
}

# Valores de prueba
x_values <- seq(-2 * pi, 2 * pi, length.out = 400)
n_terms <- 100

# Calcular la serie de Taylor y los valores reales de sin(x)
taylor_approximations <- sapply(x_values, taylor_series_sin, n_terms = n_terms)
actual_sin_values <- sin(x_values)

# Comparar la serie de Taylor con la función sin(x)
comparison <- data.frame(
  x = x_values,
  Taylor_Series = taylor_approximations,
  Actual_Sin = actual_sin_values
)

# Mostrar los primeros valores para verificar
head(comparison)

# Graficar
plot(x_values, actual_sin_values, type = "l", col = "blue", lwd = 2, 
     ylab = "y", xlab = "x", main = "Comparación entre sin(x) y su serie de Taylor")
lines(x_values, taylor_approximations, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("sin(x)", paste("Taylor series (n =", n_terms, ")")), 
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)


>>>>> #R// SE DEMUESTRA NUMERICAMENTE QUE : sin(x) = P1n=0(−1)n (2n+1)!x2n+1 (Serie de taylor) <<<<<<<




#############################MANIPULACION DE DATOS ##################################################################################

################################METODOLOGIA # 1 ############################################
# Cargar la librería necesaria
library(readr)

# Leeemos el archivo CSV sin encabezado y sin modificar
data <- read.csv("/workspaces/assignment-r-SantiagoCardonaLenis/assignment/data.csv", header = FALSE)
View(data)

################################METODOLOGIA # 2 ############################################
# Cargar el archivo CSV
data2 <- read.csv("/workspaces/assignment-r-SantiagoCardonaLenis/assignment/data.csv")
View(data2)


########################################## Punto 1.1 ######################################################################################

### LA suma de la segunda columna no aplica a datos de tipo character
# Calcular la suma de la segunda columna
suma_segunda_columna <- sum(data[[2]], na.rm = TRUE)

# Imprimir la suma de la segunda columna
cat("Suma de la segunda columna:", suma_segunda_columna, "\n")

########################################## Punto 1.1 ######################################################################################
## se puede extrar la posiciones y sumar 
#Imprima la cantidad de registros por letra para la segunda columna, ordenados alfabeticamente:

# version 1
segunda_columna <- substr(data[[2]], 1, 1)
segunda_columna

segunda_columna <-toupper(segunda_columna)

conteo_letras <- table(segunda_columna)
conteo_letras


########################################## punto 1.2 ##################################################################

#Imprima la cantidad de registros por letra para la primera columna, ordenados alfabeticamente:

# version 1
primera_columna <- substr(data[[1]], 1, 1)
primera_columna

primera_columna <- toupper(primera_columna)

conteo_letra <- table(primera_columna)
conteo_letra


## version 2

# Contar la cantidad de registros por letra en la primera columna y ordenarlos alfabéticamente

primera_columna <- substr(data2[[1]], 1, 1)
primera_columna

primera_columna <- toupper(primera_columna)

suma<- table(primera_columna)
suma

########################################## punto 1.3 ##################################################################


# Calcular la suma de la segunda columna por cada letra de la primera columna
suma_por_letra <- conteo_letras %>%
  group_by(primera_columna) %>%
  summarise(suma = sum(valor, na.rm = TRUE)) %>%
  arrange(primera_columna)

# Imprimir los resultados en el formato solicitado
for (i in 1:nrow(suma_por_letra)) {
  cat(suma_por_letra$letra[i], suma_por_letra$suma[i], sep = ",", "\n")
}