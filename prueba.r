library(dplyr)
library(pracma)


set.seed(12345)

n_pruebas = 50

errores_media1 <- c()
errores_media2 <- c()
errores_media3 <- c()
errores_media4 <- c()
errores_media5 <- c()
errores_media6 <- c()
errores_media7 <- c()
errores_media8 <- c()
errores_media9 <- c()
errores_media10 <- c()
errores_media11 <- c()
errores_media12 <- c()
errores_media13 <- c()
errores_media14 <- c()
errores_media15 <- c()

n=2000

for(i in 1:n_pruebas){
  print(i)
  borra_conjunto <- datos_normalizados
  a_borrar = 1
  coordenadas <- list()
  while (a_borrar<=n) {
    borra_fila <- sample(1:nrow(borra_conjunto),1)
    borra_columna <- sample(3:ncol(borra_conjunto), 1)
    if(!is.na(datos_normalizados[borra_fila,borra_columna])){
      borra_conjunto[borra_fila,borra_columna] <- NA
      coordenadas <- append(coordenadas,list(c(borra_fila,borra_columna)))
      a_borrar <- a_borrar +1
    }
  }

  borra_matriz_de_distancias <- calcular_matriz_distancias(borra_conjunto)
  borra_paises_cercanos <- paises_mas_cercanos(borra_matriz_de_distancias, n = 177)
  
  #coordenadas_na <- which(is.na(borra_conjunto), arr.ind = TRUE)
  
  borra_parejas <- which(is.na(borra_conjunto), arr.ind = TRUE)
  borra_parejas[,1] <- borra_conjunto[borra_parejas[,1],1]
  borra_parejas[,2] <- names(borra_conjunto)[as.integer(borra_parejas[,2])]
  borra_parejas <- split(borra_parejas, seq(nrow(borra_parejas)))
  
  nuevo_conjunto1 <- media_method(borra_parejas,borra_conjunto,1,
                                 borra_paises_cercanos)
  
  nuevo_conjunto2 <- media_method(borra_parejas,borra_conjunto,2,
                                  borra_paises_cercanos)
  
  nuevo_conjunto3 <- media_method(borra_parejas,borra_conjunto,3,
                                  borra_paises_cercanos)
  
  nuevo_conjunto4 <- media_method(borra_parejas,borra_conjunto,4,
                                  borra_paises_cercanos)
  
  nuevo_conjunto5 <- media_method(borra_parejas,borra_conjunto,5,
                                  borra_paises_cercanos)
  
  nuevo_conjunto6 <- media_method(borra_parejas,borra_conjunto,6,
                                  borra_paises_cercanos)
  
  nuevo_conjunto7 <- media_method(borra_parejas,borra_conjunto,7,
                                  borra_paises_cercanos)
  
  nuevo_conjunto8 <- media_method(borra_parejas,borra_conjunto,8,
                                  borra_paises_cercanos)
  nuevo_conjunto9 <- media_method(borra_parejas,borra_conjunto,9,
                                  borra_paises_cercanos)
  
  nuevo_conjunto10 <- media_method(borra_parejas,borra_conjunto,10,
                                  borra_paises_cercanos)
  
  nuevo_conjunto11 <- media_method(borra_parejas,borra_conjunto,11,
                                  borra_paises_cercanos)
  
  nuevo_conjunto12 <- media_method(borra_parejas,borra_conjunto,12,
                                  borra_paises_cercanos)
  
  nuevo_conjunto13 <- media_method(borra_parejas,borra_conjunto,13,
                                  borra_paises_cercanos)
  nuevo_conjunto14 <- media_method(borra_parejas,borra_conjunto,14,
                                  borra_paises_cercanos)
  
  nuevo_conjunto15 <- media_method(borra_parejas,borra_conjunto,15,
                                   borra_paises_cercanos)
  
  vector1 <- c()
  vector2 <- c()
  vector3 <- c()
  vector4 <- c()
  vector5 <- c()
  vector6 <- c()
  vector7 <- c()
  vector8 <- c()
  vector9 <- c()
  vector10 <- c()
  vector11 <- c()
  vector12 <- c()
  vector13 <- c()
  vector14 <- c()
  vector15 <- c()
  for(i in 1:n){
    vector1 <- c(vector1, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto1[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector2 <- c(vector2, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto2[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector3 <- c(vector3, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto3[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector4 <- c(vector4, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto4[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector5 <- c(vector5, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto5[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector6 <- c(vector6, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto6[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector7 <- c(vector7, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto7[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector8 <- c(vector8, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto8[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector9 <- c(vector9, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto9[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector10 <- c(vector10, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto10[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector11 <- c(vector11, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto11[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector12 <- c(vector12, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto12[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector13 <- c(vector13, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto13[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector14 <- c(vector14, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto14[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector15 <- c(vector15, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto15[coordenadas[[i]][1], coordenadas[[i]][2]])
  }
  error1 <- norm(vector1, type = "2")
  error2 <- norm(vector2, type = "2")
  error3 <- norm(vector3, type = "2")
  error4 <- norm(vector4, type = "2")
  error5 <- norm(vector5, type = "2")
  error6 <- norm(vector6, type = "2")
  error7 <- norm(vector7, type = "2")
  error8 <- norm(vector8, type = "2")
  error9 <- norm(vector9, type = "2")
  error10 <- norm(vector10, type = "2")
  error11 <- norm(vector11, type = "2")
  error12 <- norm(vector12, type = "2")
  error13 <- norm(vector13, type = "2")
  error14 <- norm(vector14, type = "2")
  error15 <- norm(vector15, type = "2")
  
  print(c(error1,error2,error3,error4,error5,error6,error7,error8,error9,error10,
          error11,error12,error13,error14,error15))
  errores_media1 <- c(errores_media1,error1)
  errores_media2 <- c(errores_media2,error2)
  errores_media3 <- c(errores_media3,error3)
  errores_media4 <- c(errores_media4,error4)
  errores_media5 <- c(errores_media5,error5)
  errores_media6 <- c(errores_media6,error6)
  errores_media7 <- c(errores_media7,error7)
  errores_media8 <- c(errores_media8,error8)
  errores_media9 <- c(errores_media9,error9)
  errores_media10 <- c(errores_media10,error10)
  errores_media11 <- c(errores_media11,error11)
  errores_media12 <- c(errores_media12,error12)
  errores_media13 <- c(errores_media13,error13)
  errores_media14 <- c(errores_media14,error14)
  errores_media15 <- c(errores_media15,error15)
}

mean(errores_media)
sd(errores_media)













################### USANDO P PARALELA ######################3




library(parallel)



set.seed(12345)

n_pruebas = 100

errores_media1 <- c()
errores_media2 <- c()
errores_media3 <- c()
errores_media4 <- c()
errores_media5 <- c()
errores_media6 <- c()
errores_media7 <- c()
errores_media8 <- c()
errores_media9 <- c()
errores_media10 <- c()

n = 1000

# Define el número de núcleos a utilizar (ajusta según tus necesidades)
num_cores <- 4

# Crea un clúster de clústeres paralelos
cl <- makeCluster(num_cores)

# Ejecuta las pruebas en paralelo
results <- mclapply(1:n_pruebas, function(i) {
  print(i)
  borra_conjunto <- datos_normalizados
  # Resto del código aquí, manteniendo la estructura original
  a_borrar = 1
  coordenadas <- list()
  while (a_borrar<=n) {
    borra_fila <- sample(1:nrow(borra_conjunto),1)
    borra_columna <- sample(3:ncol(borra_conjunto), 1)
    if(!is.na(datos_normalizados[borra_fila,borra_columna])){
      borra_conjunto[borra_fila,borra_columna] <- NA
      coordenadas <- append(coordenadas,list(c(borra_fila,borra_columna)))
      a_borrar <- a_borrar +1
    }
  }
  
  borra_matriz_de_distancias <- calcular_matriz_distancias(borra_conjunto)
  borra_paises_cercanos <- paises_mas_cercanos(borra_matriz_de_distancias, n = 100)
  
  #coordenadas_na <- which(is.na(borra_conjunto), arr.ind = TRUE)
  
  borra_parejas <- which(is.na(borra_conjunto), arr.ind = TRUE)
  borra_parejas[,1] <- borra_conjunto[borra_parejas[,1],1]
  borra_parejas[,2] <- names(borra_conjunto)[as.integer(borra_parejas[,2])]
  borra_parejas <- split(borra_parejas, seq(nrow(borra_parejas)))
  
  nuevo_conjunto1 <- media_method(borra_parejas,borra_conjunto,1,
                                  borra_paises_cercanos)
  
  nuevo_conjunto2 <- media_method(borra_parejas,borra_conjunto,2,
                                  borra_paises_cercanos)
  
  nuevo_conjunto3 <- media_method(borra_parejas,borra_conjunto,3,
                                  borra_paises_cercanos)
  
  nuevo_conjunto4 <- media_method(borra_parejas,borra_conjunto,4,
                                  borra_paises_cercanos)
  
  nuevo_conjunto5 <- media_method(borra_parejas,borra_conjunto,5,
                                  borra_paises_cercanos)
  
  nuevo_conjunto6 <- media_method(borra_parejas,borra_conjunto,6,
                                  borra_paises_cercanos)
  
  nuevo_conjunto7 <- media_method(borra_parejas,borra_conjunto,7,
                                  borra_paises_cercanos)
  
  nuevo_conjunto8 <- media_method(borra_parejas,borra_conjunto,8,
                                  borra_paises_cercanos)
  nuevo_conjunto9 <- media_method(borra_parejas,borra_conjunto,9,
                                  borra_paises_cercanos)
  
  nuevo_conjunto10 <- media_method(borra_parejas,borra_conjunto,10,
                                   borra_paises_cercanos)
  
  vector1 <- c()
  vector2 <- c()
  vector3 <- c()
  vector4 <- c()
  vector5 <- c()
  vector6 <- c()
  vector7 <- c()
  vector8 <- c()
  vector9 <- c()
  vector10 <- c()
  for(i in 1:n){
    vector1 <- c(vector1, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto1[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector2 <- c(vector2, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto2[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector3 <- c(vector3, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto3[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector4 <- c(vector4, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto4[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector5 <- c(vector5, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto5[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector6 <- c(vector6, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto6[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector7 <- c(vector7, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto7[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector8 <- c(vector8, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto8[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector9 <- c(vector9, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto9[coordenadas[[i]][1], coordenadas[[i]][2]])
    vector10 <- c(vector10, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto10[coordenadas[[i]][1], coordenadas[[i]][2]])
  }
  error1 <- norm(vector1, type = "2")
  error2 <- norm(vector2, type = "2")
  error3 <- norm(vector3, type = "2")
  error4 <- norm(vector4, type = "2")
  error5 <- norm(vector5, type = "2")
  error6 <- norm(vector6, type = "2")
  error7 <- norm(vector7, type = "2")
  error8 <- norm(vector8, type = "2")
  error9 <- norm(vector9, type = "2")
  error10 <- norm(vector10, type = "2")
  
  print(c(error1,error2,error3,error4,error5,error6,error7,error8,error9,error10))
  errores_media1 <- c(errores_media1,error1)
  errores_media2 <- c(errores_media2,error2)
  errores_media3 <- c(errores_media3,error3)
  errores_media4 <- c(errores_media4,error4)
  errores_media5 <- c(errores_media5,error5)
  errores_media6 <- c(errores_media6,error6)
  errores_media7 <- c(errores_media7,error7)
  errores_media8 <- c(errores_media8,error8)
  errores_media9 <- c(errores_media9,error9)
  errores_media10 <- c(errores_media10,error10)
  
  return(c(error1, error2, error3, error4, error5, error6, error7, error8, error9, error10))
}, mc.cores = num_cores)

# Cierra el clúster de clústeres paralelos
stopCluster(cl)

# Procesa los resultados si es necesario
# Por ejemplo, puedes calcular la media de errores para cada prueba
mean_errors <- colMeans(do.call(rbind, results))

# Visualiza los resultados
print(mean_errors)





#########################################################################





for(i in 1:n_pruebas){
  borra_conjunto <- datos_completos
  borra_fila <- sample(1:nrow(borra_conjunto), size = n, replace = TRUE)
  borra_columna <- sample(3:ncol(borra_conjunto), size = n, replace = TRUE)
  coordenadas <- cbind(borra_fila, borra_columna)
  #borra_conjunto[coordenadas] <- NA
  coordenadas <- unique(coordenadas)
  
  
  for (i in 1:nrow(coordenadas)) {
    x <- coordenadas[i, "borra_fila"]
    y <- coordenadas[i, "borra_columna"]
    borra_conjunto[x, y] <- NA
  }
  
  # Calcular la nueva matriz de distancias, y los paises más cercanos:
  borra_matriz_de_distancias <- calcular_matriz_distancias(borra_conjunto)
  borra_paises_cercanos <- paises_mas_cercanos(borra_matriz_de_distancias, n = 100)
  
  borra_parejas <- coordenadas 
  borra_parejas[,1] <- borra_conjunto[borra_parejas[,1],1]
  borra_parejas[,2] <- names(borra_conjunto)[as.integer(borra_parejas[,2])]
  borra_parejas <- split(borra_parejas, seq(nrow(borra_parejas)))
  
  nuevo_conjunto <- media_method(borra_parejas,borra_conjunto,4,
                                 borra_paises_cercanos)
  
  vector <- c()
  for(i in 1000){
    vector <- c(vector,datos_completos[coordenadas[i,1],coordenadas[i,2]]-
                  nuevo_conjunto[coordenadas[i,1],coordenadas[i,2]])
  }
  error <- norm(vector, type = "2")
  print(error)
  errores_media <- c(errores_media,error)
}

medias <- c(mean(errores_media1),mean(errores_media2),
             mean(errores_media3),mean(errores_media4),
             mean(errores_media5),mean(errores_media6),
             mean(errores_media7),mean(errores_media8),
             mean(errores_media9),mean(errores_media10),
            mean(errores_media11),mean(errores_media12),
            mean(errores_media13),mean(errores_media14),
            mean(errores_media15))

sds <- c(sd(errores_media1), sd(errores_media2),
                  sd(errores_media3), sd(errores_media4),
                  sd(errores_media5), sd(errores_media6),
                  sd(errores_media7), sd(errores_media8),
                  sd(errores_media9), sd(errores_media10),
         sd(errores_media11), sd(errores_media12),
         sd(errores_media13), sd(errores_media14),sd(errores_media15))

ks <- 1:15

kmeans <- data.frame(medias,sds,ks)

kmeans_data <- data.frame(
  k = 1:10,
  mean_error = c(800.4015, 695.1095, 661.1075, 645.4030, 637.4403, 633.8915, 632.4955, 631.1095, 630.6833, 631.1434),
  sd_error = c(29.64377, 24.48339, 22.33951, 21.69335, 20.96534, 20.56081, 20.59624, 20.07304, 20.03883, 19.80258)
)

kmeans_data <- data.frame(
  k = 1:15,
  mean_error = c(
    531.7570, 461.8075, 441.8825, 434.5986, 430.8879, 428.2288, 427.4609,
    426.8785, 427.0262, 427.5133, 427.6445, 428.4098, 429.0830, 429.9230, 431.0118
  ),
  sd_error = c(
    25.94733, 23.11955, 21.93208, 20.53330, 20.71106, 20.48461, 20.37751,
    20.02341, 20.07289, 20.04055, 19.26466, 19.42443, 19.77871, 19.85235, 19.80898
  )
)

kmeans_data <- data.frame(
  k = 1:15,
  mean_error = c(1115.1114, 969.5318, 926.0134, 908.9510, 898.1270, 892.4782, 889.3059,
                 887.9663, 887.7213, 887.6212, 888.3870, 889.2693, 890.5570, 891.9987,
                 893.4302),
  sd_error = sds <- c(29.88213, 22.73668, 23.17033, 22.61885, 22.42113, 22.68998, 21.46450,
                      20.97281, 20.71929, 20.26914, 19.90858, 19.72101, 20.04550, 20.71350,
                      21.39269)
  
)

bar_plot <- ggplot(kmeans_data, aes(x = factor(k), y = mean_error, fill = factor(k))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_error - sd_error, ymax = mean_error + sd_error), width = 0.2) +
  labs(x = "k", y = "Mean Error", fill = "k") +
  ylim(800, 1000)+
  theme_minimal() +
  theme(legend.position = "none")

# Mostrar el gráfico
print(bar_plot)


# Crear el gráfico de puntos con zoom en el eje y
point_plot_zoomed <- ggplot(kmeans_data, aes(x = factor(k), y = mean_error, color = factor(k))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_error - sd_error, ymax = mean_error + sd_error), width = 0.2) +
  labs(x = "k", y = "Mean Error", color = "k") +
  ylim(850, 1000) +  # Ajustar el rango del eje y aquí
  theme_minimal() +
  theme(legend.position = "none")

# Mostrar el gráfico de puntos con zoom
print(point_plot_zoomed)

#Este código generará un gráfico de barras con barras representando el error medio y las líneas de error representando la desviación estándar para cada valor de "k". Puedes personalizar el gráfico según tus preferencias si lo deseas.

# Create a data frame with the new values
new_data <- data.frame(
  k = 1:10,
  mean_error = c(563.7800, 489.2856, 466.5387, 454.9538, 449.1294, 446.8645, 629.4241, 628.9381, 629.4241, 628.9381),
  sd_error = c(27.25017, 23.54351, 21.38373, 19.68920, 19.16379, 19.60739, 27.71464, 27.60764, 27.71464, 27.60764)
)

# Create the plot with zoom on the y-axis
point_plot_zoomed_new <- ggplot(new_data, aes(x = factor(k), y = mean_error, color = factor(k))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_error - sd_error, ymax = mean_error + sd_error), width = 0.2) +
  labs(x = "k", y = "Mean Error", color = "k") +
  ylim(400, 700) +  # Adjust the y-axis range here
  theme_minimal() +
  theme(legend.position = "none")

# Show the plot with zoom
print(point_plot_zoomed_new)







#write.csv(kmeans, file = "errores100.csv", row.names = FALSE)

alpha <- 0.05  # Nivel de confianza
n <- n_pruebas  # Tamaño de la muestra (reemplaza con el valor adecuado)
z <- qnorm(1 - alpha / 2)  # Valor crítico de Z para el nivel de confianza

kmeans$CI_Lower <- kmeans$medias - z * (kmeans$sds / sqrt(n))
kmeans$CI_Upper <- kmeans$medias + z * (kmeans$sds / sqrt(n))

# Crear el gráfico de barras con intervalos de confianza
barplot(kmeans$medias, names.arg = kmeans$ks, ylim = c(0, max(kmeans$CI_Upper) + 2), 
        col = "lightblue", main = "Gráfico de Barras con Intervalos de Confianza",
        xlab = "Variables", ylab = "Medias", border = "black")

arrows(
  x0 = 1:nrow(kmeans),
  y0 = kmeans$CI_Lower,
  x1 = 1:nrow(kmeans),
  y1 = kmeans$CI_Upper,
  angle = 90,
  code = 3,
  length = 0.1
)

mean(errores_media)
sd(errores_media)


################# quantile method #################

mean_with_na <- function(x) {
  if (!any(is.na(x))) {
    return(trapz(x))
  } else {
    return(NA)
  }
}

errores_quantile <- c()

for(i in 1:n_pruebas){
  print(i)
  borra_conjunto <- datos_normalizados
  borra_conjunto_aux <- borra_conjunto
  a_borrar = 1
  coordenadas <- list()
  while (a_borrar<=n) {
    borra_fila <- sample(1:nrow(borra_conjunto),1)
    borra_columna <- sample(3:ncol(borra_conjunto), 1)
    if(!is.na(datos_normalizados[borra_fila,borra_columna])){
      borra_conjunto[borra_fila,borra_columna] <- NA
      coordenadas <- append(coordenadas,list(c(borra_fila,borra_columna)))
      a_borrar <- a_borrar +1
    }
  }
  
  
  borra_distancias_indicadores <- calcular_matriz_distancias_indicadores(borra_conjunto)
  print(dim(borra_distancias_indicadores))
  borra_indicadores_cercanos <- indicadores_mas_cercanos(borra_distancias_indicadores, n = 100)

  borra_parejas <- which(is.na(borra_conjunto), arr.ind = TRUE)
  borra_parejas[,1] <- borra_conjunto[borra_parejas[,1],1]
  borra_parejas[,2] <- names(borra_conjunto)[as.integer(borra_parejas[,2])]
  borra_parejas <- split(borra_parejas, seq(nrow(borra_parejas)))
  
  
  borra_rankings_indicadores <- borra_conjunto_aux %>%
    group_by(GeoAreaName) %>%
    summarise(across(`5.5.1: Proportion of seats held by women in national parliaments (% of total number of seats)`:`17.8.1: Proportion of individuals using the Internet (%)`, mean_with_na))
  borra_rankings_indicadores <- as.data.frame(borra_rankings_indicadores)
  borra_ranking_paises <- matrix(NA_character_, nrow = nrow(borra_rankings_indicadores), ncol = ncol(borra_rankings_indicadores))
  
  for (j in 2:ncol(borra_rankings_indicadores)) {
    borra_ranking_indices <- order(borra_rankings_indicadores[, j], decreasing = TRUE)
    borra_ranking_paises[, j] <- borra_rankings_indicadores$GeoAreaName
    borra_ranking_paises[is.na(borra_rankings_indicadores[, j]), j] <- NA
    borra_ranking_paises[, j] <- borra_ranking_paises[borra_ranking_indices, j]
  }
  borra_ranking_paises <- borra_ranking_paises[, -1]
  colnames(borra_ranking_paises) <- colnames(borra_rankings_indicadores)[-1]
  
  nuevo_conjunto <- quantile_method(borra_conjunto,borra_parejas,
                                    borra_indicadores_cercanos,borra_ranking_paises,borra_conjunto_aux)
  
  vector <- c()
  for(j in 1:n){
    print(j)
    vector <- c(vector, datos_normalizados[coordenadas[[i]][1], coordenadas[[i]][2]] - nuevo_conjunto[coordenadas[[i]][1], coordenadas[[i]][2]])
  }
  error <- norm(na.omit(vector), type = "2")
  print(error)
  errores_quantile <- c(errores_quantile,error)
}

mean(errores_quantile)
sd(errores_quantile)

