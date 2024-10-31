


# Instalación de librerias necesarias 
install.packages("ggplot2")
library(ggplot2)

# Establecer el directorio de trabajo donde tienes guardado el archivo CSV
setwd("D:/Datos de regresion multiple")

# Verificar la carpeta actual de trabajo
getwd()

# Ahora puedes cargar el archivo sin necesidad de escribir la ruta completa
data <- read.csv("superstore.csv")

# Verifica si los datos fueron cargados correctamente
head(data)


# Abrir un cuadro de diálogo para seleccionar el archivo
data <- read.csv(file.choose())

# Ver las primeras filas del archivo
head(data)





# Verificar la estructura de los datos
str(data)

# Verificar si hay valores faltantes en el conjunto de datos
sum(is.na(data))

# Eliminar las filas que contienen valores NA
data <- na.omit(data)

# Crear el modelo de regresión lineal múltiple
modelo <- lm(Profit ~ Sales + Discount + Shipping.Cost, data = data)

# Mostrar el resumen del modelo
summary(modelo)

# Graficar los diagnósticos del modelo
par(mfrow = c(2, 2))
plot(modelo)




# Extraer los coeficientes del modelo
coef_modelo <- data.frame(Variable = names(coef(modelo)), Coeficiente = coef(modelo))

# Graficar los coeficientes
ggplot(coef_modelo, aes(x = Variable, y = Coeficiente)) +
  geom_bar(stat = "identity", fill = "blue") +
  ggtitle("Coeficientes del Modelo de Regresión") +
  theme_minimal()



# Intervalos de confianza para los coeficientes
confint(modelo)








# Ver los primeros registros del conjunto de datos
head(data)

# Resumen estadístico
summary(data)

# Matriz de correlación entre las variables clave
cor(data[, c("Sales", "Discount", "Shipping.Cost", "Quantity", "Profit")])














# Modelo de regresión lineal múltiple
modelo <- lm(Profit ~ Sales + Discount + Shipping.Cost + Quantity, data = data)

# Resumen del modelo
summary(modelo)

# Calcular el AIC del modelo
AIC


# Gráfico de diagnóstico del modelo
par(mfrow = c(2, 2))
plot(modelo)









# Construir el modelo de regresión lineal múltiple
modelo <- lm(Profit ~ Sales + Discount + Shipping.Cost + Quantity, data = data)

# Resumen del modelo para ver los coeficientes, R-cuadrado y p-valores
summary(modelo)


# Calcular el AIC del modelo
AIC(modelo)










# Ver el R-cuadrado del modelo
summary(modelo)$r.squared


# Calcular los intervalos de confianza para los coeficientes
confint(modelo)









# Construir el modelo de regresión lineal múltiple con log(Profit) como variable dependiente
modelo_log <- lm(log(Profit) ~ Sales + Discount + Shipping.Cost + Quantity, data = data)

# Ver el resumen del nuevo modelo
summary(modelo_log)







# Verificar si hay valores negativos o cero en la variable Profit
summary(data$Profit)

# Ver los registros con Profit negativo o cero
data[data$Profit <= 0, ]


# Eliminar filas con Profit <= 0
data <- data[data$Profit > 0, ]



# Aplicar una pequeña constante a Profit
data$Profit_adj <- data$Profit + 1

# Construir el modelo de regresión lineal múltiple con log(Profit_adj)
modelo_log <- lm(log(Profit_adj) ~ Sales + Discount + Shipping.Cost + Quantity, data = data)

# Ver el resumen del nuevo modelo
summary(modelo_log)



# Graficar los diagnósticos del nuevo modelo
par(mfrow = c(2, 2))
plot(modelo_log)
