# Cargar las librerías necesarias
library(readxl) 
library(dplyr)   
library(knitr) 
library(tidyr)  
library(ggplot2)
library(lmtest)
library(tseries)
library(gridExtra)
library(psych)


# Leer el archivo Excel 
df <- read_excel("~/Rstudio/EconometriaI/Dataset.xlsx", 
                 sheet = "Hoja1")

# Cambiar los nombres de las columnas 
colnames(df) <- c("Años", "PGS", "PGR", "PD", 
                  "PPC", "CGS", "CGR", "CD", "REZAGOGS")

# Verificar los nombres de las columnas
print(colnames(df))

# Asegurarse de que la columna "Años" esté en formato Date
df <- df %>%
  mutate(Años = as.Date(Años))  


# Verificar los datos después de la limpieza
print(head(df))

# Resumen estadístico de los datos
summary(df %>% select(-Años))


# Graficar los precios de combustibles 
suppressWarnings({
  ggplot(df, aes(x = Años)) +
    geom_line(aes(y = PGS, color = "Gasolina Super"), linewidth = 1) +
    geom_line(aes(y = PGR, color = "Gasolina Regular"), linewidth = 1) +
    geom_line(aes(y = PD, color = "Diésel"), linewidth = 1) +
    labs(title = "Evolución de los precios de combustibles",
         x = "Fecha", y = "Precio (Q)") +
    scale_color_manual(values = c("Gasolina Super" = "blue", 
                                  "Gasolina Regular" = "green", 
                                  "Diésel" = "red")) +
    theme_minimal()
})

# calcular la correlacion
correlacion <- cor(df[, -c(1, 9)])
print(correlacion)

# crear figuras de correlaciones con histograma. 
df_subset <- df[, -c(1, 9)]
pairs.panels( 
  df_subset, 
  pch = 20, 
  stars = TRUE, 
  main = "Matriz de diagrama de dispersión"
)

# Grafica de dispersion de los modelos 
suppressWarnings({
  p1 <- ggplot(df, aes(x = PPC, y = PGS)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(title = "Grafica de dispersión",
         x = "Precio de petróleo", y = "GSuper") +
    theme_minimal()+
    theme(plot.title = element_text(hjust =0.5))
  
  p2 <- ggplot(df, aes(x = PPC, y = PGR)) +
    geom_point(color = "green") +
    geom_smooth(method = "lm", se = TRUE, color = "green") +
    labs(title = "Grafica de dispersión",
         x = "Precio de petróleo", y = "GRegular") +
    theme_minimal()+
    theme(plot.title = element_text(hjust =0.5))
  
  p3 <- ggplot(df, aes(x = PPC, y = PD)) +
    geom_point(color = "red") +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = "Grafica de dispersión",
         x = "Precio de petróleo", y = "Diésel") +
    theme_minimal()+
    theme(plot.title = element_text(hjust =0.5))
  
  grid.arrange(p1, p2, p3, ncol = 3)
})

# MODELO 1 Dependiente de precios de gasolinas(PG), Independiente Consumo de gasolinas super (CGS), Precio de petroleo (PPC)
# Ajustar modelo de rmúltiple
modelo1 <- lm(PGS ~ PPC + CGS, data = df)
summary(modelo1)
# Obtener los residuos del modelo
residuos1 <- residuals(modelo1)

# 1. NORMALIDAD DE LOS RESIDUOS

# Histograma
hist(residuos1, breaks = 15, 
     main = "Histograma de Residuos de GS", 
     xlab = "Residuos")
# Prueba de Jarque-Bera
jarque.bera.test(residuos1)

# 2. AUTOCORRELACIÓN DE RESIDUOS

# Prueba de Durbin-Watson
dwtest(modelo1)
# Correlograma (ACF) de los residuos
acf(residuos1, main = "Correlograma de Residuos de GS")

# Ajuste de autocorrelación rezago
rezago1 <- lm(REZAGOGS ~ PPC + CGS, data = df)
summary(rezago1)
# Obtener los residuos del modelo
residuos_rezago1 <- residuals(rezago1)
dwtest(rezago1)
# Correlograma (ACF) de los residuos
acf(residuos_rezago1, 
    main = "Correlograma de Residuos con Rezago de GS")

# 3. HETEROCEDASTICIDAD

# Prueba de Breusch-Pagan
bptest(modelo1)
# Prueba de White
bptest(modelo1, ~ PPC + CGS + I(PPC^2) + I(CGS^2), data = df)



# MODELO 2 Dependiente de precios de gasolinas(PG), Independiente Consumo de gasolinas Regular (CGR), Precio de petroleo (PPC)
# Ajustar modelo de rmúltiple
modelo2 <- lm(PGR ~ PPC + CGR, data = df)
summary(modelo2)
# Obtener los residuos del modelo
residuos2 <- residuals(modelo2)

# 1. NORMALIDAD DE LOS RESIDUOS
# Histograma 
hist(residuos2, breaks = 15, 
     main = "Histograma de Residuos de GR", xlab = "Residuos")

# Prueba de Jarque-Bera 
jarque.bera.test(residuos2)

# 2. AUTOCORRELACIÓN DE RESIDUOS

# Prueba de Durbin-Watson
dwtest(modelo2)
# Correlograma (ACF) de los residuos
acf(residuos2, main = "Correlograma de Residuos de GR")

# Ajuste de autocorrelación rezago
rezago2 <- lm(REZAGOGS ~ PPC + CGR, data = df)
summary(rezago2)
# Obtener los residuos del modelo
residuos_rezago2 <- residuals(rezago2)
dwtest(rezago2)
# Correlograma (ACF) de los residuos
acf(residuos_rezago2, main = "Correlograma de Residuos con Rezago de GR")

# 3. HETEROCEDASTICIDAD

# Prueba de Breusch-Pagan
bptest(modelo2)
# Prueba de White
bptest(modelo2, ~ PPC + CGR + I(PPC^2) + I(CGR^2), 
       data = df)



# MODELO 3 Dependiente de precios de gasolinas(PG), Independiente Consumo de diésel (CD), Precio de petroleo (PPC)

# Ajustar modelo de rmúltiple
modelo3 <- lm(PD ~ PPC + CD, data = df)
summary(modelo3)
# Obtener los residuos del modelo
residuos3 <- residuals(modelo3)

# 1. NORMALIDAD DE LOS RESIDUOS

# Histograma 
hist(residuos3, breaks = 15, 
     main = "Histograma de Residuos de diésel", 
     xlab = "Residuos")
# Prueba de Jarque-Bera 
jarque.bera.test(residuos3)

# 2. AUTOCORRELACIÓN DE RESIDUOS

# Prueba de Durbin-Watson
dwtest(modelo3)
# Correlograma (ACF) de los residuos
acf(residuos3, main = "Correlograma de Residuos de diésel")

# Ajuste de autocorrelación
rezago3 <- lm(REZAGOGS ~ PPC + CD, data = df)
summary(rezago3)
# Obtener los residuos del modelo
residuos_rezago3 <- residuals(rezago3)
# Prueba de Durbin-Watson
dwtest(rezago3)
# Correlograma (ACF) de los residuos
acf(residuos_rezago3, 
    main = "Correlograma de Residuos con Rezago de diésel")


# 3. HETEROCEDASTICIDAD

# Prueba de Breusch-Pagan
bptest(modelo3)
# Prueba de White
bptest(modelo3, ~ PPC + CD + I(PPC^2) + I(CD^2), 
       data = df)


