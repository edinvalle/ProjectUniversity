##INDICE DE HERFINDAHL-HIRSCHMAN
# Crear el DataFrame 
datos <- data.frame(
  Año = c(2008, 2009, 2010, 2011, 2012),
  Gasolina_Super = c(19.8, 20.5, 21.9, 19.7, 20.5),
  Gasolina_Regular = c(11.2, 13.1, 13.5, 13.5, 12.5),
  Diesel = c(38.1, 37.9, 37.7, 39.1, 41.8)
)

# Calcular el índice de Herfindahl-Hirschman
datos$HHI <- (datos$Gasolina_Super / 100)^2 + 
  (datos$Gasolina_Regular / 100)^2 + 
  (datos$Diesel / 100)^2

# Multiplicar por 10,000 para obtener el HHI en escala tradicional
datos$HHI <- datos$HHI * 10000

# Mostrar el DataFrame con el índice de Herfindahl-Hirschman
library(knitr)
kable(datos, caption = "Índice de Herfindahl-Hirschman para los años 2008-2012")

# graficar el índice de Herfindahl-Hirschman 
suppressWarnings({
  ggplot(datos, aes(x = Año, y = HHI)) +
    geom_line(color = "blue", size = 1) +
    labs(title = "Índice de Herfindahl-Hirschman para los años 2008-2012",
         x = "Año", y = "HHI") +
    theme_minimal()
})




# Calcular el HHI por producto (Gasolina Super, Gasolina Regular, Diesel)
hhi_super <- sum((datos$Gasolina_Super / sum(datos$Gasolina_Super))^2) * 10000
hhi_regular <- sum((datos$Gasolina_Regular / sum(datos$Gasolina_Regular))^2) * 10000
hhi_diesel <- sum((datos$Diesel / sum(datos$Diesel))^2) * 10000

# Crear un DataFrame con los resultados
hhi_productos <- data.frame(
  Producto = c("Gasolina Super", "Gasolina Regular", "Diesel"),
  HHI = c(hhi_super, hhi_regular, hhi_diesel)
)

# Mostrar el DataFrame con los resultados del HHI por producto
library(knitr)
kable(hhi_productos, caption = "Índice de Herfindahl-Hirschman por Producto")

