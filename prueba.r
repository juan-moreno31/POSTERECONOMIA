# leer la base
library(readr)
base <- read.csv("Base_100_Anios_Construccion.csv")
head(base)
# verifacamos que no se repitan los años
length(unique(base$Año))
# verifacamos que no haya valores faltantes
sum(is.na(base))
# hacemos un gráfico de la producción agrícola a lo largo del tiempo
library(ggplot2)
ggplot(base, aes(x = Año, y = Valor)) +
  geom_line() +
  geom_point() +
  labs(title = "Producción Agrícola en Colombia (1925-2025)",
       x = "Año",
       y = "Producción Agrícola") +
  theme_minimal()
# hacemos un analisis des descriptivo de la producción agrícola
summary(base$Valor)
# hacemos la prueba ADF para verificar la estacionariedad de la serie
library(tseries)
ts <- ts(base$Valor, start = min(base$Año), end = max(base$Año))
adf.test(ts)
# hacemos la prueba KPSS para verificar la estacionariedad de la serie
adf.test(ts)
# observamos que la serie no es estacionaria, por lo que aplicamos una diferenciación
diff_ts <- diff(ts)
# hacemos la prueba ADF para verificar la estacionariedad de la serie diferenciada
adf.test(diff_ts)
# ahora si, la serie es estacionaria, por lo que podemos proceder a hacer el análisis econométrico
# graficamos la serie diferenciada
ggplot(data.frame(Año = base$Año[-1], Valor = diff_ts), aes(x = Año, y = Valor)) +
  geom_line() +
  geom_point() +
  labs(title = "Diferencia de la Producción Agrícola en Colombia (1925-2025)",
       x = "Año",
       y = "Diferencia de la Producción Agrícola") +
  theme_minimal()

library(urca)
# Prueba de Zivot-Andrews buscando quiebre en ambos (nivel y tendencia)
za_test <- ur.za(diff_ts, model = "both", lag = 1)
summary(za_test)
plot(za_test) # Esto te mostrará gráficamente dónde está el choque
