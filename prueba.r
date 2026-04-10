# grafica de las series de tiempo del producto agropecuario y del consumo de los hogares
library(ggplot2)
library(readr)
# Cargar los datos
df_producto_agropecuario <- read_csv("Base_100_Anios_Construccion.csv")
df_consumo_final <- read_csv("Base_100_Anios_Consumo.csv")

# Unimos las dos series en un solo data frame para graficar
df <- data.frame(
  Año = df_producto_agropecuario$Año,
  Producto_Agropecuario = df_producto_agropecuario$Valor,
  Consumo_Hogares = df_consumo_final$Valor
)
# pasamos año a formato fecha
df$Año <- as.Date(as.character(df$Año), format="%Y")

library(ggplot2)

# Reformas con nombres y colores
reformas <- data.frame(
  inicio = as.Date(c("1936-01-01", "1961-01-01", "1994-01-01", "2016-01-01")),
  fin    = as.Date(c("1960-12-31", "1993-12-31", "2015-12-31", "2025-12-31")),
  nombre = c("Ley 200 de 1936", "Ley 135 de 1961", "Ley 160 de 1994", "Acuerdo de Paz 2016"),
fill = c("#429cf0ff", "#e7933eff", "#75ea51ff", "#de609bff")
)

# Gráfica
ggplot(df, aes(x = Año, y = Producto_Agropecuario)) +
  
  # Sombras por reforma
  geom_rect(data = reformas,
            aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf, fill = nombre),
            alpha = 0.3, inherit.aes = FALSE) +
  
  scale_fill_manual(values = setNames(reformas$fill, reformas$nombre)) +
  
  # Serie
  geom_line(size = 1, color = "black") +
  
  labs(
    title = "Producto Agropecuario y Reformas Agrarias en Colombia",
    subtitle = "Áreas sombreadas corresponden a periodos de reforma agraria",
    x = "Año",
    y = "Producto Agropecuario",
    fill = "Reformas",
    caption = "Fuente: Elaboración propia con base en datos del Banco de la República de Colombia.\nNota: Valores expresados en miles de millones."
  ) + 
  
  theme_classic()


library(dplyr)

df <- df %>%
  mutate(
    reforma_1936 = ifelse(Año >= as.Date("1936-01-01") & Año <= as.Date("1960-12-31"), 1, 0),
    reforma_1961 = ifelse(Año >= as.Date("1961-01-01") & Año <= as.Date("1993-12-31"), 1, 0),
    reforma_1994 = ifelse(Año >= as.Date("1994-01-01") & Año <= as.Date("2015-12-31"), 1, 0),
    reforma_2016 = ifelse(Año >= as.Date("2016-01-01"), 1, 0)
  )

df <- df %>%
  arrange(Año) %>%
  mutate(
    y = log(Producto_Agropecuario),
    dy = y - lag(y)
  )

library(purrr)

H <- 10

resultados <- map_dfr(0:H, function(h){

  df_lp <- df %>%
    mutate(
      y_lead = lead(y, h),
      dy_h = y_lead - y   # respuesta acumulada
    )
  
  modelo <- lm(
    dy_h ~ reforma_1936 + reforma_1961 + reforma_1994 + reforma_2016 +
      lag(dy,1) + lag(dy,2),
    data = df_lp
  )
  
  coefs <- summary(modelo)$coefficients
  
  data.frame(
    h = h,
    beta_1936 = coefs["reforma_1936","Estimate"],
    beta_1961 = coefs["reforma_1961","Estimate"],
    beta_1994 = coefs["reforma_1994","Estimate"],
    beta_2016 = coefs["reforma_2016","Estimate"]
  )
})
library(tidyr)
library(ggplot2)

resultados_long <- resultados %>%
  pivot_longer(-h, names_to = "reforma", values_to = "beta")

ggplot(resultados_long, aes(x = h, y = beta, color = reforma)) +
  geom_line(size = 1) +
  labs(
    title = "Proyecciones Locales: Impacto de Reformas Agrarias",
    x = "Horizonte (años)",
    y = "Efecto sobre el producto agropecuario (log puntos)",
    color = "Reforma"
  ) +
  theme_classic()
