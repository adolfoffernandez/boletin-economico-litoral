#### Índice de Cambio Fronterizo ####
options(scipen = 999)

# Fijar directorio de trabajo
setwd("C:/Users/PC/OneDrive/Documentos/Consultoría/Boletín Económico")

# Librerías
library(tidyverse)
library(readxl)
library(lubridate)
library(readr)

#-----------------------------
# Función robusta para limpiar fechas mixtas
#-----------------------------
parse_fecha_mixta <- function(x) {
  x_chr <- as.character(x)
  
  # 1) Intento como número serial de Excel
  fecha_num <- suppressWarnings(as.numeric(x_chr))
  fecha_excel <- as.Date(fecha_num, origin = "1899-12-30")
  
  # 2) Intento como texto en varios formatos
  fecha_texto <- suppressWarnings(parse_date_time(
    x_chr,
    orders = c(
      "d/m/Y",
      "d/m/Y H:M:S",
      "d/m/y",
      "Y-m-d",
      "Y-m-d H:M:S",
      "Y/m/d",
      "Y/m/d H:M:S"
    )
  ))
  
  fecha_texto <- as.Date(fecha_texto)
  
  # 3) Priorizo texto bien parseado; si no, uso Excel
  fecha_final <- fecha_texto
  fecha_final[is.na(fecha_final)] <- fecha_excel[is.na(fecha_final)]
  
  return(fecha_final)
}

#-----------------------------
# Importar base USD/ARS
#-----------------------------
USD_ARG <- read_delim(
  "Datos históricos USD_ARS.csv",
  delim = ",",
  locale = locale(decimal_mark = ",", grouping_mark = "."),
  show_col_types = FALSE
)

#-----------------------------
# Importar base USD/UYU
#-----------------------------
USD_UYU <- read_excel("Cotizacion_uyu_usd.xlsx")

#-----------------------------
# Limpiar USD_ARG
#-----------------------------
USD_ARG <- USD_ARG %>%
  mutate(
    Fecha = dmy(Fecha)
  ) %>%
  select(Fecha, `Último`) %>%
  rename(USD_ARG = `Último`) %>%
  arrange(Fecha)

#-----------------------------
# Limpiar USD_UYU
#-----------------------------
USD_UYU <- USD_UYU %>%
  mutate(
    Fecha = parse_fecha_mixta(Fecha)
  ) %>%
  select(Fecha, `Dólar billete`) %>%
  rename(USD_UYU = `Dólar billete`) %>%
  arrange(Fecha)

#-----------------------------
# Controles rápidos
#-----------------------------
cat("NA en Fecha USD_ARG:", sum(is.na(USD_ARG$Fecha)), "\n")
cat("NA en Fecha USD_UYU:", sum(is.na(USD_UYU$Fecha)), "\n")

# Ver últimas fechas de cada base
print(tail(USD_ARG, 10))
print(tail(USD_UYU, 10))

#-----------------------------
# Cruce de bases
#-----------------------------
USD_CRUZADO <- inner_join(USD_ARG, USD_UYU, by = "Fecha") %>%
  arrange(Fecha) %>%
  mutate(
    ICF = USD_ARG / USD_UYU,
    ICF_base100 = ICF / first(ICF) * 100
  )

#-----------------------------
# Gráfico
#-----------------------------
ggplot(USD_CRUZADO, aes(x = Fecha, y = ICF_base100)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Índice de Cambio Fronterizo",
    x = "Fecha",
    y = "Índice base 100"
  ) +
  theme_minimal()

#-----------------------------
# Exportar base final
#-----------------------------
write_csv2(USD_CRUZADO, "USD_CRUZADO_ICF.csv")

# Vista final
print(tail(USD_CRUZADO, 15))
