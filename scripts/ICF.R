#### Indicado Fronterizo #####
options(scipen = 999)

# Fijar directorio de trabajo

setwd("C:/Users/PC/OneDrive/Documentos/Consultoría/Boletín Económico")

# Cargar librerías

library(tidyverse)
library(corrr)
library(readxl)

# Importar base de datos

USD_ARG <- read_delim("Datos históricos USD_ARS.csv", delim=",")
USD_UYU <- read_excel("Cotizacion_uyu_usd.xlsx")

str(USD_ARG)

# Arreglo la base de datos USD_ARG

cols <- c("Último","Apertura","Máximo","Mínimo")

USD_ARG[cols] <- USD_ARG[cols] * 1000

USD_ARG[335:nrow(USD_ARG), cols] <- USD_ARG[335:nrow(USD_ARG), cols]/
  10000000
# Selecciono las columnas de USD_ARG que viy a utilizar

USD_ARG <- select(USD_ARG, Fecha, Último)
USD_ARG <- USD_ARG %>% mutate(Fecha = as.Date(Fecha, format =
                                             "%d.%m.%Y"))%>%
  arrange(Fecha)

# Creamos un data set nuevo

USD_CRUZADO <- inner_join(USD_ARG, USD_UYU, by = "Fecha")

# Renombramos columnas

USD_CRUZADO <- rename(USD_CRUZADO, USD_ARG = 'Último', USD_UYU = 'Dólar billete')

# Creo el Indice de Cambio Fronterizo

USD_CRUZADO <- USD_CRUZADO %>%
  mutate(
    ICF = USD_ARG / USD_UYU,
    ICF_base100 = ICF / first(ICF) * 100
  )

ggplot(USD_CRUZADO, aes(x = Fecha, y = ICF_base100)) +
  geom_line() +
  labs(
    title = "Índice de Cambio Fronterizo",
    x = "Fecha",
    y = "Índice"
  ) +
  theme_minimal()

write_csv2(USD_CRUZADO, "USD_CRUZADO_ICF.csv")




