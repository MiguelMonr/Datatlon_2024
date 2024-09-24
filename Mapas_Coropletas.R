library(sf)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(scales)


df1 <- st_read("/Users/miguelmonreal/Desktop/VisualizacionInformacion/Notebooks/MapaMexico.geojson")

df1
ggplot(df1) +
  geom_sf() + #* geom_sf es el metodo para graficar. En mapping ponemos las variables a corolear
  coord_sf() 

str(df1)

#Sin geometria
sin_geometria <- st_drop_geometry(df1)
sin_geometria_limpio <- sin_geometria %>% select(AREA,PERIMETER,ENTIDAD,CAPITAL)
sin_geometria_limpio

#Cargano con poblaciones
poblaciones <- read.csv("/Users/miguelmonreal/Desktop/VisualizacionInformacion/nueva_poblacion2.csv")
str(poblaciones)

# Supongamos que tu dataframe se llama df
# Convertir las columnas a numéricas en el dataframe poblaciones
poblaciones$Total <- as.numeric(gsub(",", "", poblaciones$Total))
poblaciones$Hombres <- as.numeric(gsub(",", "", poblaciones$Hombres))
poblaciones$Mujeres <- as.numeric(gsub(",", "", poblaciones$Mujeres))

# Verifica el tipo de datos
str(poblaciones)
summary(poblaciones)

#Identificando los limites
fila_mas_baja_total <- poblaciones[which.min(poblaciones$Total), ]

# Encuentra la fila con el registro más bajo en la columna Hombres
fila_mas_baja_hombres <- poblaciones[which.min(poblaciones$Hombres), ]

# Encuentra la fila con el registro más bajo en la columna Mujeres
fila_mas_baja_mujeres <- poblaciones[which.min(poblaciones$Mujeres), ]

# Imprimir las filas
print(fila_mas_baja_total)
print(fila_mas_baja_hombres)
print(fila_mas_baja_mujeres)

#Capitalizando 
poblaciones$Entidad.federativa <- toupper(poblaciones$Entidad.federativa)
poblaciones

#Para edoMex
poblaciones$Entidad.federativa[poblaciones$Entidad.federativa == "MÉXICO"] <- "EDO. MEX"
sin_geometria_limpio$ENTIDAD[sin_geometria_limpio$ENTIDAD == "MEXICO"] <- "EDO. MEX"
sin_geometria_limpio$ENTIDAD[sin_geometria_limpio$ENTIDAD == "MICHOACAN DE OCAMPO"] <- "MICHOACÁN DE OCAMPO"
sin_geometria_limpio$ENTIDAD[sin_geometria_limpio$ENTIDAD == "SAN LUIS POTOSI"] <- "SAN LUIS POTOSÍ"
sin_geometria_limpio$ENTIDAD[sin_geometria_limpio$ENTIDAD == "NUEVO LEON"] <- "NUEVO LEÓN"
sin_geometria_limpio$ENTIDAD[sin_geometria_limpio$ENTIDAD == "YUCATAN"] <- "YUCATÁN"


#Para Queretaro
poblaciones$Entidad.federativa[poblaciones$Entidad.federativa == "QUERÉTARO"] <- "QUERETARO DE ARTEAGA"

#Prueba del join
# Realizar el join
resultado <- merge(sin_geometria_limpio, 
                   poblaciones[, c("Entidad.federativa", "Total", "Hombres", "Mujeres")], 
                   by.x = "ENTIDAD", 
                   by.y = "Entidad.federativa", 
                   all.x = TRUE)

# Ver el resultado
head(resultado)


#Par ver si sirven
# Asegúrate de que la columna ENTIDAD sea del mismo tipo en ambos dataframes
resultado <- resultado %>%
  mutate(ENTIDAD = as.character(ENTIDAD)) # Asegúrate de que sea del mismo tipo

# Realiza el join para agregar la geometría
resultado_final <- resultado %>%
  left_join(df1 %>% select(ENTIDAD, geometry), by = "ENTIDAD")

# Verifica que la columna geometry se haya añadido correctamente
if("geometry" %in% colnames(resultado_final)) {
  # Convierte resultado_final a un objeto sf utilizando la geometría existente
  resultado_final <- st_as_sf(resultado_final, sf_column_name = "geometry", crs = st_crs(df1))
} else {
  stop("No se pudo encontrar la columna 'geometry' en resultado_final.")
}

# Verifica el resultado
summary(resultado_final)

#Prueba
resultado_final <- resultado_final %>%
  mutate(
    poblacion_categoria = cut(
      Total,
      breaks = c(731391, 1500000, 3000000, 5000000, 10000000, 17000000),
      labels = c("731K - 1.5M", "1.5M - 3M", "3M - 5M", "5M - 10M", "10M - 17M"),
      include.lowest = TRUE
    )
  )

# Crear el mapa de coropletas usando ggplot2

ggplot(data = resultado_final) +
  geom_sf(mapping = aes(fill = poblacion_categoria), color = "white") +  # Usar la variable categórica
  scale_fill_manual(
    values = c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#e41a1c"),  # Colores manuales
    name = "Población"
  ) +
  labs(title = "Mapa de Coropletas de la Población por Estado",
       subtitle = "Total de Población por Estado en México",
       caption = "Fuente: Datos del usuario") +
  theme_minimal() +
  theme(legend.position = "right")  

resultado_final <- resultado_final %>%
  mutate(
    poblacion_categoria = cut(
      Total,
      breaks = c(731391, 1500000, 3000000, 5000000, 10000000, 17000000),
      labels = c("731K - 1.5M", "1.5M - 3M", "3M - 5M", "5M - 10M", "10M - 17M"),
      include.lowest = TRUE
    )
  )

# Crear el mapa de coropletas usando ggplot2
ggplot(data = resultado_final) +
  geom_sf(mapping = aes(fill = Total), color = "white") + # Usar Total directamente
  scale_fill_gradient(low = "#ffffb2", high = "#e41a1c", name = "Población Total") + # Gradiente de color amarillo a rojo
  labs(title = "Mapa de Coropletas de la Población por Estado",
       subtitle = "Total de Población por Estado en México",
       caption = "Fuente: Datos del usuario") +
  theme_minimal() +
  theme(legend.position = "right")








#Aqui inicia nueva prueba
# 1. Convertir la población total en millones
# 1. Convertir la población total en millones
library(ggplot2)
library(dplyr)
library(scales)  # Para formatear los números

# 1. Convertir la población total en millones
resultado_final <- resultado_final %>%
  mutate(
    Total_millones = Total / 1e6  # Convertir la población total a millones
  )

# 2. Crear el mapa de coropletas usando ggplot2 con escala continua
ggplot(data = resultado_final) +
  geom_sf(mapping = aes(fill = Total_millones), color = "white") +  # Usar la columna continua en millones
  scale_fill_gradient(
    low = "#ffffb2", high = "#e41a1c",  # Colores de bajo a alto
    name = "Población (millones)",  # Título de la leyenda
    labels = label_number(accuracy = 1, suffix = "M")  # Formatear en millones
  ) +
  labs(title = "Mapa de Coropletas de la Población por Estado",
       subtitle = "Total de Población por Estado en México",
       caption = "Fuente: Datos del usuario") +
  theme_minimal() +
  theme(legend.position = "right")









