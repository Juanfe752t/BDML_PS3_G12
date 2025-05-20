#------------------------------------------------------------------------------#
#                     Problem Set 3: Making Money with ML                      #
#                           Data import and cleaning                           #     
#------------------------------------------------------------------------------#

#1.Preliminaries

# Cargar pacman (contiene la función p_load)
library(pacman) 
library(stringr)

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(rio, ## read datasets
       stringi, # Manipular cadenas de texto
       tidyverse, # Manipular dataframes
       tm,   # para Text Mining
       tidytext, #Para tokenización
       stopwords,  # consultar stopwords
       tidymodels, # modelos de machine learning
       sf, # datos espaciales
       spatialsample # validación cruzada espacial
) 

#2.  Data import
train <- read.csv("stores/train.csv") # 38644 observaciones
test <- read.csv("stores/test.csv") # 10286 observaciones

#3. Exploración de la base

stargazer(train,type="text")
vis_dat(train)
colSums(is.na(train)) # Variable con missing: surface_total surface_covered rooms bathrooms title descriptions
colSums(is.na(test)) # Variable con missing: surface_total surface_covered rooms bathrooms title descriptions

#4. Imputación de valores faltantes


# Todo en minuscula
train <- train %>%
  mutate(description = str_to_lower(description))
# Eliminamos tildes
train <- train %>%
  mutate(description = stringi::stri_trans_general(description, "Latin-ASCII"))
# Eliminamos caracteres especiales
train <- train %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))
# Eliminamos espacios extras
train <- train %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))

train$description[2]


#### 

# de lo contrario, se mantiene el valor original de property_type
train <- train %>%
  mutate(property_type_2 = ifelse(grepl("casa", description), "Casa", property_type))

# Se repite el caso anterior pero ahora buscamos apartamento o apto.
train <- train %>%
  mutate(property_type_2 = ifelse(grepl("apto|apartamento", description), "Apartamento", property_type_2))


## Bedroom  

train <- train %>%
  mutate(
    alcoba_raw = str_extract(description, "\\d+\\s*(alcobas?|cuartos?|habitaciones?)"),
    alcobas = str_extract(alcoba_raw, "\\d+") %>% as.integer()
  )

## tiene terraza 

train <- train %>%
  mutate(tiene_terraza = if_else(str_detect(description, "terraza"), 1, 0))


## Numero de baños 

train <- train %>%
  mutate(
    baño_raw = str_extract(description, "\\d+\\s*(baño?)"),
    baño = str_extract(alcoba_raw, "\\d+") %>% as.integer()
  )

##. Texto como datos


palabras_positivas <- c("remodelado", "estrato", "lujo", "vista", "nuevo", "exclusivo", "terraza", "balcón", "iluminación", "garaje")

# Cuenta cuántas veces aparece alguna de estas palabras en cada descripción
train <- train %>%
          mutate(
            desc_clean = str_to_lower(description) %>%
              str_replace_all("[[:punct:]]", " ") %>%
              str_squish(),
            score_palabras_positivas = str_count(desc_clean, str_c(palabras_positivas, collapse = "|"))
          )

train <- train %>%
  mutate(score_palabras_positivas = if_else(is.na(score_palabras_positivas), 0L, score_palabras_positivas))

test <- test %>%
  mutate(
    desc_clean = str_to_lower(description) %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish(),
    score_palabras_positivas = str_count(desc_clean, str_c(palabras_positivas, collapse = "|"))
  )

test <- test %>%
  mutate(score_palabras_positivas = if_else(is.na(score_palabras_positivas), 0L, score_palabras_positivas))

#5. Datos espaciales

#5.1. Primera visualización 
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat)

# Pese a que aparentemente todas parecen estar dentro de bogotá, se filtra a los limites de la ciudad

limits <- getbb("Bogotá Colombia")
limits

train <- train %>%
  filter(
    between(lon, limits[1, "min"], limits[1, "max"]) & 
      between(lat, limits[2, "min"], limits[2, "max"])
  )



#5.2. Variables espaciales


# Convertir a objetos sf
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)
sf_test  <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# Construcción de función para agregar variables espaciales: versión sin mapa


agregar_variable_distancia <- function(sf_data, bbox, key, value, nombre_variable) {
  # 1. Obtener datos de OpenStreetMap
  datos_osm <- tryCatch({
    opq(bbox = bbox) %>%
      add_osm_feature(key = key, value = value) %>%
      osmdata_sf()
  }, error = function(e) {
    message(paste("Error al obtener datos OSM para", nombre_variable, ":", e$message))
    return(NULL)
  })
  
  if (is.null(datos_osm)) {
    sf_data[[nombre_variable]] <- NA
    return(sf_data)
  }
  
  # 2. Seleccionar geometría válida (preferimos polígonos)
  objetos_sf <- NULL
  
  if (nrow(datos_osm$osm_polygons) > 0) {
    objetos_sf <- datos_osm$osm_polygons %>%
      dplyr::select(osm_id, geometry) %>%
      st_make_valid() %>%
      filter(!st_is_empty(geometry), st_is_valid(geometry))
  }
  
  if ((is.null(objetos_sf) || nrow(objetos_sf) == 0) && nrow(datos_osm$osm_points) > 0) {
    objetos_sf <- datos_osm$osm_points %>%
      dplyr::select(osm_id, geometry) %>%
      filter(!st_is_empty(geometry), st_is_valid(geometry))
  }
  
  if (is.null(objetos_sf) || nrow(objetos_sf) == 0) {
    message(paste("No se encontraron geometrías válidas para", nombre_variable))
    sf_data[[nombre_variable]] <- NA
    return(sf_data)
  }
  
  # 3. Calcular centroides si no son puntos
  if (!inherits(objetos_sf$geometry, "sfc_POINT")) {
    suppressWarnings({
      objetos_sf <- objetos_sf %>%
        mutate(geometry = st_centroid(geometry))
    })
  }
  
  # 4. Calcular distancia mínima
  dist_matrix <- st_distance(sf_data, objetos_sf)
  dist_min <- apply(dist_matrix, 1, min)
  
  # 5. Agregar la variable al dataset
  sf_data[[nombre_variable]] <- as.numeric(dist_min)
  
  return(sf_data)
}

# versión con mapa
# agregar_variable_distancia <- function(sf_data, bbox, key, value, nombre_variable) {
            #   # 1. Obtener datos de OpenStreetMap
            #   datos_osm <- tryCatch({
            #     opq(bbox = bbox) %>%
            #       add_osm_feature(key = key, value = value) %>%
            #       osmdata_sf()
            #   }, error = function(e) return(NULL))
            #   
            #   if (is.null(datos_osm)) {
            #     message(paste("No se pudieron obtener datos para", nombre_variable))
            #     sf_data[[nombre_variable]] <- NA
            #     return(list(data = sf_data, mapa = NULL))
            #   }
            #   
            #   # 2. Seleccionar geometría más útil
            #   objetos_sf <- if (nrow(datos_osm$osm_polygons) > 0) {
            #     datos_osm$osm_polygons
            #   } else if (nrow(datos_osm$osm_points) > 0) {
            #     datos_osm$osm_points
            #   } else {
            #     message(paste("No hay geometrías válidas para", nombre_variable))
            #     sf_data[[nombre_variable]] <- NA
            #     return(list(data = sf_data, mapa = NULL))
            #   }
            #   
            #   objetos_sf <- st_as_sf(objetos_sf)
            #   
            #   # 3. Calcular centroides sin atributos (para evitar warning)
            #   objetos_centroides <- objetos_sf %>%
            #     dplyr::select(geometry) %>%
            #     st_centroid()
            #   
            #   # 4. Calcular distancia mínima a cada objeto
            #   dist_matrix <- st_distance(x = sf_data, y = objetos_centroides)
            #   dist_min <- apply(dist_matrix, 1, min)
            #   sf_data[[nombre_variable]] <- as.numeric(dist_min)
            #   
            #   # 5. Crear mapa
            #   mapa <- leaflet() %>%
            #     addTiles() %>%
            #     addCircleMarkers(data = sf_data,
            #                      color = "blue",
            #                      radius = 3,
            #                      label = ~paste("Propiedad")) %>%
            #     addCircleMarkers(data = objetos_centroides,
            #                      color = "red",
            #                      radius = 4,
            #                      label = ~paste(nombre_variable)) %>%
            #     addLegend(position = "bottomright",
            #               colors = c("blue", "red"),
            #               labels = c("Propiedades", nombre_variable),
            #               title = paste("Distancia a", nombre_variable))
            #   
            #   # 6. Mostrar el mapa
            #   print(mapa)
            #   
            #   # 7. Devolver ambos: base y mapa
            #   return(list(data = sf_data, mapa = mapa))
# }

# Creación de variables

#1. Parques
sf_train <- agregar_variable_distancia(sf_train, limits, "leisure", "park", "dist_parque")
sf_test <- agregar_variable_distancia(sf_test, limits, "leisure", "park", "dist_parque")

#2. Hospitales
sf_train <- agregar_variable_distancia(sf_train, limits, "amenity", "hospital", "dist_hospt")
sf_test <- agregar_variable_distancia(sf_test, limits, "amenity", "hospital", "dist_hospt")

#3. Centros comerciales
sf_train <- agregar_variable_distancia(sf_train, limits, "shop", "mall", "dist_mall")
sf_test <- agregar_variable_distancia(sf_test, limits, "shop", "mall", "dist_mall")

#4. Transporte píblico
sf_train <- agregar_variable_distancia(sf_train, limits, "public_transport", "station", "dist_pt")
sf_test <- agregar_variable_distancia(sf_test, limits, "public_transport", "station", "dist_pt")

#5. 
sf_train <- agregar_variable_distancia(sf_train, limits, "amenity", "school", "dist_sch")
sf_test <- agregar_variable_distancia(sf_test, limits, "amenity", "school", "dist_sch")

### VOLVER A LAS BASES TRAIN Y TEST

train <- sf_train %>% st_drop_geometry()
test  <- sf_test  %>% st_drop_geometry()


#7. Cleaning y Guardado de bases

#7.1. Variables para remover

train <- train %>%  
  select(-city, -month, -year, -property_type, -operation_type, -description, -title, -desc_clean )


test <- test %>%  
  select(-city, -month, -year, -property_type, -operation_type, -description, -title, -desc_clean )


#7.2. Guardar bases

write.csv(train, "stores/train_processed.csv", row.names = FALSE)
write.csv(test, "stores/test_processed.csv", row.names = FALSE)
