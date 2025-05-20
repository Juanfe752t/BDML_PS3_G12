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

### DESCRIPCIÓN A DATOS 


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

