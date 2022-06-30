######################## 1. Librerias ##########################################
install.packages("pacman")
library(pacman)
pacman::p_load(pdftools,     # Leer archivos PDF
               dplyr,        # Manipular datos
               stopwords,    # Selección de Oraciones en solitario
               tidytext,
               stringi,
               stringr,
               ggplot2,      # Graficar
               scales,
               tidyr,
               widyr,
               ggraph,
               igraph,
               quanteda,
               topicmodels,
               cvTools,
               magrittr,    # Concatenar operaciones con pipe %>%
               wordcloud,   # Generar la nube de Oraciones
               RColorBrewer)# Color Oraciones  

# fijamos a UTF-8 para facilitar lectura.
options(encoding = "utf-8")

######################## 2. Importe de archivo #################################
# Import pdf file.
texto <- pdftools::pdf_text("intro/Borrador_NC.pdf")

# N° pp. doc.
length(texto)

# General data cleaning
texto <- gsub("\\r", " ", texto)  
texto <- gsub("\\n", "", texto)
texto <- gsub("\\d\\K\\.(?=\\d)", "", texto, perl = TRUE)

# Collapse pp. (juntar todas las páginas del texto). Antes, nos arrojaba el valor
# de '160' con el comando 'lenght()'. Ahora, como se observa, el valor del mismo
# comando ejecutado es '1', como resultado del colapso de páginas.
texto <- paste(texto, collapse = '')


# Seguimos estructurando el texto.
texto <- gsub('"', "", texto)
texto <- gsub("§", "", texto)

# Creamos un generador de frases con 'for' (repetición de instrucciones).
vector = c()
  for(i in 1:length(texto)){
    temp <- (strsplit(texto[[i]], "\\.")[[1]])
    print(temp)
    vector <- c(vector, temp)
  }
  
# Convertimos el 'vector' en un data frame.
texto_words <- as.data.frame(vector)
View(texto_words)

######################## 3. Limpieza de datos ##################################
# Definimos el título de la columna con las Oraciones del documento.
colnames(texto_words)[1] <- "Oraciones"

# Quitamos detalles gráficos que perturbarán el posterior análisis (pies de páginas, encabezados, etc)
texto_words$Oraciones <- trimws(texto_words$Oraciones, "left", whitespace = "[\r]")

# Convertimos nuestro vector a variable 'chr' o 'character'.
texto_words$Oraciones <- as.character(texto_words$Oraciones)

# Seguimos limpiando nuestros datos
texto_words$Oraciones <- gsub("Artículo", "", texto_words$Oraciones)
texto_words$Oraciones <- gsub("Bis", "", texto_words$Oraciones)
texto_words$Oraciones <- gsub("°", "", texto_words$Oraciones)
texto_words$Oraciones <- gsub("-", "", texto_words$Oraciones)
texto_words$Oraciones <- gsub("CAPÍTULO", "", texto_words$Oraciones)
texto_words$Oraciones <- gsub("Capítulo", "", texto_words$Oraciones)
texto_words$Oraciones <- gsub("ley", "", texto_words$Oraciones)
texto_words$Oraciones <- gsub("•", "", texto_words$Oraciones)
texto_words$Oraciones <- gsub("bis", "", texto_words$Oraciones)
texto_words$Oraciones <- gsub("*", "", texto_words$Oraciones)
texto_words$Oraciones <- gsub("COM", "", texto_words$Oraciones)
texto_words$Oraciones <- gsub("0", " ", texto_words$Oraciones)
texto_words$Oraciones <- gsub("1", " ", texto_words$Oraciones)
texto_words$Oraciones <- gsub("2", " ", texto_words$Oraciones)
texto_words$Oraciones <- gsub("3", " ", texto_words$Oraciones)
texto_words$Oraciones <- gsub("4", " ", texto_words$Oraciones)
texto_words$Oraciones <- gsub("5", " ", texto_words$Oraciones)
texto_words$Oraciones <- gsub("6", " ", texto_words$Oraciones)
texto_words$Oraciones <- gsub("7", " ", texto_words$Oraciones)
texto_words$Oraciones <- gsub("8", " ", texto_words$Oraciones)
texto_words$Oraciones <- gsub("9", " ", texto_words$Oraciones)
texto_words$Oraciones <- gsub("0", " ", texto_words$Oraciones)


# Creamos una lexicología de stopwords en español ('es') y lo convertimos a df ('data frame').
# Lo que ocurrirá es que se aparecerá cada palabra en solitario ('stopwords') y esto
# lo podemos ver en una tabla que podemos visualizar ('as.data.frame()')
lexico_tw <- stopwords("es")
lexico_tw <- as.data.frame(lexico_tw) 

## *IMPORTANTE! Este es uno de los pasos más importantes dentro de nuestra mineria de texto, 
##  pues nos permite analizar, individualmente, el total de Oraciones que el conjunto
##  de datos nos enseña.

# Nombramos a nuestra columna y la definimos como una columna 'chr' o character.
colnames(lexico_tw) <- "Concepto"
lexico_tw$Concepto <- as.character(lexico_tw$Concepto)

######################## 4. Análisis ###########################################
# Generamos un ID para cada frase
df_tw <- tibble::rowid_to_column(texto_words, "ID")

# Estructuramos las Oraciones (oraciones) y extremos los conceptos repetidos y su respectiva cantidad.
revision_words <- df_tw %>% 
  distinct(Oraciones, .keep_all = TRUE) %>% # Para eliminar filas duplicadas basadas en 'Oraciones'
  unnest_tokens(Concepto, Oraciones, drop = FALSE) %>% 
  distinct(ID, Concepto, .keep_all = TRUE) %>% 
  anti_join(lexico_tw) %>% # devuelve todas las filas de x donde no hay valores coincidentes en y, manteniendo solo columnas de x
  filter(str_detect(Concepto, "[^\\d]")) %>% 
  group_by(Concepto) %>% 
  dplyr::mutate(Concepto_total = n()) %>% 
  ungroup()

# Contamos las Oraciones totales resultantes ('Conceptos'), y las ordenamos con 'sort'
contar_words <- revision_words %>% 
  dplyr::count(Concepto, sort = T)

# Graficamos con 'ggplot2' para visualizar de una manera más interactiva este trabajo.
contar_words %>% 
  head(40) %>% 
  mutate(Concepto = reorder(Concepto, n)) %>% 
  ggplot(aes(Concepto, n)) + 
    geom_col(fill = "red") + 
    scale_y_continuous(labels = comma_format()) + 
    coord_flip() + 
    labs(title = paste0("Presencia de conceptos "),
         subtitle = "Stopwords extraidas",
         x = "Concepto",
         y = "N de veces presente")                                                                                                                   


# Antes de generar el 'WorldCLoud', añadimos al 'df' la frecuencia de los conceptos dentro del documento.
df_frq_revision_words <- revision_words %>%
  group_by(Concepto) %>% 
  count(Concepto) %>%  
  group_by(Concepto) %>%
  mutate(frecuencia = n/dim(revision_words)[1])

wordcloud(words = df_frq_revision_words$Concepto, freq = df_frq_revision_words$frecuencia,
          max.words = 400, 
          random.order = FALSE, 
          rot.per = 0.10,
          colors = brewer.pal(6, "Paired"))

# Si queremos personalizar la nube, ejecutamos el siguiente comando de colores.
display.brewer.all(colorblindFriendly = T)
######################## 5. Conclusiones #######################################

--  not yet --
######################## 6. Guardar datos ######################################

saveRDS(contar_words, file = "output/contar_words.rds")
