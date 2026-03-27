# utils.R - Funciones auxiliares para market-watch-economia
# Versión: 1.0
# Última actualización: 2024

# Cargar librerías ---------------------------------------------------------
library(rvest)
library(httr)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(yaml)

# Cargar configuración ------------------------------------------------------
cargar_config <- function() {
  config_path <- "config/config.yml"
  if (!file.exists(config_path)) {
    stop("No se encontró el archivo de configuración: config/config.yml")
  }
  yaml::read_yaml(config_path)
}

# Configuración global
config <- cargar_config()

# Funciones de logging ------------------------------------------------------
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s: %s\n", timestamp, level, message))
}

log_error <- function(message) {
  log_message(message, "ERROR")
}

log_success <- function(message) {
  log_message(message, "SUCCESS")
}

# Funciones de scraping -----------------------------------------------------

#' Realizar una request HTTP con manejo de errores y retries
#' @param url URL a solicitar
#' @param headers Headers HTTP adicionales
#' @param max_retries Número máximo de reintentos
#' @return Objeto response de httr o NULL si falla
scrape_url <- function(url, headers = NULL, max_retries = NULL) {
  
  if (is.null(max_retries)) {
    max_retries <- config$general$max_retries
  }
  
  # Headers por defecto
  default_headers <- list(
    `User-Agent` = config$general$user_agent,
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    `Accept-Language` = "es-CL,es;q=0.9,en;q=0.8",
    `Connection` = "keep-alive"
  )
  
  # Combinar headers
  if (!is.null(headers)) {
    all_headers <- c(default_headers, headers)
  } else {
    all_headers <- default_headers
  }
  
  # Intentar la request con retries
  for (i in 1:(max_retries + 1)) {
    tryCatch({
      log_message(sprintf("Request a: %s (intento %d de %d)", url, i, max_retries + 1))
      
      response <- httr::GET(
        url,
        httr::add_headers(.headers = all_headers),
        timeout(config$general$timeout)
      )
      
      if (httr::status_code(response) == 200) {
        log_success(sprintf("Éxito: %s", url))
        return(response)
      } else {
        log_error(sprintf("Error HTTP %d en: %s", httr::status_code(response), url))
      }
      
    }, error = function(e) {
      log_error(sprintf("Error en request: %s", e$message))
    })
    
    if (i <= max_retries) {
      wait_time <- 2 ^ i  # Backoff exponencial
      log_message(sprintf("Reintentando en %d segundos...", wait_time))
      Sys.sleep(wait_time)
    }
  }
  
  log_error(sprintf("Fallaron todos los intentos para: %s", url))
  return(NULL)
}

#' Extraer texto de un nodo HTML de forma segura
#' @param node Nodo HTML
#' @param default Valor por defecto si el nodo es NULL
#' @return Texto extraído o valor por defecto
safe_text <- function(node, default = NA_character_) {
  if (is.null(node) || length(node) == 0) {
    return(default)
  }
  text <- rvest::html_text2(node)
  if (is.na(text) || text == "") {
    return(default)
  }
  return(trimws(text))
}

#' Extraer atributo de un nodo HTML de forma segura
#' @param node Nodo HTML
#' @param attr Nombre del atributo
#' @param default Valor por defecto si el atributo no existe
#' @return Valor del atributo o valor por defecto
safe_attr <- function(node, attr, default = NA_character_) {
  if (is.null(node) || length(node) == 0) {
    return(default)
  }
  value <- rvest::html_attr(node, attr)
  if (is.na(value) || value == "") {
    return(default)
  }
  return(trimws(value))
}

#' Parsear fecha de forma robusta
#' @param date_string String con fecha
#' @param formats Vector de formatos posibles
#' @return Objeto Date o NA
parse_fecha <- function(date_string, formats = NULL) {
  if (is.na(date_string) || date_string == "") {
    return(NA)
  }
  
  if (is.null(formats)) {
    formats <- c(
      "%d/%m/%Y",
      "%d-%m-%Y",
      "%Y-%m-%d",
      "%d de %B de %Y",
      "%B %d, %Y",
      "%d %b %Y"
    )
  }
  
  # Intentar parsear con lubridate primero
  parsed <- lubridate::parse_date_time(date_string, orders = c("dmY", "Ymd", "dBY", "Bdy"), locale = "es_CL")
  
  if (!is.na(parsed)) {
    return(as.Date(parsed))
  }
  
  # Intentar con formatos específicos
  for (fmt in formats) {
    tryCatch({
      result <- as.Date(date_string, format = fmt)
      if (!is.na(result)) {
        return(result)
      }
    }, error = function(e) {})
  }
  
  return(NA)
}

# Funciones de guardado de datos --------------------------------------------

#' Guardar datos en formato RDS
#' @param data Dataframe a guardar
#' @param filename Nombre del archivo (sin extensión)
#' @param directory Directorio donde guardar
guardar_rds <- function(data, filename, directory = "data/raw") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filepath <- file.path(directory, paste0(filename, "_", timestamp, ".rds"))
  
  # Crear directorio si no existe
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  saveRDS(data, filepath)
  log_success(sprintf("Datos guardados en: %s", filepath))
  return(filepath)
}

#' Guardar datos en formato CSV
#' @param data Dataframe a guardar
#' @param filename Nombre del archivo (sin extensión)
#' @param directory Directorio donde guardar
guardar_csv <- function(data, filename, directory = "data/raw") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filepath <- file.path(directory, paste0(filename, "_", timestamp, ".csv"))
  
  # Crear directorio si no existe
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  readr::write_csv(data, filepath, na = "")
  log_success(sprintf("Datos guardados en: %s", filepath))
  return(filepath)
}

#' Cargar datos desde RDS
#' @param filepath Ruta al archivo RDS
#' @return Dataframe cargado
cargar_rds <- function(filepath) {
  if (!file.exists(filepath)) {
    log_error(sprintf("Archivo no encontrado: %s", filepath))
    return(NULL)
  }
  data <- readRDS(filepath)
  log_message(sprintf("Datos cargados desde: %s", filepath))
  return(data)
}

# Funciones de limpieza de datos --------------------------------------------

#' Limpiar texto de HTML y caracteres especiales
#' @param text Texto a limpiar
#' @return Texto limpio
limpiar_texto <- function(text) {
  if (is.na(text)) return(NA_character_)
  
  text %>%
    gsub("\\s+", " ", .) %>%  # Múltiples espacios a uno solo
    gsub("[\\t\\n\\r]", " ", .) %>%  # Tabs y newlines
    gsub("&nbsp;", " ", .) %>%
    gsub("&amp;", "&", .) %>%
    gsub("&lt;", "<", .) %>%
    gsub("&gt;", ">", .) %>%
    gsub("&quot;", "\"", .) %>%
    gsub("&#39;", "'", .) %>%
    trimws()
}

#' Normalizar columnas de un dataframe
#' @param data Dataframe a normalizar
#' @return Dataframe con columnas normalizadas
normalizar_columnas <- function(data) {
  # Nombres de columnas en minúsculas y sin espacios
  names(data) <- names(data) %>%
    tolower() %>%
    gsub("\\s+", "_", .) %>%
    gsub("[^a-z0-9_]", "", .)
  
  return(data)
}

# Funciones utilitarias -----------------------------------------------------

#' Esperar entre requests respetando politeness
#' @param source_name Nombre de la fuente
#' @param delay_override Delay personalizado (opcional)
esperar_request <- function(source_name, delay_override = NULL) {
  delay <- ifelse(is.null(delay_override), 
                  config$fuentes[[source_name]]$delay, 
                  delay_override)
  
  log_message(sprintf("Esperando %d segundos antes de siguiente request...", delay))
  Sys.sleep(delay)
}

#' Obtener timestamp actual formateado
get_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

#' Calcular hash de un dataframe para detectar cambios
calcular_hash <- function(data) {
  digest::digest(data, algo = "md5")
}

# Mensaje de inicialización
log_success("utils.R cargado exitosamente")
log_message(sprintf("Configuración cargada: %d fuentes disponibles", length(config$fuentes)))
