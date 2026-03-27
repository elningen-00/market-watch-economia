# utils.R - Funciones auxiliares para market-watch-economia

#' Cargar configuración desde YAML
#' @param config_path Ruta al archivo config.yml
#' @return Lista con la configuración
load_config <- function(config_path = "config/config.yml") {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    install.packages("yaml")
  }
  yaml::read_yaml(config_path)
}

#' Configurar headers para scraping con politeness
#' @param config Lista de configuración
#' @return Headers listos para usar
setup_headers <- function(config) {
  list(
    "User-Agent" = config$scraping$user_agent,
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language" = "es-CL,es;q=0.9,en;q=0.8",
    "Connection" = "keep-alive"
  )
}

#' Realizar request HTTP con retry y delay
#' @param url URL a solicitar
#' @param config Lista de configuración
#' @param headers Headers HTTP
#' @return Respuesta httr
http_request_with_retry <- function(url, config, headers = NULL) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    install.packages("httr")
  }
  
  max_retries <- config$scraping$max_retries
  delay_min <- config$scraping$delay_min
  delay_max <- config$scraping$delay_max
  
  attempt <- 1
  response <- NULL
  
  while (attempt <= max_retries) {
    tryCatch({
      # Delay aleatorio entre requests (politeness)
      Sys.sleep(runif(1, delay_min, delay_max))
      
      response <- httr::GET(url, httr::add_headers(.headers = headers))
      
      if (httr::status_code(response) == 200) {
        return(response)
      } else {
        warning(sprintf("Status code %d for URL: %s", 
                       httr::status_code(response), url))
      }
    }, error = function(e) {
      warning(sprintf("Error en intento %d: %s", attempt, e$message))
    })
    
    attempt <- attempt + 1
    if (attempt <= max_retries) {
      message(sprintf("Reintentando (%d/%d)...", attempt, max_retries))
    }
  }
  
  stop(sprintf("Fallaron todos los intentos para URL: %s", url))
}

#' Parsear HTML con rvest
#' @param url URL de la página
#' @param config Lista de configuración
#' @param headers Headers HTTP
#' @return Objeto xml_document
parse_html <- function(url, config, headers = NULL) {
  if (!requireNamespace("rvest", quietly = TRUE)) {
    install.packages("rvest")
  }
  
  response <- http_request_with_retry(url, config, headers)
  rvest::read_html(response)
}

#' Guardar datos crudos (raw)
#' @param data Datos a guardar
#' @param filename Nombre del archivo
#' @param format Formato ("rds" o "csv")
#' @param config Lista de configuración
save_raw_data <- function(data, filename, format = "rds", config = NULL) {
  if (is.null(config)) {
    config <- load_config()
  }
  
  raw_path <- file.path(config$data$raw_path, filename)
  
  # Crear directorio si no existe
  dir.create(dirname(raw_path), showWarnings = FALSE, recursive = TRUE)
  
  if (format == "rds") {
    saveRDS(data, raw_path)
    message(sprintf("Datos guardados en: %s", raw_path))
  } else if (format == "csv") {
    if (!requireNamespace("readr", quietly = TRUE)) {
      install.packages("readr")
    }
    readr::write_csv(data, raw_path)
    message(sprintf("Datos guardados en: %s", raw_path))
  } else {
    stop("Formato no soportado. Use 'rds' o 'csv'")
  }
  
  invisible(raw_path)
}

#' Guardar datos procesados
#' @param data Datos a guardar
#' @param filename Nombre del archivo
#' @param format Formato ("rds" o "csv")
#' @param config Lista de configuración
save_processed_data <- function(data, filename, format = "rds", config = NULL) {
  if (is.null(config)) {
    config <- load_config()
  }
  
  processed_path <- file.path(config$data$processed_path, filename)
  
  # Crear directorio si no existe
  dir.create(dirname(processed_path), showWarnings = FALSE, recursive = TRUE)
  
  if (format == "rds") {
    saveRDS(data, processed_path)
    message(sprintf("Datos procesados guardados en: %s", processed_path))
  } else if (format == "csv") {
    if (!requireNamespace("readr", quietly = TRUE)) {
      install.packages("readr")
    }
    readr::write_csv(data, processed_path)
    message(sprintf("Datos procesados guardados en: %s", processed_path))
  } else {
    stop("Formato no soportado. Use 'rds' o 'csv'")
  }
  
  invisible(processed_path)
}

#' Cargar datos desde archivo
#' @param filepath Ruta al archivo
#' @return Datos cargados
load_data <- function(filepath) {
  ext <- tools::file_ext(filepath)
  
  if (ext == "rds") {
    return(readRDS(filepath))
  } else if (ext == "csv") {
    if (!requireNamespace("readr", quietly = TRUE)) {
      install.packages("readr")
    }
    return(readr::read_csv(filepath))
  } else {
    stop(sprintf("Extensión no soportada: %s", ext))
  }
}

#' Limpiar texto HTML
#' @param text Texto con posible HTML
#' @return Texto limpio
clean_html_text <- function(text) {
  if (!requireNamespace("rvest", quietly = TRUE)) {
    install.packages("rvest")
  }
  
  text %>%
    rvest::html_text2() %>%
    stringr::str_squish()
}

#' Formatear fecha para nombres de archivo
#' @param date Fecha a formatear
#' @return String con formato YYYYMMDD
format_date_for_filename <- function(date = Sys.Date()) {
  format(as.Date(date), "%Y%m%d")
}

#' Log de actividad
#' @param message Mensaje a loguear
#' @param level Nivel del log (INFO, WARNING, ERROR)
log_activity <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s: %s\n", timestamp, level, message))
}
