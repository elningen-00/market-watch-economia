# 02_scrape_tdlc.R
# Scraping de datos desde el Tribunal de Defensa de la Libre Competencia (TDLC)
# Fuentes: jurisprudencia, causas

library(tidyverse)
library(rvest)
library(httr)
library(lubridate)
library(jsonlite)

# Cargar funciones auxiliares
source("scripts/utils.R")

# Configurar
config <- load_config()
headers <- setup_headers(config)

log_activity("Iniciando scraping de TDLC")

# URL base TDLC
tdlc_base <- config$tdlc$url_base

# Función para extraer jurisprudencia
scrape_jurisprudencia <- function() {
  log_activity("Scrapeando jurisprudencia TDLC")
  
  url <- paste0(tdlc_base, config$tdlc$jurisprudencia_path)
  
  tryCatch({
    page <- parse_html(url, config, headers)
    
    jurisprudencia <- page %>%
      html_elements(".sentencia-item, .jurisprudencia-item, article, .case-item") %>%
      map_df(~{
        tibble(
          rol = .x %>% html_element(".rol, .numero, .case-number") %>% html_text2() %>% str_squish(),
          titulo = .x %>% html_element("h2, h3, .title") %>% html_text2() %>% str_squish(),
          fecha = .x %>% html_element(".date, time, .fecha") %>% html_text2() %>% str_squish(),
          materia = .x %>% html_element(".materia, .subject, .categoria") %>% html_text2() %>% str_squish(),
          enlace = .x %>% html_element("a") %>% html_attr("href"),
          tipo = "jurisprudencia",
          fuente = "TDLC",
          fecha_scraping = Sys.time()
        )
      })
    
    # Normalizar URLs relativas
    jurisprudencia$enlace <- ifelse(
      !is.na(jurisprudencia$enlace) & !str_starts(jurisprudencia$enlace, "http"),
      paste0(tdlc_base, jurisprudencia$enlace),
      jurisprudencia$enlace
    )
    
    log_activity(sprintf("Se extrajeron %d sentencias", nrow(jurisprudencia)))
    return(jurisprudencia)
    
  }, error = function(e) {
    warning(sprintf("Error al scrapear jurisprudencia: %s", e$message))
    return(NULL)
  })
}

# Función para extraer causas
scrape_causas <- function() {
  log_activity("Scrapeando causas TDLC")
  
  url <- paste0(tdlc_base, config$tdlc$causas_path)
  
  tryCatch({
    page <- parse_html(url, config, headers)
    
    # Intentar detectar si hay tabla de causas
    causas_tabla <- page %>%
      html_elements("table") %>%
      map_df(~{
        .x %>%
          html_elements("tr") %>%
          map_df(~{
            celdas <- .x %>% html_elements("td, th")
            if (length(celdas) >= 3) {
              tibble(
                rol = celdas[1] %>% html_text2() %>% str_squish(),
                caratula = celdas[2] %>% html_text2() %>% str_squish(),
                estado = celdas[3] %>% html_text2() %>% str_squish(),
                tipo = "causa",
                fuente = "TDLC",
                fecha_scraping = Sys.time()
              )
            } else {
              NULL
            }
          })
      })
    
    # Si no hay tabla, intentar con otro formato
    if (nrow(causas_tabla) == 0) {
      causas <- page %>%
        html_elements(".causa-item, .case-item, article") %>%
        map_df(~{
          tibble(
            rol = .x %>% html_element(".rol, .numero") %>% html_text2() %>% str_squish(),
            caratula = .x %>% html_element(".caratula, .title, h2, h3") %>% html_text2() %>% str_squish(),
            estado = .x %>% html_element(".estado, .status") %>% html_text2() %>% str_squish(),
            enlace = .x %>% html_element("a") %>% html_attr("href"),
            tipo = "causa",
            fuente = "TDLC",
            fecha_scraping = Sys.time()
          )
        })
      
      # Normalizar URLs relativas
      causas$enlace <- ifelse(
        !is.na(causas$enlace) & !str_starts(causas$enlace, "http"),
        paste0(tdlc_base, causas$enlace),
        causas$enlace
      )
      
      return(causas)
    }
    
    log_activity(sprintf("Se extrajeron %d causas", nrow(causas_tabla)))
    return(causas_tabla)
    
  }, error = function(e) {
    warning(sprintf("Error al scrapear causas: %s", e$message))
    return(NULL)
  })
}

# Función para extraer detalles de una sentencia específica
scrape_sentencia_detalle <- function(url_sentencia) {
  tryCatch({
    page <- parse_html(url_sentencia, config, headers)
    
    detalle <- tibble(
      contenido = page %>% 
        html_elements(".contenido, .content, article, .sentencia-texto") %>%
        html_text2() %>%
        paste(collapse = "\n"),
      fecha_emision = page %>% 
        html_element(".fecha-emision, .date, time") %>%
        html_text2() %>%
        str_squish(),
      tribunal = page %>% 
        html_element(".tribunal, .court") %>%
        html_text2() %>%
        str_squish()
    )
    
    return(detalle)
    
  }, error = function(e) {
    warning(sprintf("Error al scrapear detalle de sentencia: %s", e$message))
    return(NULL)
  })
}

# Ejecutar scraping
jurisprudencia_data <- scrape_jurisprudencia()
causas_data <- scrape_causas()

# Combinar todos los datos
tdlc_data <- bind_rows(
  jurisprudencia_data,
  causas_data
)

if (!is.null(tdlc_data) && nrow(tdlc_data) > 0) {
  # Guardar datos raw
  fecha_hoy <- format_date_for_filename()
  save_raw_data(tdlc_data, sprintf("tdlc_%s.rds", fecha_hoy), "rds", config)
  
  # Guardar también en CSV para facilidad de uso
  save_raw_data(tdlc_data, sprintf("tdlc_%s.csv", fecha_hoy), "csv", config)
  
  log_activity(sprintf("Total registros TDLC: %d", nrow(tdlc_data)))
} else {
  warning("No se obtuvieron datos de TDLC")
}

log_activity("Finalizado scraping de TDLC")
