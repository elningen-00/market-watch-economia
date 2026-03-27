# 01_scrape_fne.R
# Scraping de datos desde la Fiscalía Nacional Económica (FNE)
# Fuentes: boletines, investigaciones, resoluciones

library(tidyverse)
library(rvest)
library(httr)
library(lubridate)

# Cargar funciones auxiliares
source("scripts/utils.R")

# Configurar
config <- load_config()
headers <- setup_headers(config)

log_activity("Iniciando scraping de FNE")

# URL base FNE
fne_base <- config$fne$url_base

# Función para extraer boletines
scrape_boletines <- function() {
  log_activity("Scrapeando boletines FNE")
  
  url <- paste0(fne_base, config$fne$boletines_path)
  
  tryCatch({
    page <- parse_html(url, config, headers)
    
    boletines <- page %>%
      html_elements(".boletin-item, .news-item, article") %>%
      map_df(~{
        tibble(
          titulo = .x %>% html_element("h2, h3, .title") %>% html_text2() %>% str_squish(),
          fecha = .x %>% html_element(".date, time, .fecha") %>% html_text2() %>% str_squish(),
          resumen = .x %>% html_element(".summary, .excerpt, p") %>% html_text2() %>% str_squish(),
          enlace = .x %>% html_element("a") %>% html_attr("href"),
          tipo = "boletin",
          fuente = "FNE",
          fecha_scraping = Sys.time()
        )
      })
    
    # Normalizar URLs relativas
    boletines$enlace <- ifelse(
      !is.na(boletines$enlace) & !str_starts(boletines$enlace, "http"),
      paste0(fne_base, boletines$enlace),
      boletines$enlace
    )
    
    log_activity(sprintf("Se extrajeron %d boletines", nrow(boletines)))
    return(boletines)
    
  }, error = function(e) {
    warning(sprintf("Error al scrapear boletines: %s", e$message))
    return(NULL)
  })
}

# Función para extraer investigaciones
scrape_investigaciones <- function() {
  log_activity("Scrapeando investigaciones FNE")
  
  url <- paste0(fne_base, config$fne$investigaciones_path)
  
  tryCatch({
    page <- parse_html(url, config, headers)
    
    investigaciones <- page %>%
      html_elements(".investigacion-item, .research-item, article") %>%
      map_df(~{
        tibble(
          titulo = .x %>% html_element("h2, h3, .title") %>% html_text2() %>% str_squish(),
          fecha = .x %>% html_element(".date, time, .fecha") %>% html_text2() %>% str_squish(),
          area = .x %>% html_element(".area, .categoria") %>% html_text2() %>% str_squish(),
          enlace = .x %>% html_element("a") %>% html_attr("href"),
          tipo = "investigacion",
          fuente = "FNE",
          fecha_scraping = Sys.time()
        )
      })
    
    # Normalizar URLs relativas
    investigaciones$enlace <- ifelse(
      !is.na(investigaciones$enlace) & !str_starts(investigaciones$enlace, "http"),
      paste0(fne_base, investigaciones$enlace),
      investigaciones$enlace
    )
    
    log_activity(sprintf("Se extrajeron %d investigaciones", nrow(investigaciones)))
    return(investigaciones)
    
  }, error = function(e) {
    warning(sprintf("Error al scrapear investigaciones: %s", e$message))
    return(NULL)
  })
}

# Función para extraer resoluciones
scrape_resoluciones <- function() {
  log_activity("Scrapeando resoluciones FNE")
  
  url <- paste0(fne_base, config$fne$resoluciones_path)
  
  tryCatch({
    page <- parse_html(url, config, headers)
    
    resoluciones <- page %>%
      html_elements(".resolucion-item, .resolution-item, article") %>%
      map_df(~{
        tibble(
          numero = .x %>% html_element(".numero, .number") %>% html_text2() %>% str_squish(),
          titulo = .x %>% html_element("h2, h3, .title") %>% html_text2() %>% str_squish(),
          fecha = .x %>% html_element(".date, time, .fecha") %>% html_text2() %>% str_squish(),
          enlace = .x %>% html_element("a") %>% html_attr("href"),
          tipo = "resolucion",
          fuente = "FNE",
          fecha_scraping = Sys.time()
        )
      })
    
    # Normalizar URLs relativas
    resoluciones$enlace <- ifelse(
      !is.na(resoluciones$enlace) & !str_starts(resoluciones$enlace, "http"),
      paste0(fne_base, resoluciones$enlace),
      resoluciones$enlace
    )
    
    log_activity(sprintf("Se extrajeron %d resoluciones", nrow(resoluciones)))
    return(resoluciones)
    
  }, error = function(e) {
    warning(sprintf("Error al scrapear resoluciones: %s", e$message))
    return(NULL)
  })
}

# Ejecutar scraping
boletines_data <- scrape_boletines()
investigaciones_data <- scrape_investigaciones()
resoluciones_data <- scrape_resoluciones()

# Combinar todos los datos
fne_data <- bind_rows(boletines_data, investigaciones_data, resoluciones_data)

if (!is.null(fne_data) && nrow(fne_data) > 0) {
  # Guardar datos raw
  fecha_hoy <- format_date_for_filename()
  save_raw_data(fne_data, sprintf("fne_%s.rds", fecha_hoy), "rds", config)
  
  # Guardar también en CSV para facilidad de uso
  save_raw_data(fne_data, sprintf("fne_%s.csv", fecha_hoy), "csv", config)
  
  log_activity(sprintf("Total registros FNE: %d", nrow(fne_data)))
} else {
  warning("No se obtuvieron datos de FNE")
}

log_activity("Finalizado scraping de FNE")
