# 03_scrape_corfo.R
# Scraping de datos desde CORFO
# Fuentes: programas de fomento, innovación

library(tidyverse)
library(rvest)
library(httr)
library(lubridate)

# Cargar funciones auxiliares
source("scripts/utils.R")

# Configurar
config <- load_config()
headers <- setup_headers(config)

log_activity("Iniciando scraping de CORFO")

# URL base CORFO
corfo_base <- config$corfo$url_base

# Función para extraer programas de fomento
scrape_programas <- function() {
  log_activity("Scrapeando programas de fomento CORFO")
  
  url <- paste0(corfo_base, config$corfo$programas_path)
  
  tryCatch({
    page <- parse_html(url, config, headers)
    
    programas <- page %>%
      html_elements(".programa-item, .program-item, article, .beneficio-item") %>%
      map_df(~{
        tibble(
          nombre = .x %>% html_element("h2, h3, .title, .nombre") %>% html_text2() %>% str_squish(),
          descripcion = .x %>% html_element(".descripcion, .description, .summary, p") %>% html_text2() %>% str_squish(),
          area = .x %>% html_element(".area, .categoria, .linea") %>% html_text2() %>% str_squish(),
          beneficiarios = .x %>% html_element(".beneficiarios, .target, .dirigido") %>% html_text2() %>% str_squish(),
          monto = .x %>% html_element(".monto, .amount, .financiamiento") %>% html_text2() %>% str_squish(),
          enlace = .x %>% html_element("a") %>% html_attr("href"),
          tipo = "programa",
          fuente = "CORFO",
          fecha_scraping = Sys.time()
        )
      })
    
    # Normalizar URLs relativas
    programas$enlace <- ifelse(
      !is.na(programas$enlace) & !str_starts(programas$enlace, "http"),
      paste0(corfo_base, programas$enlace),
      programas$enlace
    )
    
    log_activity(sprintf("Se extrajeron %d programas", nrow(programas)))
    return(programas)
    
  }, error = function(e) {
    warning(sprintf("Error al scrapear programas: %s", e$message))
    return(NULL)
  })
}

# Función para extraer proyectos de innovación
scrape_innovacion <- function() {
  log_activity("Scrapeando proyectos de innovación CORFO")
  
  url <- paste0(corfo_base, config$corfo$innovacion_path)
  
  tryCatch({
    page <- parse_html(url, config, headers)
    
    innovacion <- page %>%
      html_elements(".proyecto-item, .project-item, article, .innovacion-item") %>%
      map_df(~{
        tibble(
          titulo = .x %>% html_element("h2, h3, .title") %>% html_text2() %>% str_squish(),
          descripcion = .x %>% html_element(".descripcion, .description, .summary, p") %>% html_text2() %>% str_squish(),
          sector = .x %>% html_element(".sector, .industria, .area") %>% html_text2() %>% str_squish(),
          region = .x %>% html_element(".region, .ubicacion, .location") %>% html_text2() %>% str_squish(),
          estado = .x %>% html_element(".estado, .status") %>% html_text2() %>% str_squish(),
          enlace = .x %>% html_element("a") %>% html_attr("href"),
          tipo = "innovacion",
          fuente = "CORFO",
          fecha_scraping = Sys.time()
        )
      })
    
    # Normalizar URLs relativas
    innovacion$enlace <- ifelse(
      !is.na(innovacion$enlace) & !str_starts(innovacion$enlace, "http"),
      paste0(corfo_base, innovacion$enlace),
      innovacion$enlace
    )
    
    log_activity(sprintf("Se extrajeron %d proyectos de innovación", nrow(innovacion)))
    return(innovacion)
    
  }, error = function(e) {
    warning(sprintf("Error al scrapear innovación: %s", e$message))
    return(NULL)
  })
}

# Función para extraer convocatorias activas
scrape_convocatorias <- function() {
  log_activity("Scrapeando convocatorias CORFO")
  
  url <- paste0(corfo_base, "/convocatorias")
  
  tryCatch({
    page <- parse_html(url, config, headers)
    
    convocatorias <- page %>%
      html_elements(".convocatoria-item, .call-item, article") %>%
      map_df(~{
        tibble(
          titulo = .x %>% html_element("h2, h3, .title") %>% html_text2() %>% str_squish(),
          fecha_apertura = .x %>% html_element(".fecha-apertura, .apertura, .start-date") %>% html_text2() %>% str_squish(),
          fecha_cierre = .x %>% html_element(".fecha-cierre, .cierre, .end-date, .deadline") %>% html_text2() %>% str_squish(),
          estado = .x %>% html_element(".estado, .status") %>% html_text2() %>% str_squish(),
          enlace = .x %>% html_element("a") %>% html_attr("href"),
          tipo = "convocatoria",
          fuente = "CORFO",
          fecha_scraping = Sys.time()
        )
      })
    
    # Normalizar URLs relativas
    convocatorias$enlace <- ifelse(
      !is.na(convocatorias$enlace) & !str_starts(convocatorias$enlace, "http"),
      paste0(corfo_base, convocatorias$enlace),
      convocatorias$enlace
    )
    
    log_activity(sprintf("Se extrajeron %d convocatorias", nrow(convocatorias)))
    return(convocatorias)
    
  }, error = function(e) {
    warning(sprintf("Error al scrapear convocatorias: %s", e$message))
    return(NULL)
  })
}

# Ejecutar scraping
programas_data <- scrape_programas()
innovacion_data <- scrape_innovacion()
convocatorias_data <- scrape_convocatorias()

# Combinar todos los datos
corfo_data <- bind_rows(
  programas_data,
  innovacion_data,
  convocatorias_data
)

if (!is.null(corfo_data) && nrow(corfo_data) > 0) {
  # Guardar datos raw
  fecha_hoy <- format_date_for_filename()
  save_raw_data(corfo_data, sprintf("corfo_%s.rds", fecha_hoy), "rds", config)
  
  # Guardar también en CSV para facilidad de uso
  save_raw_data(corfo_data, sprintf("corfo_%s.csv", fecha_hoy), "csv", config)
  
  log_activity(sprintf("Total registros CORFO: %d", nrow(corfo_data)))
} else {
  warning("No se obtuvieron datos de CORFO")
}

log_activity("Finalizado scraping de CORFO")
