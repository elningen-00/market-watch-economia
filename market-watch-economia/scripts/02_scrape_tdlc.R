# 02_scrape_tdlc.R - Scraping del Tribunal de Defensa de la Libre Competencia (TDLC)
# Fuente: https://www.tdlc.cl
# Descripción: Extrae jurisprudencia, causas y sentencias

# Cargar funciones auxiliares
source("scripts/utils.R")

log_success("Iniciando scraping de TDLC...")

# Configuración específica para TDLC
tdlc_config <- config$fuentes$tdlc
url_base <- tdlc_config$url_base

# Lista para almacenar todos los datos
todos_datos_tdlc <- list()

# ============================================================================
# 1. SCRAPING DE CAUSAS
# ============================================================================
log_message("Scrapeando causas del TDLC...")

scrape_causas_tdlc <- function() {
  url_causas <- paste0(url_base, tdlc_config$causas)
  
  response <- scrape_url(url_causas)
  if (is.null(response)) {
    log_error("No se pudo obtener la página de causas")
    return(NULL)
  }
  
  page <- httr::content(response, "parsed", encoding = "UTF-8")
  
  causas <- tryCatch({
    # Selectores genéricos que deben ajustarse según la estructura real
    items <- page %>% rvest::html_nodes(".causa-item, .case-item, article, li, tr")
    
    if (length(items) == 0) {
      log_message("No se encontraron causas con selectores comunes")
      return(NULL)
    }
    
    datos <- lapply(items, function(item) {
      numero_causa <- safe_text(item %>% rvest::html_nodes(".numero, .case-number, .rol") %>% .[1])
      titulo <- safe_text(item %>% rvest::html_nodes("h2, h3, h4, a, .title") %>% .[1])
      enlace <- safe_attr(item %>% rvest::html_nodes("a") %>% .[1], "href")
      fecha <- safe_text(item %>% rvest::html_nodes(".date, time, .fecha") %>% .[1])
      estado <- safe_text(item %>% rvest::html_nodes(".estado, .status, .state") %>% .[1])
      caratula <- safe_text(item %>% rvest::html_nodes(".caratula, .summary") %>% .[1])
      
      if (!is.na(enlace) && !grepl("^http", enlace)) {
        enlace <- paste0(url_base, ifelse(substr(enlace, 1, 1) != "/", paste0("/", enlace), enlace))
      }
      
      data.frame(
        tipo = "causa",
        numero_causa = limpiar_texto(numero_causa),
        titulo = limpiar_texto(titulo),
        caratula = limpiar_texto(caratula),
        enlace = enlace,
        fecha = parse_fecha(fecha),
        estado = limpiar_texto(estado),
        fuente = "TDLC",
        fecha_scraping = get_timestamp(),
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, datos)
    
  }, error = function(e) {
    log_error(sprintf("Error al scrapear causas: %s", e$message))
    return(NULL)
  })
  
  if (!is.null(causas) && nrow(causas) > 0) {
    log_success(sprintf("Se extrajeron %d causas", nrow(causas)))
  } else {
    log_message("No se extrajeron causas")
    causas <- data.frame(
      tipo = character(),
      numero_causa = character(),
      titulo = character(),
      caratula = character(),
      enlace = character(),
      fecha = as.Date(character()),
      estado = character(),
      fuente = character(),
      fecha_scraping = character(),
      stringsAsFactors = FALSE
    )
  }
  
  return(causas)
}

causas_tdlc <- scrape_causas_tdlc()
if (!is.null(causas_tdlc)) {
  todos_datos_tdlc$causas <- causas_tdlc
}

esperar_request("tdlc")

# ============================================================================
# 2. SCRAPING DE SENTENCIAS
# ============================================================================
log_message("Scrapeando sentencias del TDLC...")

scrape_sentencias_tdlc <- function() {
  url_sentencias <- paste0(url_base, tdlc_config$sentencias)
  
  response <- scrape_url(url_sentencias)
  if (is.null(response)) {
    log_error("No se pudo obtener la página de sentencias")
    return(NULL)
  }
  
  page <- httr::content(response, "parsed", encoding = "UTF-8")
  
  sentencias <- tryCatch({
    items <- page %>% rvest::html_nodes(".sentencia-item, .resolution-item, article, li")
    
    datos <- lapply(items, function(item) {
      numero_sentencia <- safe_text(item %>% rvest::html_nodes(".numero, .number") %>% .[1])
      titulo <- safe_text(item %>% rvest::html_nodes("h2, h3, a, .title") %>% .[1])
      enlace <- safe_attr(item %>% rvest::html_nodes("a") %>% .[1], "href")
      fecha <- safe_text(item %>% rvest::html_nodes(".date, time, .fecha") %>% .[1])
      tipo_fallo <- safe_text(item %>% rvest::html_nodes(".tipo, .type, .fallos") %>% .[1])
      
      if (!is.na(enlace) && !grepl("^http", enlace)) {
        enlace <- paste0(url_base, ifelse(substr(enlace, 1, 1) != "/", paste0("/", enlace), enlace))
      }
      
      data.frame(
        tipo = "sentencia",
        numero_sentencia = limpiar_texto(numero_sentencia),
        titulo = limpiar_texto(titulo),
        enlace = enlace,
        fecha = parse_fecha(fecha),
        tipo_fallo = limpiar_texto(tipo_fallo),
        fuente = "TDLC",
        fecha_scraping = get_timestamp(),
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, datos)
    
  }, error = function(e) {
    log_error(sprintf("Error al scrapear sentencias: %s", e$message))
    return(NULL)
  })
  
  if (!is.null(sentencias) && nrow(sentencias) > 0) {
    log_success(sprintf("Se extrajeron %d sentencias", nrow(sentencias)))
  } else {
    log_message("No se extrajeron sentencias")
    sentencias <- data.frame(
      tipo = character(),
      numero_sentencia = character(),
      titulo = character(),
      enlace = character(),
      fecha = as.Date(character()),
      tipo_fallo = character(),
      fuente = character(),
      fecha_scraping = character(),
      stringsAsFactors = FALSE
    )
  }
  
  return(sentencias)
}

sentencias_tdlc <- scrape_sentencias_tdlc()
if (!is.null(sentencias_tdlc)) {
  todos_datos_tdlc$sentencias <- sentencias_tdlc
}

esperar_request("tdlc")

# ============================================================================
# 3. SCRAPING DE JURISPRUDENCIA
# ============================================================================
log_message("Scrapeando jurisprudencia del TDLC...")

scrape_jurisprudencia_tdlc <- function() {
  url_jurisprudencia <- paste0(url_base, tdlc_config$jurisprudencia)
  
  response <- scrape_url(url_jurisprudencia)
  if (is.null(response)) {
    log_error("No se pudo obtener la página de jurisprudencia")
    return(NULL)
  }
  
  page <- httr::content(response, "parsed", encoding = "UTF-8")
  
  jurisprudencia <- tryCatch({
    items <- page %>% rvest::html_nodes(".jurisprudencia-item, .case-law-item, article, li")
    
    datos <- lapply(items, function(item) {
      titulo <- safe_text(item %>% rvest::html_nodes("h2, h3, a, .title") %>% .[1])
      enlace <- safe_attr(item %>% rvest::html_nodes("a") %>% .[1], "href")
      fecha <- safe_text(item %>% rvest::html_nodes(".date, time, .fecha") %>% .[1])
      materia <- safe_text(item %>% rvest::html_nodes(".materia, .subject, .topic") %>% .[1])
      resumen <- safe_text(item %>% rvest::html_nodes("p, .summary, .excerpt") %>% .[1])
      
      if (!is.na(enlace) && !grepl("^http", enlace)) {
        enlace <- paste0(url_base, ifelse(substr(enlace, 1, 1) != "/", paste0("/", enlace), enlace))
      }
      
      data.frame(
        tipo = "jurisprudencia",
        titulo = limpiar_texto(titulo),
        enlace = enlace,
        fecha = parse_fecha(fecha),
        materia = limpiar_texto(materia),
        resumen = limpiar_texto(resumen),
        fuente = "TDLC",
        fecha_scraping = get_timestamp(),
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, datos)
    
  }, error = function(e) {
    log_error(sprintf("Error al scrapear jurisprudencia: %s", e$message))
    return(NULL)
  })
  
  if (!is.null(jurisprudencia) && nrow(jurisprudencia) > 0) {
    log_success(sprintf("Se extrajeron %d registros de jurisprudencia", nrow(jurisprudencia)))
  } else {
    log_message("No se extrajeron registros de jurisprudencia")
    jurisprudencia <- data.frame(
      tipo = character(),
      titulo = character(),
      enlace = character(),
      fecha = as.Date(character()),
      materia = character(),
      resumen = character(),
      fuente = character(),
      fecha_scraping = character(),
      stringsAsFactors = FALSE
    )
  }
  
  return(jurisprudencia)
}

jurisprudencia_tdlc <- scrape_jurisprudencia_tdlc()
if (!is.null(jurisprudencia_tdlc)) {
  todos_datos_tdlc$jurisprudencia <- jurisprudencia_tdlc
}

# ============================================================================
# 4. CONSOLIDAR Y GUARDAR DATOS
# ============================================================================
log_message("Consolidando datos de TDLC...")

# Combinar todos los dataframes
datos_tdlc <- do.call(rbind, todos_datos_tdlc)

if (!is.null(datos_tdlc) && nrow(datos_tdlc) > 0) {
  # Normalizar columnas
  datos_tdlc <- normalizar_columnas(datos_tdlc)
  
  # Guardar en formatos RDS y CSV
  guardar_rds(datos_tdlc, "tdlc_completo", "data/raw")
  guardar_csv(datos_tdlc, "tdlc_completo", "data/raw")
  
  log_success(sprintf("Total de datos TDLC guardados: %d registros", nrow(datos_tdlc)))
  
  # Mostrar resumen
  cat("\n=== RESUMEN TDLC ===\n")
  print(table(datos_tdlc$tipo))
  cat("\n")
  
} else {
  log_error("No se obtuvieron datos de TDLC para guardar")
}

log_success("Scraping de TDLC completado")
