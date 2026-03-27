# 01_scrape_fne.R - Scraping de la Fiscalía Nacional Económica (FNE)
# Fuente: https://www.fne.gob.cl
# Descripción: Extrae boletines, investigaciones y resoluciones

# Cargar funciones auxiliares
source("scripts/utils.R")

log_success("Iniciando scraping de FNE...")

# Configuración específica para FNE
fne_config <- config$fuentes$fne
url_base <- fne_config$url_base

# Lista para almacenar todos los datos
todos_datos_fne <- list()

# ============================================================================
# 1. SCRAPING DE BOLETINES
# ============================================================================
log_message("Scrapeando boletines de FNE...")

scrape_boletines_fne <- function() {
  url_boletines <- paste0(url_base, fne_config$boletines)
  
  response <- scrape_url(url_boletines)
  if (is.null(response)) {
    log_error("No se pudo obtener la página de boletines")
    return(NULL)
  }
  
  page <- httr::content(response, "parsed", encoding = "UTF-8")
  
  # NOTA: Los selectores CSS deben ajustarse según la estructura real del sitio
  # Estos son ejemplos genéricos que probablemente necesiten modificación
  boletines <- tryCatch({
    items <- page %>% rvest::html_nodes(".boletin-item, .news-item, article, .post")
    
    if (length(items) == 0) {
      log_message("No se encontraron boletines con selectores comunes, intentando alternativa...")
      items <- page %>% rvest::html_nodes("li, div")
    }
    
    datos <- lapply(items, function(item) {
      titulo <- safe_text(item %>% rvest::html_nodes("h2, h3, .title, a") %>% .[1])
      enlace <- safe_attr(item %>% rvest::html_nodes("a") %>% .[1], "href")
      fecha <- safe_text(item %>% rvest::html_nodes(".date, time, .fecha, span") %>% .[1])
      resumen <- safe_text(item %>% rvest::html_nodes("p, .summary, .excerpt") %>% .[1])
      
      # Construir URL completa si es relativa
      if (!is.na(enlace) && !grepl("^http", enlace)) {
        enlace <- paste0(url_base, ifelse(substr(enlace, 1, 1) != "/", paste0("/", enlace), enlace))
      }
      
      data.frame(
        tipo = "boletin",
        titulo = limpiar_texto(titulo),
        enlace = enlace,
        fecha = parse_fecha(fecha),
        resumen = limpiar_texto(resumen),
        fuente = "FNE",
        fecha_scraping = get_timestamp(),
        stringsAsFactors = FALSE
      )
    })
    
    # Combinar todos los dataframes
    do.call(rbind, datos)
    
  }, error = function(e) {
    log_error(sprintf("Error al scrapear boletines: %s", e$message))
    return(NULL)
  })
  
  if (!is.null(boletines) && nrow(boletines) > 0) {
    log_success(sprintf("Se extrajeron %d boletines", nrow(boletines)))
  } else {
    log_message("No se extrajeron boletines o la estructura del sitio cambió")
    boletines <- data.frame(
      tipo = character(),
      titulo = character(),
      enlace = character(),
      fecha = as.Date(character()),
      resumen = character(),
      fuente = character(),
      fecha_scraping = character(),
      stringsAsFactors = FALSE
    )
  }
  
  return(boletines)
}

boletines_fne <- scrape_boletines_fne()
if (!is.null(boletines_fne)) {
  todos_datos_fne$boletines <- boletines_fne
}

esperar_request("fne")

# ============================================================================
# 2. SCRAPING DE INVESTIGACIONES
# ============================================================================
log_message("Scrapeando investigaciones de FNE...")

scrape_investigaciones_fne <- function() {
  url_investigaciones <- paste0(url_base, fne_config$investigaciones)
  
  response <- scrape_url(url_investigaciones)
  if (is.null(response)) {
    log_error("No se pudo obtener la página de investigaciones")
    return(NULL)
  }
  
  page <- httr::content(response, "parsed", encoding = "UTF-8")
  
  investigaciones <- tryCatch({
    items <- page %>% rvest::html_nodes(".investigacion-item, .case-item, article, li")
    
    datos <- lapply(items, function(item) {
      titulo <- safe_text(item %>% rvest::html_nodes("h2, h3, h4, a") %>% .[1])
      enlace <- safe_attr(item %>% rvest::html_nodes("a") %>% .[1], "href")
      fecha <- safe_text(item %>% rvest::html_nodes(".date, time, .fecha") %>% .[1])
      categoria <- safe_text(item %>% rvest::html_nodes(".category, .categoria, .tag") %>% .[1])
      
      if (!is.na(enlace) && !grepl("^http", enlace)) {
        enlace <- paste0(url_base, ifelse(substr(enlace, 1, 1) != "/", paste0("/", enlace), enlace))
      }
      
      data.frame(
        tipo = "investigacion",
        titulo = limpiar_texto(titulo),
        enlace = enlace,
        fecha = parse_fecha(fecha),
        categoria = limpiar_texto(categoria),
        fuente = "FNE",
        fecha_scraping = get_timestamp(),
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, datos)
    
  }, error = function(e) {
    log_error(sprintf("Error al scrapear investigaciones: %s", e$message))
    return(NULL)
  })
  
  if (!is.null(investigaciones) && nrow(investigaciones) > 0) {
    log_success(sprintf("Se extrajeron %d investigaciones", nrow(investigaciones)))
  } else {
    log_message("No se extrajeron investigaciones")
    investigaciones <- data.frame(
      tipo = character(),
      titulo = character(),
      enlace = character(),
      fecha = as.Date(character()),
      categoria = character(),
      fuente = character(),
      fecha_scraping = character(),
      stringsAsFactors = FALSE
    )
  }
  
  return(investigaciones)
}

investigaciones_fne <- scrape_investigaciones_fne()
if (!is.null(investigaciones_fne)) {
  todos_datos_fne$investigaciones <- investigaciones_fne
}

esperar_request("fne")

# ============================================================================
# 3. SCRAPING DE RESOLUCIONES
# ============================================================================
log_message("Scrapeando resoluciones de FNE...")

scrape_resoluciones_fne <- function() {
  url_resoluciones <- paste0(url_base, fne_config$resoluciones)
  
  response <- scrape_url(url_resoluciones)
  if (is.null(response)) {
    log_error("No se pudo obtener la página de resoluciones")
    return(NULL)
  }
  
  page <- httr::content(response, "parsed", encoding = "UTF-8")
  
  resoluciones <- tryCatch({
    items <- page %>% rvest::html_nodes(".resolucion-item, .resolution-item, article, li")
    
    datos <- lapply(items, function(item) {
      titulo <- safe_text(item %>% rvest::html_nodes("h2, h3, a") %>% .[1])
      enlace <- safe_attr(item %>% rvest::html_nodes("a") %>% .[1], "href")
      numero <- safe_text(item %>% rvest::html_nodes(".numero, .number, .exento") %>% .[1])
      fecha <- safe_text(item %>% rvest::html_nodes(".date, time, .fecha") %>% .[1])
      
      if (!is.na(enlace) && !grepl("^http", enlace)) {
        enlace <- paste0(url_base, ifelse(substr(enlace, 1, 1) != "/", paste0("/", enlace), enlace))
      }
      
      data.frame(
        tipo = "resolucion",
        titulo = limpiar_texto(titulo),
        numero = limpiar_texto(numero),
        enlace = enlace,
        fecha = parse_fecha(fecha),
        fuente = "FNE",
        fecha_scraping = get_timestamp(),
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, datos)
    
  }, error = function(e) {
    log_error(sprintf("Error al scrapear resoluciones: %s", e$message))
    return(NULL)
  })
  
  if (!is.null(resoluciones) && nrow(resoluciones) > 0) {
    log_success(sprintf("Se extrajeron %d resoluciones", nrow(resoluciones)))
  } else {
    log_message("No se extrajeron resoluciones")
    resoluciones <- data.frame(
      tipo = character(),
      titulo = character(),
      numero = character(),
      enlace = character(),
      fecha = as.Date(character()),
      fuente = character(),
      fecha_scraping = character(),
      stringsAsFactors = FALSE
    )
  }
  
  return(resoluciones)
}

resoluciones_fne <- scrape_resoluciones_fne()
if (!is.null(resoluciones_fne)) {
  todos_datos_fne$resoluciones <- resoluciones_fne
}

# ============================================================================
# 4. CONSOLIDAR Y GUARDAR DATOS
# ============================================================================
log_message("Consolidando datos de FNE...")

# Combinar todos los dataframes
datos_fne <- do.call(rbind, todos_datos_fne)

if (!is.null(datos_fne) && nrow(datos_fne) > 0) {
  # Normalizar columnas
  datos_fne <- normalizar_columnas(datos_fne)
  
  # Guardar en formatos RDS y CSV
  guardar_rds(datos_fne, "fne_completo", "data/raw")
  guardar_csv(datos_fne, "fne_completo", "data/raw")
  
  log_success(sprintf("Total de datos FNE guardados: %d registros", nrow(datos_fne)))
  
  # Mostrar resumen
  cat("\n=== RESUMEN FNE ===\n")
  print(table(datos_fne$tipo))
  cat("\n")
  
} else {
  log_error("No se obtuvieron datos de FNE para guardar")
}

log_success("Scraping de FNE completado")
