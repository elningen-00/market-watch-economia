# 03_scrape_corfo.R - Scraping de CORFO
# Fuente: https://www.corfo.cl
# Descripción: Extrae programas de fomento, noticias e innovación

# Cargar funciones auxiliares
source("scripts/utils.R")

log_success("Iniciando scraping de CORFO...")

# Configuración específica para CORFO
corfo_config <- config$fuentes$corfo
url_base <- corfo_config$url_base

# Lista para almacenar todos los datos
todos_datos_corfo <- list()

# ============================================================================
# 1. SCRAPING DE PROGRAMAS
# ============================================================================
log_message("Scrapeando programas de CORFO...")

scrape_programas_corfo <- function() {
  url_programas <- paste0(url_base, corfo_config$programas)
  
  response <- scrape_url(url_programas)
  if (is.null(response)) {
    log_error("No se pudo obtener la página de programas")
    return(NULL)
  }
  
  page <- httr::content(response, "parsed", encoding = "UTF-8")
  
  programas <- tryCatch({
    # Selectores genéricos que deben ajustarse según la estructura real
    items <- page %>% rvest::html_nodes(".programa-item, .program-item, article, li, .card")
    
    if (length(items) == 0) {
      log_message("No se encontraron programas con selectores comunes")
      return(NULL)
    }
    
    datos <- lapply(items, function(item) {
      titulo <- safe_text(item %>% rvest::html_nodes("h2, h3, h4, a, .title") %>% .[1])
      enlace <- safe_attr(item %>% rvest::html_nodes("a") %>% .[1], "href")
      descripcion <- safe_text(item %>% rvest::html_nodes("p, .description, .excerpt, .summary") %>% .[1])
      categoria <- safe_text(item %>% rvest::html_nodes(".category, .categoria, .tag, .area") %>% .[1])
      estado <- safe_text(item %>% rvest::html_nodes(".estado, .status, .vigencia") %>% .[1])
      fecha <- safe_text(item %>% rvest::html_nodes(".date, time, .fecha") %>% .[1])
      
      if (!is.na(enlace) && !grepl("^http", enlace)) {
        enlace <- paste0(url_base, ifelse(substr(enlace, 1, 1) != "/", paste0("/", enlace), enlace))
      }
      
      data.frame(
        tipo = "programa",
        titulo = limpiar_texto(titulo),
        descripcion = limpiar_texto(descripcion),
        enlace = enlace,
        categoria = limpiar_texto(categoria),
        estado = limpiar_texto(estado),
        fecha = parse_fecha(fecha),
        fuente = "CORFO",
        fecha_scraping = get_timestamp(),
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, datos)
    
  }, error = function(e) {
    log_error(sprintf("Error al scrapear programas: %s", e$message))
    return(NULL)
  })
  
  if (!is.null(programas) && nrow(programas) > 0) {
    log_success(sprintf("Se extrajeron %d programas", nrow(programas)))
  } else {
    log_message("No se extrajeron programas")
    programas <- data.frame(
      tipo = character(),
      titulo = character(),
      descripcion = character(),
      enlace = character(),
      categoria = character(),
      estado = character(),
      fecha = as.Date(character()),
      fuente = character(),
      fecha_scraping = character(),
      stringsAsFactors = FALSE
    )
  }
  
  return(programas)
}

programas_corfo <- scrape_programas_corfo()
if (!is.null(programas_corfo)) {
  todos_datos_corfo$programas <- programas_corfo
}

esperar_request("corfo")

# ============================================================================
# 2. SCRAPING DE NOTICIAS
# ============================================================================
log_message("Scrapeando noticias de CORFO...")

scrape_noticias_corfo <- function() {
  url_noticias <- paste0(url_base, corfo_config$noticias)
  
  response <- scrape_url(url_noticias)
  if (is.null(response)) {
    log_error("No se pudo obtener la página de noticias")
    return(NULL)
  }
  
  page <- httr::content(response, "parsed", encoding = "UTF-8")
  
  noticias <- tryCatch({
    items <- page %>% rvest::html_nodes(".noticia-item, .news-item, article, .post, li")
    
    datos <- lapply(items, function(item) {
      titulo <- safe_text(item %>% rvest::html_nodes("h2, h3, h4, a, .title") %>% .[1])
      enlace <- safe_attr(item %>% rvest::html_nodes("a") %>% .[1], "href")
      resumen <- safe_text(item %>% rvest::html_nodes("p, .summary, .excerpt") %>% .[1])
      fecha <- safe_text(item %>% rvest::html_nodes(".date, time, .fecha") %>% .[1])
      categoria <- safe_text(item %>% rvest::html_nodes(".category, .categoria, .tag") %>% .[1])
      
      if (!is.na(enlace) && !grepl("^http", enlace)) {
        enlace <- paste0(url_base, ifelse(substr(enlace, 1, 1) != "/", paste0("/", enlace), enlace))
      }
      
      data.frame(
        tipo = "noticia",
        titulo = limpiar_texto(titulo),
        resumen = limpiar_texto(resumen),
        enlace = enlace,
        categoria = limpiar_texto(categoria),
        fecha = parse_fecha(fecha),
        fuente = "CORFO",
        fecha_scraping = get_timestamp(),
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, datos)
    
  }, error = function(e) {
    log_error(sprintf("Error al scrapear noticias: %s", e$message))
    return(NULL)
  })
  
  if (!is.null(noticias) && nrow(noticias) > 0) {
    log_success(sprintf("Se extrajeron %d noticias", nrow(noticias)))
  } else {
    log_message("No se extrajeron noticias")
    noticias <- data.frame(
      tipo = character(),
      titulo = character(),
      resumen = character(),
      enlace = character(),
      categoria = character(),
      fecha = as.Date(character()),
      fuente = character(),
      fecha_scraping = character(),
      stringsAsFactors = FALSE
    )
  }
  
  return(noticias)
}

noticias_corfo <- scrape_noticias_corfo()
if (!is.null(noticias_corfo)) {
  todos_datos_corfo$noticias <- noticias_corfo
}

esperar_request("corfo")

# ============================================================================
# 3. SCRAPING DE INNOVACIÓN
# ============================================================================
log_message("Scrapeando proyectos de innovación de CORFO...")

scrape_innovacion_corfo <- function() {
  url_innovacion <- paste0(url_base, corfo_config$innovacion)
  
  response <- scrape_url(url_innovacion)
  if (is.null(response)) {
    log_error("No se pudo obtener la página de innovación")
    return(NULL)
  }
  
  page <- httr::content(response, "parsed", encoding = "UTF-8")
  
  innovacion <- tryCatch({
    items <- page %>% rvest::html_nodes(".innovacion-item, .innovation-item, article, .project, li")
    
    datos <- lapply(items, function(item) {
      titulo <- safe_text(item %>% rvest::html_nodes("h2, h3, h4, a, .title") %>% .[1])
      enlace <- safe_attr(item %>% rvest::html_nodes("a") %>% .[1], "href")
      descripcion <- safe_text(item %>% rvest::html_nodes("p, .description, .summary") %>% .[1])
      area <- safe_text(item %>% rvest::html_nodes(".area, .sector, .campo") %>% .[1])
      monto <- safe_text(item %>% rvest::html_nodes(".monto, .amount, .presupuesto") %>% .[1])
      fecha <- safe_text(item %>% rvest::html_nodes(".date, time, .fecha") %>% .[1])
      
      if (!is.na(enlace) && !grepl("^http", enlace)) {
        enlace <- paste0(url_base, ifelse(substr(enlace, 1, 1) != "/", paste0("/", enlace), enlace))
      }
      
      data.frame(
        tipo = "innovacion",
        titulo = limpiar_texto(titulo),
        descripcion = limpiar_texto(descripcion),
        enlace = enlace,
        area = limpiar_texto(area),
        monto = limpiar_texto(monto),
        fecha = parse_fecha(fecha),
        fuente = "CORFO",
        fecha_scraping = get_timestamp(),
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, datos)
    
  }, error = function(e) {
    log_error(sprintf("Error al scrapear innovación: %s", e$message))
    return(NULL)
  })
  
  if (!is.null(innovacion) && nrow(innovacion) > 0) {
    log_success(sprintf("Se extrajeron %d proyectos de innovación", nrow(innovacion)))
  } else {
    log_message("No se extrajeron proyectos de innovación")
    innovacion <- data.frame(
      tipo = character(),
      titulo = character(),
      descripcion = character(),
      enlace = character(),
      area = character(),
      monto = character(),
      fecha = as.Date(character()),
      fuente = character(),
      fecha_scraping = character(),
      stringsAsFactors = FALSE
    )
  }
  
  return(innovacion)
}

innovacion_corfo <- scrape_innovacion_corfo()
if (!is.null(innovacion_corfo)) {
  todos_datos_corfo$innovacion <- innovacion_corfo
}

# ============================================================================
# 4. CONSOLIDAR Y GUARDAR DATOS
# ============================================================================
log_message("Consolidando datos de CORFO...")

# Combinar todos los dataframes
datos_corfo <- do.call(rbind, todos_datos_corfo)

if (!is.null(datos_corfo) && nrow(datos_corfo) > 0) {
  # Normalizar columnas
  datos_corfo <- normalizar_columnas(datos_corfo)
  
  # Guardar en formatos RDS y CSV
  guardar_rds(datos_corfo, "corfo_completo", "data/raw")
  guardar_csv(datos_corfo, "corfo_completo", "data/raw")
  
  log_success(sprintf("Total de datos CORFO guardados: %d registros", nrow(datos_corfo)))
  
  # Mostrar resumen
  cat("\n=== RESUMEN CORFO ===\n")
  print(table(datos_corfo$tipo))
  cat("\n")
  
} else {
  log_error("No se obtuvieron datos de CORFO para guardar")
}

log_success("Scraping de CORFO completado")
