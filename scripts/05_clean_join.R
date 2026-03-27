# 05_clean_join.R
# Limpieza y consolidación de datos de todas las fuentes
# Genera datasets procesados listos para análisis

library(tidyverse)
library(lubridate)
library(stringr)

# Cargar funciones auxiliares
source("scripts/utils.R")

# Configurar
config <- load_config()

log_activity("Iniciando proceso de limpieza y consolidación")

# Función para normalizar fechas
normalize_fecha <- function(fecha_str) {
  if (is.na(fecha_str) || fecha_str == "") {
    return(NA)
  }
  
  # Intentar múltiples formatos
  formatos <- c(
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%d-%m-%Y",
    "%Y/%m/%d",
    "%d de %B de %Y",
    "%B %d, %Y"
  )
  
  for (fmt in formatos) {
    fecha_parsed <- tryCatch({
      parse_date_time(fecha_str, orders = fmt, locale = "es_CL")
    }, error = function(e) NA)
    
    if (!is.na(fecha_parsed)) {
      return(as.Date(fecha_parsed))
    }
  }
  
  # Si ningún formato funciona, retornar NA
  return(NA)
}

# Función para limpiar texto
clean_text <- function(text) {
  text %>%
    str_remove_all("<[^>]*>") %>%  # Remover HTML tags
    str_squish() %>%                # Normalizar espacios
    str_to_lower()                  # Convertir a minúsculas
}

# Función para normalizar URLs
normalize_url <- function(url, base_url) {
  if (is.na(url) || url == "") {
    return(NA)
  }
  
  if (!str_starts(url, "http")) {
    return(paste0(base_url, url))
  }
  
  return(url)
}

# Cargar datos raw más recientes
load_latest_raw <- function(prefix) {
  raw_path <- config$data$raw_path
  
  # Buscar archivos que coincidan con el prefijo
  archivos <- list.files(raw_path, pattern = paste0("^", prefix, ".*\\.rds$"), 
                         full.names = TRUE)
  
  if (length(archivos) == 0) {
    warning(sprintf("No se encontraron archivos raw con prefijo: %s", prefix))
    return(NULL)
  }
  
  # Obtener el más reciente
  archivo_reciente <- archivos[which.max(file.info(archivos)$mtime)]
  
  log_activity(sprintf("Cargando archivo: %s", basename(archivo_reciente)))
  return(readRDS(archivo_reciente))
}

# Cargar todos los datos
fne_data <- load_latest_raw("fne_")
tdlc_data <- load_latest_raw("tdlc_")
corfo_data <- load_latest_raw("corfo_")

# ============================================
# LIMPIEZA POR FUENTE
# ============================================

# --- FNE ---
if (!is.null(fne_data)) {
  fne_clean <- fne_data %>%
    mutate(
      titulo_limpio = clean_text(titulo),
      fecha_normalizada = normalize_fecha(fecha),
      fuente = "FNE",
      categoria = case_when(
        tipo == "boletin" ~ "Comunicación",
        tipo == "investigacion" ~ "Investigación",
        tipo == "resolucion" ~ "Resolución",
        TRUE ~ "Otro"
      )
    ) %>%
    select(titulo = titulo, titulo_limpio, fecha_original = fecha, 
           fecha_normalizada, resumen, area, enlace, tipo, categoria, 
           fuente, fecha_scraping)
  
  log_activity(sprintf("FNE: %d registros limpiados", nrow(fne_clean)))
} else {
  fne_clean <- NULL
}

# --- TDLC ---
if (!is.null(tdlc_data)) {
  tdlc_clean <- tdlc_data %>%
    mutate(
      titulo_limpio = clean_text(titulo %||% caratula %||% rol),
      fecha_normalizada = normalize_fecha(fecha),
      fuente = "TDLC",
      categoria = case_when(
        tipo == "jurisprudencia" ~ "Jurisprudencia",
        tipo == "causa" ~ "Causa",
        TRUE ~ "Otro"
      )
    ) %>%
    select(rol, titulo, titulo_limpio, materia, estado, 
           fecha_original = fecha, fecha_normalizada, enlace, 
           tipo, categoria, fuente, fecha_scraping)
  
  log_activity(sprintf("TDLC: %d registros limpiados", nrow(tdlc_clean)))
} else {
  tdlc_clean <- NULL
}

# --- CORFO ---
if (!is.null(corfo_data)) {
  corfo_clean <- corfo_data %>%
    mutate(
      titulo_limpio = clean_text(nombre %||% titulo),
      fecha_normalizada = normalize_fecha(fecha_apertura %||% fecha_cierre),
      fuente = "CORFO",
      categoria = case_when(
        tipo == "programa" ~ "Programa de Fomento",
        tipo == "innovacion" ~ "Innovación",
        tipo == "convocatoria" ~ "Convocatoria",
        TRUE ~ "Otro"
      ),
      monto_numerico = case_when(
        !is.na(monto) & str_detect(monto, "UF") ~ 
          as.numeric(str_extract(monto, "[0-9,\\.]+")) * 32000,  # Aproximado
        !is.na(monto) & str_detect(monto, "\\$") ~ 
          as.numeric(str_extract_all(monto, "[0-9,\\.]+")[[1]]) %>% 
          sum(na.rm = TRUE),
        TRUE ~ NA_real_
      )
    ) %>%
    select(nombre = nombre, titulo, titulo_limpio, descripcion, area, sector,
           region, beneficiarios, monto, monto_numerico, estado,
           fecha_apertura, fecha_cierre, fecha_normalizada, 
           enlace, tipo, categoria, fuente, fecha_scraping)
  
  log_activity(sprintf("CORFO: %d registros limpiados", nrow(corfo_clean)))
} else {
  corfo_clean <- NULL
}

# ============================================
# CONSOLIDACIÓN
# ============================================

# Unir todos los datasets con estructura común
consolidar_datasets <- function(...) {
  datasets <- list(...)
  
  # Encontrar columnas comunes
  columnas_comunes <- reduce(datasets, function(x, y) {
    intersect(names(x), names(y))
  })
  
  # Seleccionar solo columnas comunes y unir
  datasets_common <- map(datasets, ~{
    .x %>% select(any_of(columnas_comunes))
  })
  
  bind_rows(datasets_common)
}

# Consolidar todos los datos
datos_consolidados <- consolidar_datasets(
  fne_clean,
  tdlc_clean,
  corfo_clean
)

if (!is.null(datos_consolidados)) {
  # Agregar metadatos
  datos_consolidados <- datos_consolidados %>%
    mutate(
      fecha_procesamiento = Sys.time(),
      id_unico = row_number()
    )
  
  log_activity(sprintf("Total registros consolidados: %d", nrow(datos_consolidados)))
  
  # Guardar datos procesados
  fecha_hoy <- format_date_for_filename()
  
  save_processed_data(datos_consolidados, 
                      sprintf("datos_consolidados_%s.rds", fecha_hoy), 
                      "rds", config)
  
  save_processed_data(datos_consolidados, 
                      sprintf("datos_consolidados_%s.csv", fecha_hoy), 
                      "csv", config)
  
  # Crear resumen por fuente
  resumen_fuente <- datos_consolidados %>%
    group_by(fuente, categoria) %>%
    summarise(
      cantidad = n(),
      primera_fecha = min(fecha_normalizada, na.rm = TRUE),
      ultima_fecha = max(fecha_normalizada, na.rm = TRUE),
      .groups = "drop"
    )
  
  save_processed_data(resumen_fuente, 
                      sprintf("resumen_por_fuente_%s.rds", fecha_hoy), 
                      "rds", config)
  
  log_activity("Resumen por fuente guardado")
}

# ============================================
# DATOS ESPECÍFICOS PARA ANÁLISIS
# ============================================

# Crear dataset para tendencias normativas (FNE + TDLC)
if (!is.null(fne_clean) && !is.null(tdlc_clean)) {
  tendencias_normativas <- bind_rows(
    fne_clean %>% filter(categoria %in% c("Resolución", "Investigación")),
    tdlc_clean %>% filter(categoria == "Jurisprudencia")
  ) %>%
    mutate(
      mes = floor_date(fecha_normalizada, "month"),
      anio = year(fecha_normalizada)
    )
  
  save_processed_data(tendencias_normativas,
                      sprintf("tendencias_normativas_%s.rds", fecha_hoy),
                      "rds", config)
  
  log_activity(sprintf("Dataset de tendencias normativas: %d registros",
                       nrow(tendencias_normativas)))
}

log_activity("Finalizado proceso de limpieza y consolidación")
