# 05_clean_join.R - Limpieza y consolidación de datos
# Descripción: Une todos los datos scrapeados, limpia y genera datasets procesados

# Cargar funciones auxiliares
source("scripts/utils.R")

log_success("Iniciando proceso de limpieza y consolidación...")

# ============================================================================
# 1. CARGAR TODOS LOS DATOS RAW
# ============================================================================
log_message("Cargando datos raw...")

# Función para cargar el archivo más reciente de un tipo
cargar_archivo_reciente <- function(pattern, directory = "data/raw") {
  archivos <- list.files(directory, pattern = pattern, full.names = TRUE)
  
  if (length(archivos) == 0) {
    log_message(sprintf("No se encontraron archivos que coincidan con: %s", pattern))
    return(NULL)
  }
  
  # Ordenar por fecha de modificación (más reciente primero)
  archivos_ordenados <- archivos[order(file.mtime(archivos), decreasing = TRUE)]
  archivo_reciente <- archivos_ordenados[1]
  
  log_message(sprintf("Archivo más reciente: %s", basename(archivo_reciente)))
  return(readRDS(archivo_reciente))
}

# Cargar datos de cada fuente
datos_fne <- cargar_archivo_reciente("fne_completo")
datos_tdlc <- cargar_archivo_reciente("tdlc_completo")
datos_corfo <- cargar_archivo_reciente("corfo_completo")

# Reportar carga
log_message("Resumen de carga:")
cat(sprintf("  - FNE: %s registros\n", ifelse(is.null(datos_fne), "0", nrow(datos_fne))))
cat(sprintf("  - TDLC: %s registros\n", ifelse(is.null(datos_tdlc), "0", nrow(datos_tdlc))))
cat(sprintf("  - CORFO: %s registros\n", ifelse(is.null(datos_corfo), "0", nrow(datos_corfo))))

# ============================================================================
# 2. LIMPIEZA DE DATOS
# ============================================================================
log_message("Iniciando limpieza de datos...")

limpiar_dataset <- function(data, fuente) {
  if (is.null(data) || nrow(data) == 0) {
    log_message(sprintf("Dataset de %s vacío, saltando...", fuente))
    return(NULL)
  }
  
  log_message(sprintf("Limpiando datos de %s (%d registros)...", fuente, nrow(data)))
  
  # Eliminar filas completamente duplicadas
  data_sin_duplicados <- unique(data)
  log_message(sprintf("  - Después de eliminar duplicados: %d registros", nrow(data_sin_duplicados)))
  
  # Eliminar filas sin título
  data_sin_duplicados <- data_sin_duplicados[!is.na(data_sin_duplicados$titulo) & 
                                               data_sin_duplicados$titulo != "", ]
  log_message(sprintf("  - Después de eliminar sin título: %d registros", nrow(data_sin_duplicados)))
  
  # Estandarizar nombres de columnas (ya deberían estar normalizadas)
  data_limpio <- normalizar_columnas(data_sin_duplicados)
  
  # Agregar columna de fuente si no existe
  if (!"fuente" %in% names(data_limpio)) {
    data_limpio$fuente <- fuente
  }
  
  # Convertir fechas correctamente
  if ("fecha" %in% names(data_limpio)) {
    data_limpio$fecha <- as.Date(data_limpio$fecha)
  }
  
  # Agregar fecha de procesamiento
  data_limpio$fecha_procesamiento <- get_timestamp()
  
  return(data_limpio)
}

# Limpiar cada dataset
datos_fne_limpios <- limpiar_dataset(datos_fne, "FNE")
datos_tdlc_limpios <- limpiar_dataset(datos_tdlc, "TDLC")
datos_corfo_limpios <- limpiar_dataset(datos_corfo, "CORFO")

# ============================================================================
# 3. UNIR TODOS LOS DATOS
# ============================================================================
log_message("Uniendo todos los datasets...")

# Lista con todos los datasets válidos
datasets_validos <- list(
  FNE = datos_fne_limpios,
  TDLC = datos_tdlc_limpios,
  CORFO = datos_corfo_limpios
)

# Filtrar NULLs
datasets_validos <- datasets_validos[!sapply(datasets_validos, is.null)]

if (length(datasets_validos) == 0) {
  log_error("No hay datos válidos para unir. Ejecuta primero los scripts de scraping.")
  quit(status = 1)
}

# Unir todos los dataframes
datos_consolidados <- do.call(rbind, datasets_validos)

log_success(sprintf("Total de registros consolidados: %d", nrow(datos_consolidados)))

# ============================================================================
# 4. ANÁLISIS EXPLORATORIO BÁSICO
# ============================================================================
log_message("Generando análisis exploratorio...")

# Resumen por fuente
resumen_fuente <- as.data.frame(table(datos_consolidados$fuente))
names(resumen_fuente) <- c("fuente", "cantidad")
cat("\n=== RESUMEN POR FUENTE ===\n")
print(resumen_fuente)

# Resumen por tipo
if ("tipo" %in% names(datos_consolidados)) {
  resumen_tipo <- as.data.frame(table(datos_consolidados$tipo))
  names(resumen_tipo) <- c("tipo", "cantidad")
  cat("\n=== RESUMEN POR TIPO ===\n")
  print(resumen_tipo)
}

# Resumen temporal (si hay fechas)
if ("fecha" %in% names(datos_consolidados)) {
  fechas_validas <- datos_consolidados$fecha[!is.na(datos_consolidados$fecha)]
  if (length(fechas_validas) > 0) {
    cat("\n=== RANGO TEMPORAL ===\n")
    cat(sprintf("Desde: %s\n", min(fechas_validas)))
    cat(sprintf("Hasta: %s\n", max(fechas_validas)))
    cat(sprintf("Registros con fecha: %d de %d\n", length(fechas_validas), nrow(datos_consolidados)))
  }
}

# ============================================================================
# 5. GUARDAR DATOS PROCESADOS
# ============================================================================
log_message("Guardando datos procesados...")

# Guardar dataset completo consolidado
guardar_rds(datos_consolidados, "datos_consolidados", "data/processed")
guardar_csv(datos_consolidados, "datos_consolidados", "data/processed")

# Guardar resúmenes por fuente
if (!is.null(datos_fne_limpios)) {
  guardar_rds(datos_fne_limpios, "fne_procesado", "data/processed")
  guardar_csv(datos_fne_limpios, "fne_procesado", "data/processed")
}

if (!is.null(datos_tdlc_limpios)) {
  guardar_rds(datos_tdlc_limpios, "tdlc_procesado", "data/processed")
  guardar_csv(datos_tdlc_limpios, "tdlc_procesado", "data/processed")
}

if (!is.null(datos_corfo_limpios)) {
  guardar_rds(datos_corfo_limpios, "corfo_procesado", "data/processed")
  guardar_csv(datos_corfo_limpios, "corfo_procesado", "data/processed")
}

# ============================================================================
# 6. GENERAR METADATOS
# ============================================================================
log_message("Generando metadatos...")

metadatos <- list(
  fecha_generacion = get_timestamp(),
  total_registros = nrow(datos_consolidados),
  fuentes = names(datasets_validos),
  columnas = names(datos_consolidados),
  resumen_por_fuente = as.list(resumen_fuente$cantidad),
  rango_fechas = list(
    desde = if("fecha" %in% names(datos_consolidados)) min(datos_consolidados$fecha, na.rm = TRUE) else NA,
    hasta = if("fecha" %in% names(datos_consolidados)) max(datos_consolidados$fecha, na.rm = TRUE) else NA
  )
)

# Guardar metadatos como JSON
metadata_path <- "data/processed/metadatos.json"
jsonlite::write_json(metadatos, metadata_path, pretty = TRUE, auto_unbox = TRUE)
log_success(sprintf("Metadatos guardados en: %s", metadata_path))

# ============================================================================
# 7. RESUMEN FINAL
# ============================================================================
cat("\n")
cat("========================================\n")
log_success("PROCESO DE LIMPIEZA Y CONSOLIDACIÓN COMPLETADO")
cat("========================================\n")
cat(sprintf("Total de registros procesados: %d\n", nrow(datos_consolidados)))
cat(sprintf("Archivos guardados en: data/processed/\n"))
cat(sprintf("Fecha de procesamiento: %s\n", get_timestamp()))
cat("========================================\n")
