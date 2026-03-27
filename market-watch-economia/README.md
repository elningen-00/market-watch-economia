# market-watch-economia

Observatorio de regulación de mercados, inversión extranjera y fomento productivo del Ministerio de Economía.

## Descripción

Este proyecto consiste exclusivamente en scripts de scraping y análisis de datos en R para monitorear:

- Normativas y oficios del Ministerio de Economía
- Boletines, investigaciones y resoluciones de la FNE
- Jurisprudencia y causas del TDLC
- Programas de fomento e innovación de CORFO
- Regulaciones de energía verde (Ministerio de Energía)
- Regulaciones mineras y litio (Ministerio de Minería)

**Nota:** Este proyecto NO incluye páginas web ni aplicaciones Shiny. Todo el código se ejecuta localmente.

## Estructura del Proyecto

```
market-watch-economia/
├── README.md
├── config/
│   └── config.yml          # URLs de fuentes de datos
├── data/
│   ├── raw/                # Descargas sin procesar
│   └── processed/          # Datos limpios (RDS, CSV)
├── scripts/
│   ├── utils.R             # Funciones auxiliares
│   ├── 01_scrape_fne.R     # Scraping FNE
│   ├── 02_scrape_tdlc.R    # Scraping TDLC
│   ├── 03_scrape_corfo.R   # Scraping CORFO
│   └── 05_clean_join.R     # Limpieza y consolidación
├── analysis/
│   ├── 01_analisis_licitaciones.Rmd
│   ├── 02_tendencias_normativas.Rmd
│   └── figures/            # Gráficos generados
└── renv/                   # Reproducibilidad de paquetes
```

## Requisitos

- R (versión 4.0 o superior)
- RStudio (recomendado)
- Paquetes de R: `rvest`, `httr`, `tidyverse`, `jsonlite`, `lubridate`, `ggplot2`, `yaml`

## Instalación

### 1. Clonar el repositorio

```bash
git clone https://github.com/TU_USUARIO/market-watch-economia.git
cd market-watch-economia
```

### 2. Instalar paquetes necesarios

En R o RStudio, ejecuta:

```r
install.packages(c("rvest", "httr", "tidyverse", "jsonlite", "lubridate", "ggplot2", "yaml", "dplyr", "readr", "stringr"))
```

### Opcional: Usar renv para reproducibilidad

```r
install.packages("renv")
renv::init()
renv::install()
```

## Configuración

El archivo `config/config.yml` contiene las URLs de las fuentes de datos. Puedes modificarlo si las URLs cambian:

```yaml
fuentes:
  fne:
    url_base: "https://www.fne.gob.cl"
    boletines: "/boletines/"
    investigaciones: "/investigaciones/"
  
  tdlc:
    url_base: "https://www.tdlc.cl"
    jurisprudencia: "/jurisprudencia/"
    causas: "/causas/"
  
  corfo:
    url_base: "https://www.corfo.cl"
    programas: "/programas/"
    noticias: "/noticias/"
```

## Uso

### Ejecutar scripts de scraping

Los scripts deben ejecutarse en orden numérico desde la carpeta `scripts/`:

1. **Funciones auxiliares** (siempre primero):
   ```r
   source("scripts/utils.R")
   ```

2. **Scraping de fuentes**:
   ```r
   source("scripts/01_scrape_fne.R")      # FNE
   source("scripts/02_scrape_tdlc.R")     # TDLC
   source("scripts/03_scrape_corfo.R")    # CORFO
   ```

3. **Limpieza y consolidación**:
   ```r
   source("scripts/05_clean_join.R")
   ```

### Generar informes

Para obtener visualizaciones y análisis, renderiza los archivos `.Rmd` desde la carpeta `analysis/`:

- `01_analisis_licitaciones.Rmd` - Análisis de tendencias en licitaciones
- `02_tendencias_normativas.Rmd` - Tendencias en normativas y resoluciones

En RStudio, abre cada archivo `.Rmd` y haz clic en **"Knit"** para generar el informe en HTML/PDF.

## Actualización Continua

Para mantener los datos actualizados:

1. Ejecuta nuevamente los scripts de scraping en orden
2. Los nuevos datos se guardarán en `data/raw/` con timestamps
3. Ejecuta `05_clean_join.R` para consolidar
4. Renderiza los informes para ver las actualizaciones

**Recomendación:** Programa la ejecución de los scripts periódicamente (ej. semanalmente) usando:
- **Linux/Mac:** cron jobs
- **Windows:** Task Scheduler
- **R:** paquete `scheduleR`

## Notas Importantes

### Politeness en Scraping

Todos los scripts incluyen:
- Tiempos de espera entre requests (`Sys.sleep()`)
- Headers apropiados para identificar el user-agent
- Manejo de errores y reintentos

**Respeta siempre:**
- Los términos de servicio de cada sitio
- Los tiempos de espera configurados
- No hacer requests masivos en corto tiempo

### Almacenamiento de Datos

- **data/raw/:** Datos crudos descargados (no modificar)
- **data/processed/:** Datos limpios y consolidados listos para análisis

### Solución de Problemas

1. **Error de conexión:** Verifica tu conexión a internet y que las URLs estén accesibles
2. **Error de parsing:** Los sitios pueden cambiar su estructura; revisa los selectores CSS en los scripts
3. **Datos vacíos:** Algunos sitios pueden tener cambios temporales; intenta ejecutar más tarde

## Licencia

Este proyecto es de uso interno para el Ministerio de Economía.

## Contacto

Para dudas o sugerencias, contactar al equipo de datos del Ministerio de Economía.
