# market-watch-economia

Observatorio de regulación de mercados, inversión extranjera y fomento productivo del Ministerio de Economía. El proyecto consiste exclusivamente en scripts de scraping y análisis de datos en R. No incluye páginas web ni aplicaciones Shiny.

## Descripción

Este proyecto recopila, procesa y analiza datos de múltiples fuentes gubernamentales relacionadas con:

- Regulación de mercados
- Inversión extranjera
- Fomento productivo
- Competencia económica
- Licitaciones públicas

## Fuentes de Datos

| Fuente | URL | Tipo | Datos Obtenidos |
|--------|-----|------|-----------------|
| Ministerio de Economía | economia.gob.cl | Scraping | Normativas, oficios |
| FNE | fne.gob.cl | Scraping | Boletines, investigaciones, resoluciones |
| TDLC | tdlc.cl | Scraping | Jurisprudencia, causas |
| CORFO | corfo.cl | Scraping | Programas de fomento, innovación |
| Ministerio de Energía | energia.gob.cl | Scraping | Hidrógeno verde |
| Ministerio de Minería | mineria.gob.cl | Scraping | Litio, regulación |

## Estructura del Proyecto

```
market-watch-economia/
├── README.md
├── config/
│   └── config.yml          # URLs de fuentes de scraping
├── data/
│   ├── raw/                # Descargas sin procesar
│   └── processed/          # Datos limpios (RDS, CSV)
├── scripts/
│   ├── 01_scrape_fne.R
│   ├── 02_scrape_tdlc.R
│   ├── 03_scrape_corfo.R
│   ├── 05_clean_join.R
│   └── utils.R             # Funciones auxiliares
├── analysis/
│   ├── 01_analisis_corfo.Rmd
│   ├── 02_tendencias_normativas.Rmd
│   └── figures/            # Gráficos generados
└── renv/                   # Para reproducibilidad
```

## Requisitos

### Paquetes de R

El proyecto utiliza los siguientes paquetes:

- **rvest**: Web scraping
- **httr**: Peticiones HTTP
- **tidyverse**: Manipulación de datos (dplyr, ggplot2, tidyr, etc.)
- **jsonlite**: Manejo de JSON (API)
- **lubridate**: Manejo de fechas
- **ggplot2**: Visualización de datos
- **yaml**: Lectura de configuración
- **scales**: Formateo de ejes en gráficos
- **gt**: Tablas profesionales
- **wordcloud2**: Nubes de palabras

### Instalación de dependencias

Se recomienda usar `renv` para la reproducibilidad:

```r
# Instalar renv si no está disponible
install.packages("renv")

# Inicializar el entorno
renv::init()

# Restaurar el entorno desde lockfile (si existe)
renv::restore()
```

O instalar manualmente los paquetes requeridos:

```r
install.packages(c(
  "rvest", "httr", "tidyverse", "jsonlite", 
  "lubridate", "ggplot2", "yaml", "scales", 
  "gt", "wordcloud2", "tm", "SnowballC"
))
```

## Configuración

El archivo `config/config.yml` contiene las URLs de todas las fuentes de datos y los parámetros de scraping. No se requiere configuración adicional.

## Instrucciones de Uso

### Ejecución de Scripts

Ejecutar los scripts en orden numérico desde la carpeta `scripts/`:

```r
# Desde la raíz del proyecto
source("scripts/utils.R")
source("scripts/01_scrape_fne.R")
source("scripts/02_scrape_tdlc.R")
source("scripts/03_scrape_corfo.R")
source("scripts/05_clean_join.R")
```

**Nota:** Los scripts de scraping pueden tardar varios minutos dependiendo de la cantidad de datos y la velocidad de conexión.

### Flujo de Trabajo

1. **Scraping**: Los scripts `01_*` a `03_*` descargan datos crudos
2. **Limpieza**: El script `05_clean_join.R` procesa y consolida los datos
3. **Análisis**: Los archivos `.Rmd` en `analysis/` generan informes

### Ubicación de Datos

- **Datos crudos**: Se guardan en `data/raw/` con formato `{fuente}_{fecha}.rds` y `{fuente}_{fecha}.csv`
- **Datos procesados**: Se guardan en `data/processed/` listos para análisis

### Generación de Informes

Renderizar los informes `.Rmd` desde RStudio o mediante línea de comandos:

```r
# Desde RStudio: Botón "Knit" en cada archivo .Rmd

# O desde línea de comandos
rmarkdown::render("analysis/01_analisis_corfo.Rmd")
rmarkdown::render("analysis/02_tendencias_normativas.Rmd")
```

Los gráficos generados se guardan en `analysis/figures/`.

## Consideraciones Importantes

### Politeness en Scraping

Los scripts implementan políticas de respeto a los servidores:

- Delay aleatorio entre 2-5 segundos entre requests
- Máximo 3 reintentos en caso de error
- User-Agent identificable

### Consideraciones Legales

- Verificar los términos de uso de cada sitio antes de hacer scraping
- Respetar robots.txt cuando esté disponible
- Usar los datos únicamente para fines institucionales autorizados

### Manejo de Errores

Los scripts están diseñados para:

- Continuar ejecutándose si una fuente falla
- Guardar logs de actividad
- Alertar sobre datos faltantes

## Solución de Problemas

### Error: "No se encontraron datos procesados"

Ejecute primero los scripts de scraping en orden.

### Error de conexión o timeout

- Verifique su conexión a internet
- Aumente el timeout en `config/config.yml`
- Los sitios pueden estar temporalmente no disponibles

## Licencia

Uso exclusivo institucional - Ministerio de Economía, Fomento y Turismo de Chile.

---

*Última actualización: `r Sys.Date()`*
