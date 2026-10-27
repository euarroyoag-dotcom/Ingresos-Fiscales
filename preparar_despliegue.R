# =============================================================================
#  Preparar la app para GitHub + Posit Connect Cloud
#
#  Uso (en la consola de R):
#    setwd("C:/ruta/a/recaudacion-mexico")   # la carpeta donde está app.R
#    source("preparar_despliegue.R")
#
#  Vuelva a correrlo cada vez que agregue o quite un paquete en app.R.
# =============================================================================

paquetes <- c("shiny", "bslib", "dplyr", "tidyr", "readr", "stringr", "purrr",
              "forcats", "tibble", "ggplot2", "readxl", "plotly", "DT", "scales",
              "jsonlite", "rsconnect")

faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan)) install.packages(faltan, type = "binary")

stopifnot("No se encontró app.R: use setwd() con la carpeta de la app" = file.exists("app.R"))

# manifest.json le dice al servidor qué versión de R y de cada paquete instalar
rsconnect::writeManifest(
  appDir        = ".",
  appPrimaryDoc = "app.R",
  appFiles      = c("app.R", list.files("data", full.names = TRUE),
                    list.files("www", full.names = TRUE))   # incluye la imagen de portada
)

message("Listo: se creó manifest.json. Ahora haga commit y push a GitHub.")
