# =============================================================================
#  Recaudación y renuncias recaudatorias en México
#  Aplicación Shiny — Economía Pública
#
#  Datos (carpeta data/):
#    - analitico_ingresos_2010_2026.xlsx   Ley de Ingresos de la Federación
#    - total_recaudacion.csv               Recaudación local por entidad
#    - mapa_imp_predial.csv                Impuesto predial por municipio
#    - renuncias_recaudatorias_oxfam.xlsx  Renuncias recaudatorias (Oxfam México)
#    - OECD_and_LATAM_overall_taxation.csv Ingresos tributarios, % del PIB (OCDE y ALC)
#    - OECD_labor_tax_income.csv           Cuña fiscal laboral (OCDE)
#    - corporate_tax_OECD.csv              Tasas legales sobre dividendos, 2026 (OCDE)
#    - mexico_estados.json                 Cartografía estatal (se descarga sola
#                                          la primera vez; si no hay internet se
#                                          usa un mapa de cuadrícula)
#
#    - pib.csv, Ingresos_petroleros.csv    Banco Mundial (PIB y rentas del petróleo)
#    - social_security.csv                 Financiamiento de la seguridad social (OCDE)
#    - participaciones_aportaciones_federales.csv  Transferencias federales (INEGI, EFIPEM)
#
#  Paquetes:
#    install.packages(c("shiny", "bslib", "dplyr", "tidyr", "readr", "stringr", "purrr",
#                       "forcats", "tibble", "ggplot2", "readxl", "plotly", "DT",
#                       "scales", "jsonlite"), type = "binary")
#
#  Despliegue: ver README.md (GitHub + Posit Connect Cloud).
#
#  Guarde este archivo con codificación UTF-8 (R >= 4.2 recomendado).
# =============================================================================

options(encoding = "UTF-8")

library(shiny)
library(bslib)
# Solo los paquetes del tidyverse que se usan (más ligero para desplegar en el servidor)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(forcats)
library(tibble)
library(ggplot2)
library(readxl)
library(plotly)
library(DT)
library(scales)
# jsonlite se usa con jsonlite:: para no enmascarar shiny::validate()

`%||%` <- function(a, b) if (is.null(a)) b else a

# -----------------------------------------------------------------------------
# 0. Estilo: viridis + Times New Roman
# -----------------------------------------------------------------------------
fuente      <- "Times New Roman"
fuente_css  <- "'Times New Roman', Times, serif"
vir         <- function(n, ...) viridis_pal(begin = 0, end = 0.9, ...)(n)
vir_uno     <- "#31688e"   # tono viridis para gráficas de una sola serie
vir_dos     <- c("#440154", "#35b779")
gris_ref    <- "#7f7f7f"
tinta       <- "#111111"
tinta_sec   <- "#4a4a4a"

theme_eco <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = fuente) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#e3e3e3", linewidth = 0.3),
      axis.text          = element_text(color = tinta_sec),
      axis.title         = element_text(color = tinta_sec),
      legend.title       = element_blank(),
      legend.position    = "bottom"
    )
}

a_plotly <- function(p, leyenda = TRUE, tooltip = "text") {
  ggplotly(p, tooltip = tooltip) |>
    estilo_plotly(leyenda)
}

estilo_plotly <- function(p, leyenda = TRUE) {
  p |>
    layout(
      font = list(family = fuente_css, size = 14, color = tinta),
      hoverlabel = list(font = list(family = fuente_css)),
      showlegend = leyenda,
      legend = list(orientation = "h", x = 0, y = -0.18),
      margin = list(l = 10, r = 10, t = 10, b = 10)
    ) |>
    config(displaylogo = FALSE, locale = "es",
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

fmt_mdp   <- label_comma(accuracy = 0.1, big.mark = ",")

idioma_dt <- list(
  search = "Buscar:", lengthMenu = "Mostrar _MENU_ registros",
  info = "Registros _START_ a _END_ de _TOTAL_", infoEmpty = "Sin registros",
  infoFiltered = "(filtrado de _MAX_ registros)", zeroRecords = "No se encontraron resultados",
  paginate = list(first = "Primero", last = "Último", `next` = "Siguiente", previous = "Anterior")
)
fmt_pesos <- label_dollar(accuracy = 1, big.mark = ",")
fmt_pct   <- label_percent(accuracy = 0.1)

# -----------------------------------------------------------------------------
# 1. Lectura de datos (con detección de codificación para conservar acentos)
# -----------------------------------------------------------------------------
ruta    <- function(x) file.path("data", x)
na_vals <- c("", " ", "NA")

detectar_codificacion <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  if (validUTF8(rawToChar(bytes))) "UTF-8" else "windows-1252"
}

leer_csv <- function(archivo, tipos) {
  path <- ruta(archivo)
  read_csv(path, locale = locale(encoding = detectar_codificacion(path)),
           na = na_vals, col_types = tipos) |>
    rename_with(\(x) x |> str_trim() |> str_to_lower())
}

## 1.1 Recaudación local por entidad ------------------------------------------
recaudacion_local <- leer_csv(
  "total_recaudacion.csv",
  cols(ID_ENTIDAD_FEDERATIVA = col_character(), ENTIDAD_FEDERATIVA = col_character(),
       .default = col_double())
) |>
  rename(anio = ciclo, cve_ent = id_entidad_federativa, entidad = entidad_federativa)

etiquetas_local <- c(
  total_recaudacion_local = "Recaudación local total",
  monto_impuestos         = "Impuestos estatales (incluye ISN)",
  monto_derechos          = "Derechos",
  monto_predial           = "Impuesto predial",
  monto_agua              = "Derechos de agua"
)

local_largo <- recaudacion_local |>
  select(anio, cve_ent, entidad, poblacion, all_of(names(etiquetas_local))) |>
  pivot_longer(all_of(names(etiquetas_local)), names_to = "variable", values_to = "monto") |>
  mutate(rubro     = factor(unname(etiquetas_local[variable]), levels = unname(etiquetas_local)),
         percapita = monto / poblacion)

local_nacional <- local_largo |>
  group_by(anio, rubro) |>
  summarise(monto = sum(monto, na.rm = TRUE), poblacion = sum(poblacion, na.rm = TRUE),
            .groups = "drop") |>
  mutate(percapita = monto / poblacion)

entidades   <- sort(unique(recaudacion_local$entidad))
nombres_ent <- recaudacion_local |> distinct(cve_ent, entidad)   # nombres homologados por clave INEGI

## 1.3 Predial municipal --------------------------------------------------------
predial <- leer_csv(
  "mapa_imp_predial.csv",
  cols(ID_ENTIDAD_FEDERATIVA = col_character(), ENTIDAD_FEDERATIVA = col_character(),
       ID_MUNICIPIO = col_character(), MUNICIPIO = col_character(),
       .default = col_double())
) |>
  rename(anio = ciclo, cve_ent = id_entidad_federativa, entidad = entidad_federativa,
         cve_mun = id_municipio) |>
  select(-entidad) |>
  left_join(nombres_ent, by = "cve_ent") |>
  mutate(percapita = if_else(poblacion > 0, monto_predial / poblacion, NA_real_))

# La base original de predial perdió los acentos de origen (aparecen como "\uFFFD",
# p. ej. "Cos\uFFFDo"). Se restauran con un diccionario de 535 palabras verificado
# contra los nombres oficiales de los municipios (data/correcciones_acentos.csv).
dic_acentos <- read_csv(ruta("correcciones_acentos.csv"), locale = locale(encoding = "UTF-8"),
                        col_types = cols(.default = col_character()))
dic_acentos <- set_names(dic_acentos$palabra_corregida, dic_acentos$palabra_danada)

reparar_acentos <- function(x) {
  map_chr(x, \(nombre) {
    if (is.na(nombre) || !str_detect(nombre, "\uFFFD")) return(nombre)
    tokens <- str_split(nombre, " ")[[1]]
    tokens <- map_chr(tokens, \(t) {
      base <- str_remove_all(t, "[(),.]")
      if (base %in% names(dic_acentos)) str_replace(t, fixed(base), dic_acentos[[base]]) else t
    })
    str_c(tokens, collapse = " ")
  })
}

catalogo_mun <- predial |> distinct(municipio) |> mutate(municipio_ok = reparar_acentos(municipio))
predial <- predial |>
  left_join(catalogo_mun, by = "municipio") |>
  mutate(municipio = str_squish(municipio_ok),
         mun_key   = str_c(cve_ent, "-", cve_mun),               # llave única en todo el país
         mun_lab   = str_c(municipio, " (", entidad, ")")) |>     # nombre con entidad, para la vista nacional
  select(-municipio_ok)

## 1.4 Ley de Ingresos de la Federación ----------------------------------------
limpiar_concepto <- function(x) {
  x |>
    str_squish() |>
    str_remove("^([A-Z]\\.|[IVX]+\\.|\\d+\\.|[a-z]\\.|[ivx]+\\))\\s*") |>
    str_remove("[:.\\s]+$")
}

leer_lif <- function(path, hoja) {
  raw <- read_excel(path, sheet = hoja, col_names = FALSE, col_types = "text",
                    .name_repair = \(x) paste0("c", seq_along(x)))
  hdr <- as.character(unlist(raw[3, ]))
  ci  <- which(hdr == "Iniciativa")[1]
  ca  <- which(hdr == "Aprobado")[1]
  cuerpo <- raw[-(1:3), ]
  etiq   <- as.matrix(cuerpo[, seq_len(ci - 1)])

  nivel    <- apply(etiq, 1, \(x) { w <- which(!is.na(x)); if (length(w)) w[1] - 1L else NA_integer_ })
  concepto <- apply(etiq, 1, \(x) { x <- x[!is.na(x)]; if (length(x)) x[1] else NA_character_ })

  out <- tibble(
    anio              = as.integer(hoja),
    nivel             = as.integer(nivel),
    concepto_original = concepto,
    iniciativa        = suppressWarnings(as.numeric(cuerpo[[ci]])),
    aprobado          = suppressWarnings(as.numeric(cuerpo[[ca]]))
  ) |>
    filter(!is.na(concepto_original))

  fin <- which(str_detect(out$concepto_original, "^Fuentes"))[1]
  if (!is.na(fin)) out <- out[seq_len(fin - 1), ]

  out |> mutate(orden = row_number(), concepto = limpiar_concepto(concepto_original))
}

ruta_lif <- ruta("analitico_ingresos_2010_2026.xlsx")
lif <- map(excel_sheets(ruta_lif), \(h) leer_lif(ruta_lif, h)) |> list_rbind()

conceptos_lif <- tribble(
  ~clave,      ~etiqueta,                                 ~nivel_req, ~patron,
  "total",     "Ingresos presupuestarios totales",        0L,  "^TOTAL$",
  "impuestos", "Ingresos tributarios (total)",            1L,  "^Impuestos$",
  "isr",       "Impuesto sobre la Renta (ISR)",           NA,  "^Impuesto sobre la renta",
  "iva",       "Impuesto al Valor Agregado (IVA)",        NA,  "^Impuesto al valor agregado",
  "ieps",      "IEPS",                                    NA,  "^Impuesto especial sobre producci",
  "comext",    "Impuestos al comercio exterior",          NA,  "^Impuestos? (generales )?(al|de) comercio exterior",
  "segsoc",    "Cuotas y aportaciones de seguridad social", 1L, "seguridad social",
  "derechos",  "Derechos",                                1L,  "^Derechos",
  "aprov",     "Aprovechamientos",                        1L,  "^Aprovechamientos",
  "ventas",    "Ingresos de organismos y empresas",       1L,
               "^(Ingresos por ventas? de bienes|Ingresos de organismos y empresas)",
  "financ",    "Ingresos derivados de financiamiento",    NA,  "^Ingresos derivados de financiamiento"
)

lif_armonizado <- pmap(conceptos_lif, \(clave, etiqueta, nivel_req, patron) {
  lif |>
    filter(str_detect(concepto, regex(patron, ignore_case = TRUE)),
           is.na(nivel_req) | nivel == nivel_req) |>
    group_by(anio) |>
    slice_min(orden, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(anio, clave = clave, rubro = etiqueta, iniciativa, aprobado)
}) |>
  list_rbind() |>
  mutate(rubro = factor(rubro, levels = conceptos_lif$etiqueta))

anios_lif <- sort(unique(lif$anio))
impuestos_lif <- lif_armonizado |> filter(clave == "impuestos") |> select(anio, impuestos = aprobado)

## 1.5 Renuncias recaudatorias (Oxfam México con datos de la SHCP) -------------
ruta_ren <- ruta("renuncias_recaudatorias_oxfam.xlsx")

ren_herr <- read_excel(ruta_ren, sheet = "B-PorHerramienta") |>
  transmute(anio = as.integer(ciclo), clave, herramienta = herramienta_consulta,
            tratamiento = str_to_sentence(tratamiento), concepto,
            nominal = monto_nominal, real2026 = monto_a_precios_de_2026,
            pib = renuncia_recaudatoria_como_porcentaje_del_pib * 100)

ren_sector <- read_excel(ruta_ren, sheet = "B-PorSector") |>
  transmute(anio = as.integer(ciclo), sector, tratamiento, concepto,
            nominal = monto_nominal, real2026 = monto_a_precios_de_2026,
            pib = renuncia_recaudatoria_como_porcentaje_del_pib * 100)
## Índice de precios para expresar montos a precios constantes de 2010
# Sale de la base de Oxfam (monto a precios de 2026 / monto nominal), que es el mismo para
# todas las renuncias de un año. 2023 no tiene informe: se interpola con la media geométrica.
deflactor <- ren_herr |>
  filter(nominal > 0, real2026 > 0) |>
  group_by(anio) |>
  summarise(f = median(real2026 / nominal), .groups = "drop") |>
  complete(anio = full_seq(anio, 1)) |>
  mutate(f      = if_else(is.na(f), sqrt(lag(f) * lead(f)), f),
         a_2010 = f / f[anio == 2010]) |>   # factor que lleva pesos corrientes a pesos de 2010
  select(anio, a_2010)

niveles_decil <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")
ren_decil <- read_excel(ruta_ren, sheet = "B-Deciles") |>
  filter(unidad == "Distribución de la renuncia por decil (%)") |>
  mutate(anio = as.integer(ciclo), decil = factor(decil, levels = niveles_decil),
         serie = as.character(str_glue("Informe {anio} (obs. {ciclo_observacion})"))) |>
  group_by(herramienta_fiscal, tratamiento, concepto, anio) |>
  filter(sum(valor, na.rm = TRUE) > 0) |>
  ungroup()

herramientas_decil <- sort(unique(ren_decil$herramienta_fiscal))

unidades_ren <- c("Porcentaje del PIB" = "pib",
                  "Millones de pesos de 2026" = "real2026",
                  "Millones de pesos corrientes" = "nominal")

## 1.6 Comparación internacional: OCDE y América Latina y el Caribe (ALC) ------
## Tres archivos con tres definiciones distintas (ver notas en la interfaz):
##  - OECD_and_LATAM_overall_taxation.csv : ingresos tributarios totales, % del PIB, 2015-2024
##  - OECD_labor_tax_income.csv           : cuña fiscal laboral, % del costo laboral, 2016-2025 (solo OCDE)
##  - corporate_tax_OECD.csv              : tasas legales sobre dividendos, 2026 (un solo corte, solo OCDE)
leer_csv_crudo <- function(archivo, ...) {
  path <- ruta(archivo)
  read_csv(path, locale = locale(encoding = detectar_codificacion(path)),
           col_types = cols(.default = col_character()), na = c(na_vals, "-"), ...)
}

paises_ocde <- c(
  AUS = "Australia", AUT = "Austria", BEL = "Bélgica", CAN = "Canadá", CHL = "Chile",
  COL = "Colombia", CRI = "Costa Rica", CZE = "Chequia", DNK = "Dinamarca", EST = "Estonia",
  FIN = "Finlandia", FRA = "Francia", DEU = "Alemania", GRC = "Grecia", HUN = "Hungría",
  ISL = "Islandia", IRL = "Irlanda", ISR = "Israel", ITA = "Italia", JPN = "Japón",
  KOR = "Corea", LVA = "Letonia", LTU = "Lituania", LUX = "Luxemburgo", MEX = "México",
  NLD = "Países Bajos", NZL = "Nueva Zelanda", NOR = "Noruega", POL = "Polonia",
  PRT = "Portugal", SVK = "Eslovaquia", SVN = "Eslovenia", ESP = "España", SWE = "Suecia",
  CHE = "Suiza", TUR = "Türkiye", GBR = "Reino Unido", USA = "Estados Unidos"
)
paises_alc <- c(
  ARG = "Argentina", ATG = "Antigua y Barbuda", BHS = "Bahamas", BLZ = "Belice", BOL = "Bolivia",
  BRA = "Brasil", BRB = "Barbados", CHL = "Chile", COL = "Colombia", CRI = "Costa Rica",
  CUB = "Cuba", DOM = "República Dominicana", ECU = "Ecuador", GRD = "Granada", GTM = "Guatemala",
  GUY = "Guyana", HND = "Honduras", JAM = "Jamaica", LCA = "Santa Lucía", MEX = "México",
  NIC = "Nicaragua", PAN = "Panamá", PER = "Perú", PRY = "Paraguay", SLV = "El Salvador",
  SUR = "Surinam", TTO = "Trinidad y Tobago", URY = "Uruguay"
)
nombres_pais <- c(paises_ocde, paises_alc)
nombres_pais <- nombres_pais[!duplicated(names(nombres_pais))]

etiquetas_prom <- c(OECD_REP = "Promedio OCDE", A9 = "Promedio ALC")  # A9 = promedio de ALC en el archivo

niveles_grupo <- c("México", "Otros OCDE", "Otros ALC", "OCDE y ALC")
colores_grupo <- c("México" = "#440154", "Otros OCDE" = "#31688e",
                   "Otros ALC" = "#35b779", "OCDE y ALC" = "#fde725")
grupo_pais <- function(cod) {
  case_when(cod == "MEX" ~ "México",
            cod %in% c("CHL", "COL", "CRI") ~ "OCDE y ALC",
            cod %in% names(paises_ocde) ~ "Otros OCDE",
            TRUE ~ "Otros ALC")
}

nombre_serie <- function(cod) {
  unname(ifelse(cod %in% names(etiquetas_prom), etiquetas_prom[cod], nombres_pais[cod]))
}

## Ingresos tributarios totales (% del PIB)
rec_int <- leer_csv_crudo("OECD_and_LATAM_overall_taxation.csv") |>
  set_names(c("codigo", "nombre", "tipo", "anio", "valor", "aviso")) |>
  mutate(anio = as.integer(anio), valor = as.numeric(valor)) |>
  filter(codigo %in% c(names(etiquetas_prom), names(nombres_pais)), !is.na(valor))
rec_paises    <- rec_int |> filter(codigo %in% names(nombres_pais)) |>
  mutate(pais = unname(nombres_pais[codigo]))
promedios_int <- rec_int |> filter(codigo %in% names(etiquetas_prom)) |>
  mutate(serie = unname(etiquetas_prom[codigo]))

## Cuña fiscal laboral (% del costo laboral): solo países de la OCDE y su promedio
# Nota: la base reporta 0.0 para Colombia en todos los años; verificar con la fuente.
cuna <- leer_csv_crudo("OECD_labor_tax_income.csv", skip = 3) |>
  rename(codigo = Code) |> select(-Country) |>
  pivot_longer(-codigo, names_to = "anio", values_to = "valor") |>
  mutate(anio = as.integer(anio), valor = as.numeric(valor)) |>
  filter(!is.na(valor))

## Financiamiento de la seguridad social (% del PIB): solo países de la OCDE
# OCDE, Revenue Statistics, "Financing of social security benefits". Gobierno general (S13).
# El archivo no trae promedio OCDE: se calcula por componente como promedio simple de los países con dato.
comp_ss <- c(T_FSSB_2000     = "Contribuciones a la seguridad social pública",
             T_FSSB_CCON_PRI = "Contribuciones obligatorias a fondos privados (como las Afores)",
             T_FSSB_CON_OTH  = "Impuestos etiquetados para seguridad social",
             T_FSSB_VCON_GOV = "Contribuciones voluntarias al gobierno")
col_ss  <- set_names(c("#31688e", "#35b779", "#fde725", "#440154"), unname(comp_ss))
seg_soc <- leer_csv_crudo("social_security.csv",
                          name_repair = \(x) make.unique(if_else(x == "", "vacia", x))) |>
  filter(STANDARD_REVENUE %in% names(comp_ss)) |>
  transmute(codigo = REF_AREA, comp = STANDARD_REVENUE,
            anio = as.integer(TIME_PERIOD), valor = as.numeric(OBS_VALUE)) |>
  filter(!is.na(valor))
seg_soc_prom <- seg_soc |>
  group_by(comp, anio) |> summarise(valor = mean(valor), n = n(), .groups = "drop") |>
  mutate(codigo = "OECD_REP")
seg_soc_n <- range(seg_soc_prom$n)
seg_soc   <- bind_rows(seg_soc, select(seg_soc_prom, codigo, comp, anio, valor))

## Rentas del petróleo (% del PIB), Banco Mundial (NY.GDP.PETR.RT.ZS)
# Ojo: no son ingresos del gobierno. Es el valor de la producción de petróleo menos su costo.
# El archivo trae agregados del Banco Mundial ponderados por PIB; aquí se calculan promedios
# simples con los mismos países de la OCDE y de ALC que usa la gráfica A1.
petro_ini <- 1990L
petro <- leer_csv_crudo("Ingresos_petroleros.csv", skip = 4,
                        name_repair = \(x) make.unique(if_else(x == "", "vacia", x))) |>
  rename(codigo = `Country Code`) |>
  select(codigo, matches("^[0-9]{4}$")) |>
  pivot_longer(-codigo, names_to = "anio", values_to = "valor") |>
  mutate(anio = as.integer(anio), valor = as.numeric(valor)) |>
  filter(!is.na(valor), anio >= petro_ini, codigo %in% names(nombres_pais))
petro_prom <- bind_rows(
  petro |> filter(codigo %in% names(paises_ocde)) |> group_by(anio) |>
    summarise(valor = mean(valor), n = n(), .groups = "drop") |> mutate(codigo = "OECD_REP"),
  petro |> filter(codigo %in% names(paises_alc)) |> group_by(anio) |>
    summarise(valor = mean(valor), n = n(), .groups = "drop") |> mutate(codigo = "A9"))
petro_n   <- petro_prom |> group_by(codigo) |> summarise(min = min(n), max = max(n), .groups = "drop")
petro_fin <- max(petro$anio)
petro     <- bind_rows(petro, select(petro_prom, codigo, anio, valor))

## Tasas legales sobre dividendos, 2026 (un solo año)
trat_es <- c("Classical" = "clásico", "Full imputation" = "imputación total",
             "Partial imputation" = "imputación parcial",
             "Full / partial imputation" = "imputación total o parcial",
             "Modified classical" = "clásico modificado", "Partial inclusion" = "inclusión parcial",
             "No shareholder taxation" = "sin impuesto al accionista", "Other" = "otro",
             "Classical / modified classical" = "clásico o clásico modificado")
div_ocde <- leer_csv_crudo("corporate_tax_OECD.csv", skip = 3) |>
  rename(codigo = `Country Code`, tratamiento = `Dividend Treatment`) |>
  filter(str_detect(codigo, "^[A-Z]{3}$")) |>
  transmute(codigo, tratamiento,
            trat_es   = recode(tratamiento, !!!trat_es, .default = tratamiento),
            cit       = as.numeric(CIT_DP),
            pit_neto  = as.numeric(NPT),
            total     = as.numeric(CPITCIT)) |>
  mutate(pit_neto = if_else(is.na(pit_neto) & tratamiento == "No shareholder taxation", 0, pit_neto),
         adic     = pmax(total - cit, 0))

val_int <- function(base, cod, a) base$valor[base$codigo == cod & base$anio == a][1]

series_comparacion <- function(base, comp, paises) {
  cods <- c("MEX", comp, paises)
  base |>
    filter(codigo %in% cods, !is.na(valor)) |>
    mutate(serie = factor(nombre_serie(codigo), levels = unique(nombre_serie(cods)))) |>
    droplevels()
}

estilos_serie <- function(niveles) {
  base_col <- c("México" = "#440154", "Promedio OCDE" = "#31688e", "Promedio ALC" = "#35b779")
  extra    <- setdiff(niveles, names(base_col))
  col <- c(base_col[intersect(names(base_col), niveles)],
           if (length(extra)) set_names(viridis_pal(option = "plasma", begin = 0.5, end = 0.85)(length(extra)), extra))
  es_prom <- names(col) %in% c("Promedio OCDE", "Promedio ALC")
  list(col = col,
       lt  = set_names(if_else(es_prom, "dashed", "solid"), names(col)),
       lw  = set_names(case_when(names(col) == "México" ~ 1.7, es_prom ~ 1.0, TRUE ~ 0.8), names(col)))
}

graf_lineas <- function(d, ylab, cero = TRUE, cortes = seq(2015, 2025, 1)) {
  e <- estilos_serie(levels(d$serie))
  p <- ggplot(d, aes(anio, y, color = serie, linetype = serie, linewidth = serie,
                     group = serie, text = texto))
  if (!cero) p <- p + geom_hline(yintercept = 0, color = tinta_sec, linewidth = 0.3)
  p <- p +
    geom_line() +
    geom_point(size = 1.8) +
    scale_color_manual(values = e$col) +
    scale_linetype_manual(values = e$lt) +
    scale_linewidth_manual(values = e$lw) +
    scale_x_continuous(breaks = cortes) +
    scale_y_continuous(labels = label_number(accuracy = 1)) +
    labs(x = NULL, y = ylab) +
    theme_eco()
  if (cero) p <- p + expand_limits(y = 0)
  a_plotly(p)
}

## 1.7 Transferencias federales a las entidades (participaciones y Ramo 33) -----
## Fuente: INEGI, EFIPEM, integrada por Calcu.mx (data/participaciones_aportaciones_federales.csv).
## El archivo trae un bloque de comentarios antes del encabezado; se salta hasta la fila "serie,...".
leer_transferencias <- function() {
  path   <- ruta("participaciones_aportaciones_federales.csv")
  lineas <- read_lines(path, locale = locale(encoding = detectar_codificacion(path)))
  ini    <- which(str_starts(lineas, "serie,"))[1]
  read_csv(I(str_c(lineas[ini:length(lineas)], collapse = "\n")),
           col_types = cols(.default = col_character()), na = na_vals)
}

transf <- leer_transferencias() |>
  filter(serie %in% c("anual", "fondo_ramo33")) |>
  transmute(serie, cve_ent = sprintf("%02d", as.integer(cve_ent)), entidad,
            anio = as.integer(anio), concepto,
            monto = as.numeric(monto_pesos), hab = as.numeric(habitantes)) |>
  mutate(es_cdmx = str_detect(entidad, "^Ciudad de M"),
         etiq    = if_else(es_cdmx, str_c(entidad, "*"), entidad),
         pc      = monto / hab)       # la columna pesos_por_habitante del archivo viene vacía para aportaciones

anios_transf   <- sort(unique(transf$anio))
entidades_tr   <- transf |> distinct(entidad, etiq) |> arrange(entidad)
etq_transf     <- c("Total (participaciones + aportaciones)" = "total",
                    "Participaciones (Ramo 28)"              = "participaciones_ramo28",
                    "Aportaciones (Ramo 33)"                 = "aportaciones_ramo33")
fondos_r33     <- c(FONE = "FONE (educación básica)", FASSA = "FASSA (salud)",
                    `FAIS / FISM` = "FAIS municipal", FISE = "FAIS estatal",
                    FORTAMUN = "FORTAMUN", FAM = "FAM", FAETA = "FAETA", FASP = "FASP", FAFEF = "FAFEF")
col_part_apor  <- c("Participaciones (Ramo 28)" = "#31688e", "Aportaciones (Ramo 33)" = "#35b779")

## 1.8 Ingresos petroleros y no petroleros del sector público presupuestario (% del PIB) ----
## Montos: Ley de Ingresos aprobada (estimaciones, no recaudación observada).
## PIB: Banco Mundial, PIB a precios corrientes en moneda nacional (NY.GDP.MKTP.CN), en pesos.
pib_mx <- leer_csv_crudo("pib.csv", skip = 4,
                         name_repair = \(x) make.unique(if_else(x == "", "vacia", x))) |>
  filter(`Country Code` == "MEX") |>
  select(matches("^[0-9]{4}$")) |>
  pivot_longer(everything(), names_to = "anio", values_to = "pib") |>
  mutate(anio = as.integer(anio), pib = as.numeric(pib) / 1e6) |>   # a millones de pesos, como la LIF
  filter(!is.na(pib))

conceptos_petro_gf <- c("Derechos a los hidrocarburos",                    # 2010-2014
                        "Impuesto a los rendimientos petroleros",          # 2010-2014
                        "Transferencias del Fondo Mexicano del Petróleo para la Estabilización y el Desarrollo",  # desde 2015
                        "Impuesto por la actividad de exploración y extracción de hidrocarburos")                 # desde 2015
lif_petro <- lif |>
  mutate(grupo = case_when(concepto %in% conceptos_petro_gf ~ "gf",
                           concepto == "Petróleos Mexicanos" & nivel == 3 ~ "pemex")) |>
  filter(!is.na(grupo)) |>
  group_by(anio, grupo) |>
  summarise(monto = sum(aprobado, na.rm = TRUE), .groups = "drop")

presup <- lif_armonizado |>
  filter(clave %in% c("total", "financ")) |>
  select(anio, clave, aprobado) |>
  pivot_wider(names_from = clave, values_from = aprobado) |>
  transmute(anio, presup = total - coalesce(financ, 0))   # ingresos presupuestarios sin financiamiento

petro_fed_w <- lif_petro |>
  pivot_wider(names_from = grupo, values_from = monto, values_fill = 0) |>
  inner_join(presup, by = "anio") |>
  inner_join(pib_mx, by = "anio") |>
  transmute(anio,
            gf       = gf / pib * 100,
            pemex    = pemex / pib * 100,
            no_petro = (presup - gf * pib / 100 - pemex * pib / 100) / pib * 100,
            total    = presup / pib * 100)
petro_fed <- petro_fed_w |>
  select(-total) |>
  pivot_longer(-anio, names_to = "comp", values_to = "valor") |>
  left_join(select(petro_fed_w, anio, total), by = "anio")
# Petroleros y no petroleros como rubros de la gráfica 1 (para ambas versiones de la ley)
petro_lif <- lif |>
  filter(concepto %in% conceptos_petro_gf | (concepto == "Petróleos Mexicanos" & nivel == 3)) |>
  group_by(anio) |>
  summarise(iniciativa = sum(iniciativa, na.rm = TRUE), aprobado = sum(aprobado, na.rm = TRUE), .groups = "drop")
presup_v <- lif_armonizado |>
  filter(clave %in% c("total", "financ")) |>
  select(anio, clave, iniciativa, aprobado) |>
  pivot_longer(c(iniciativa, aprobado), names_to = "version", values_to = "v") |>
  pivot_wider(names_from = clave, values_from = v) |>
  transmute(anio, version, presup = total - coalesce(financ, 0)) |>
  pivot_wider(names_from = version, values_from = presup)
rubros_petro <- c("Ingresos petroleros", "Ingresos no petroleros")
lif_armonizado <- bind_rows(
  lif_armonizado |> mutate(rubro = as.character(rubro)),
  petro_lif |> mutate(clave = "petro", rubro = rubros_petro[1]),
  petro_lif |>
    inner_join(presup_v, by = "anio", suffix = c("_p", "_t")) |>
    transmute(anio, clave = "no_petro", rubro = rubros_petro[2],
              iniciativa = iniciativa_t - iniciativa_p, aprobado = aprobado_t - aprobado_p)
) |>
  mutate(rubro = factor(rubro, levels = c(conceptos_lif$etiqueta[1], rubros_petro, conceptos_lif$etiqueta[-1])))

comp_petro <- c(gf       = "Petroleros del Gobierno Federal (derechos y transferencias del FMP)",
                pemex    = "Petroleros: ingresos propios de Pemex",
                no_petro = "No petroleros")
col_petro  <- set_names(c("#440154", "#31688e", "#35b779"), unname(comp_petro))

# -----------------------------------------------------------------------------
# 2. Cartografía estatal
# -----------------------------------------------------------------------------
url_geo  <- "https://raw.githubusercontent.com/angelnmara/geojson/master/mexicoHigh.json"
ruta_geo <- ruta("mexico_estados.json")

geo_mx <- local({
  if (!file.exists(ruta_geo)) {
    try(download.file(url_geo, ruta_geo, mode = "wb", quiet = TRUE), silent = TRUE)
  }
  g <- if (file.exists(ruta_geo)) tryCatch(jsonlite::read_json(ruta_geo, simplifyVector = FALSE),
                                           error = \(e) NULL) else NULL
  if (is.null(g) || is.null(g$features)) { if (file.exists(ruta_geo)) unlink(ruta_geo); NULL } else g
})

normalizar <- function(x) {
  x |>
    iconv(to = "ASCII//TRANSLIT") |>
    str_to_lower() |>
    str_remove_all("[^a-z ]") |>
    str_remove(" de zaragoza| de ocampo| de ignacio de la llave") |>
    str_replace("^distrito federal$", "ciudad de mexico") |>
    str_replace("^estado de mexico$", "mexico") |>
    str_squish()
}

iso_ent <- tibble(
  cve_ent = sprintf("%02d", 1:32),
  iso = c("AGU", "BCN", "BCS", "CAM", "COA", "COL", "CHP", "CHH", "CMX", "DUR",
          "GUA", "GRO", "HID", "JAL", "MEX", "MIC", "MOR", "NAY", "NLE", "OAX",
          "PUE", "QUE", "ROO", "SLP", "SIN", "SON", "TAB", "TAM", "TLA", "VER",
          "YUC", "ZAC")
)

llaves_geo <- if (!is.null(geo_mx)) {
  feats <- tibble(
    id_geo  = map_chr(geo_mx$features, \(f) as.character(f$properties$id %||% f$id %||% NA)),
    nom_geo = map_chr(geo_mx$features, \(f) as.character(f$properties$name %||% NA))
  )
  ent <- recaudacion_local |> distinct(cve_ent, entidad) |>
    left_join(iso_ent, by = "cve_ent") |>
    mutate(iso = if_else(iso == "CMX" & !("MX-CMX" %in% feats$id_geo) & ("MX-DIF" %in% feats$id_geo),
                         "DIF", iso),
           id_iso = str_c("MX-", iso),
           nom = normalizar(entidad))
  ent |>
    left_join(feats |> mutate(nom = normalizar(nom_geo)) |> select(nom, id_nom = id_geo), by = "nom") |>
    mutate(id_geo = case_when(id_iso %in% feats$id_geo ~ id_iso, TRUE ~ id_nom)) |>
    select(cve_ent, id_geo)
} else NULL

# Respaldo sin internet: mapa de cuadrícula (cada estado es un cuadro)
cuadricula <- tribble(
  ~cve_ent, ~abr,    ~gx, ~gy,
  "02", "BC",   1, 1,  "26", "SON",  2, 1,  "08", "CHIH", 3, 1,  "05", "COAH", 4, 1,  "19", "NL",   5, 1,
  "03", "BCS",  1, 2,  "25", "SIN",  2, 2,  "10", "DGO",  3, 2,  "32", "ZAC",  4, 2,  "24", "SLP",  5, 2,
  "28", "TAMS", 6, 2,  "18", "NAY",  3, 3,  "01", "AGS",  4, 3,  "11", "GTO",  5, 3,  "22", "QRO",  6, 3,
  "13", "HGO",  7, 3,  "14", "JAL",  3, 4,  "16", "MICH", 4, 4,  "15", "MEX",  5, 4,  "09", "CDMX", 6, 4,
  "29", "TLAX", 7, 4,  "30", "VER",  8, 4,  "31", "YUC", 10, 4,  "06", "COL",  3, 5,  "12", "GRO",  5, 5,
  "17", "MOR",  6, 5,  "21", "PUE",  7, 5,  "27", "TAB",  9, 5,  "04", "CAMP", 10, 5, "23", "QROO", 11, 5,
  "20", "OAX",  7, 6,  "07", "CHIS", 8, 6
)

# Agrupa valores en k intervalos con aproximadamente el mismo número de entidades (cuantiles)
intervalos <- function(y, k = 6) {
  ok <- y[!is.na(y)]
  if (length(unique(ok)) < 2) return(factor(rep("Sin variación", length(y))))
  cortes <- quantile(ok, probs = seq(0, 1, length.out = k + 1), names = FALSE)
  medios <- signif(cortes[-c(1, length(cortes))], 2)           # cortes intermedios redondeados
  cortes <- sort(unique(c(floor(min(ok)), medios, ceiling(max(ok)))))
  etiquetas <- str_c(fmt_pesos(head(cortes, -1)), " a ", fmt_pesos(tail(cortes, -1)))
  cut(y, breaks = cortes, labels = etiquetas, include.lowest = TRUE)
}

# Mapa coroplético estatal (d debe tener cve_ent, y, texto).
# Si se da `clases` (factor del mismo largo que d), se colorea por intervalos en vez de una escala continua.
mapa_estatal <- function(d, titulo_barra, resaltar = character(0), clases = NULL) {
  d <- d |> mutate(sel = cve_ent %in% resaltar)
  if (!is.null(clases)) {
    clases <- droplevels(clases)
    niveles <- levels(clases)
    k <- length(niveles)
    col_k <- vir(k)
  }

  if (!is.null(geo_mx) && !is.null(llaves_geo)) {
    d <- d |> left_join(llaves_geo, by = "cve_ent")
    if (is.null(clases)) {
      z <- d$y; escala <- "Viridis"; zlim <- list()
      barra <- list(title = list(text = titulo_barra), thickness = 14, len = 0.8)
    } else {
      z <- as.integer(clases)
      escala <- unlist(lapply(seq_len(k), \(i) list(list((i - 1) / k, col_k[i]), list(i / k, col_k[i]))),
                       recursive = FALSE)
      zlim  <- list(zmin = 0.5, zmax = k + 0.5)
      barra <- list(title = list(text = titulo_barra), thickness = 14, len = 0.8,
                    tickvals = seq_len(k), ticktext = niveles)
    }
    do.call(add_trace, c(
      list(plot_ly(), type = "choropleth", geojson = geo_mx, featureidkey = "properties.id",
           locations = d$id_geo, z = z, text = d$texto, hoverinfo = "text",
           colorscale = escala,
           marker = list(line = list(color = if_else(d$sel, "#111111", "#ffffff"),
                                     width = if_else(d$sel, 3, 0.7))),
           colorbar = barra),
      zlim)) |>
      layout(geo = list(fitbounds = "locations", visible = FALSE,
                        projection = list(type = "mercator"))) |>
      estilo_plotly(leyenda = FALSE)
  } else {
    d <- d |> left_join(cuadricula, by = "cve_ent")
    if (!is.null(clases)) d$clase <- clases
    p <- ggplot(d, aes(gx, -gy, text = texto)) +
      (if (is.null(clases)) geom_tile(aes(fill = y, color = sel), linewidth = 1.2)
       else geom_tile(aes(fill = clase, color = sel), linewidth = 1.2)) +
      geom_text(aes(label = abr), color = "#ffffff", size = 3.4, family = fuente) +
      (if (is.null(clases)) scale_fill_viridis_c(name = NULL, labels = label_comma())
       else scale_fill_manual(values = set_names(col_k, niveles), name = NULL, drop = FALSE)) +
      scale_color_manual(values = c(`TRUE` = "#111111", `FALSE` = "#ffffff"), guide = "none") +
      coord_equal() +
      theme_void(base_family = fuente) +
      theme(legend.position = "right")
    a_plotly(p, leyenda = TRUE)
  }
}

texto_sin_mapa <- "Mapa de cuadrícula: no se pudo cargar la cartografía estatal; conecte a internet y reinicie la aplicación para ver el mapa geográfico."

# -----------------------------------------------------------------------------
# 4. Interfaz
# -----------------------------------------------------------------------------
tema <- bs_theme(
  version = 5, bg = "#ffffff", fg = tinta, primary = "#31688e", secondary = tinta_sec,
  base_font = font_collection("Times New Roman", "Times", "serif"),
  heading_font = font_collection("Times New Roman", "Times", "serif"),
  font_scale = 1.05
)

css <- HTML(str_glue("
  body, .navbar, .card, .form-control, .form-select, .btn, .selectize-input,
  .selectize-dropdown, .irs, .value-box-title, .value-box-value, table.dataTable,
  .dataTables_wrapper {{ font-family: {fuente_css} !important; }}
  .card-header {{ font-weight: 700; font-size: 1.02rem; }}
  .fuente-nota {{ font-size: .86rem; color: {tinta_sec}; font-style: italic; margin: 0; }}
  .comentario {{ border: 2px solid #b3261e; background: #fdecea; border-radius: 8px;
                 padding: 12px 16px; margin: 14px 0; }}
  .comentario h6 {{ color: #b3261e; font-weight: 700; margin-bottom: 8px; }}
  .comentario textarea {{ border: 1px solid #b3261e; background: #fff; }}
  .comentario small {{ color: #b3261e; }}
  .pregunta {{ font-size: 1.05rem; margin: 4px 0 14px; }}
  .definicion {{ color: #555; font-size: 0.92rem; margin: -4px 0 12px; }}
  .tabla-metodo td, .tabla-metodo th {{ vertical-align: top; font-size: 0.88rem; }}
  .tabla-metodo td:first-child {{ font-weight: 600; min-width: 150px; }}
  .aviso {{ border-left: 4px solid #31688e; background: #eef4f8; padding: 10px 14px;
            border-radius: 4px; margin-bottom: 12px; }}
  .referencia {{ padding-left: 2em; text-indent: -2em; margin-bottom: .6em; }}
  .portada {{ display: grid; grid-template-columns: minmax(0, 1.1fr) minmax(0, 1fr); gap: 32px;
              align-items: center; margin: 8px 0 24px; }}
  .portada-img img {{ width: 100%; max-height: 520px; object-fit: cover; border-radius: 6px; }}
  .portada-sin-img {{ min-height: 320px; border: 2px dashed #b7b7b7; border-radius: 6px; display: flex;
                      align-items: center; justify-content: center; text-align: center; padding: 24px; color: #6b6b6b; }}
  .portada-texto h1 {{ font-size: 2.6rem; font-weight: 700; margin-bottom: 4px; }}
  .portada-texto h3 {{ font-weight: 400; color: {tinta_sec}; margin-bottom: 18px; }}
  .portada-materia {{ text-transform: uppercase; letter-spacing: .12em; color: #31688e; font-weight: 700; margin-bottom: 6px; }}
  @media (max-width: 900px) {{ .portada {{ grid-template-columns: 1fr; }} }}
  .titulo-seccion {{ margin: 22px 0 10px; }}
  .conclusion .card-header {{ font-size: 1.05rem; display: flex; gap: 10px; align-items: baseline; }}
  .num-conc {{ color: #fff; border-radius: 50%; min-width: 26px; height: 26px; display: inline-flex;
               align-items: center; justify-content: center; font-size: .9rem; flex-shrink: 0; }}
  .conclusion p {{ text-align: justify; margin-bottom: 0; }}
  .bslib-sidebar-layout > .sidebar .accordion-button {{ font-weight: 700; font-size: .95rem; }}
"))

nota <- function(...) card_footer(tags$p(class = "fuente-nota", ...))

# Fuentes que se repiten en las notas al pie de las gráficas
fuentes <- list(
  lif       = "Fuente: elaboración propia con datos de la SHCP, Ley de Ingresos de la Federación 2010–2026.",
  lif_ini   = "Fuente: elaboración propia con datos de la SHCP, Iniciativa y Ley de Ingresos de la Federación 2010–2026.",
  local     = "Fuente: elaboración propia con datos de la SHCP sobre recaudación local por entidad federativa, 2004–2025.",
  predial   = "Fuente: elaboración propia con datos de la SHCP sobre recaudación de impuesto predial por municipio, 2010–2025.",
  mapa      = "Cartografía: angelnmara/geojson (GitHub).",
  renuncias = "Fuente: Oxfam México (2026) con datos de la SHCP, Presupuesto de Gastos Fiscales y Renuncias Recaudatorias.",
  efipem    = "Fuente: INEGI, EFIPEM, integrada por Calcu.mx."
)

# Imagen de portada: coloque un archivo portada.jpg / .png / .webp en la carpeta www/
imagen_portada <- function() {
  cand <- c("portada.jpg", "portada.jpeg", "portada.png", "portada.webp")
  hay  <- cand[file.exists(file.path("www", cand))]
  if (length(hay)) hay[1] else NULL
}

# ---- Conclusiones (texto fijo; cifras verificadas con las bases de la app) ----
conclusiones <- list(
  list(seccion = "I. México frente al mundo", color = "#440154", items = list(
    list(t = "México recauda muy poco.",
         x = "En 2024 los ingresos tributarios de México sumaron 18.3% del PIB, poco más de la mitad del promedio de la OCDE (34.1%) y menos que el promedio de América Latina y el Caribe (21.7%). En 2022, 2023 y 2024 fue el país de la OCDE que menos recaudó. Una brecha de casi 16 puntos del PIB limita la capacidad del Estado para financiar bienes públicos, seguridad social y redistribución.",
         g = "Gráficas A1 y A3"),
    list(t = "La tasa del impuesto corporativo es alta para México.",
         x = "La tasa legal del ISR empresarial es 30%, contra un promedio de 23.7% en los 36 países de la base de la OCDE: solo Francia (36.1%) y Alemania (30.1%) la superan, y Australia y Costa Rica la igualan. Como el impuesto personal sobre dividendos es bajo, la tasa combinada (42%) queda cerca del promedio (42.7%). Una tasa legal alta junto con una recaudación baja indica que el problema no es la tasa, sino una base erosionada por deducciones, diferimientos y regímenes especiales.",
         g = "Gráficas A5 y 4"),
    list(t = "La cuña fiscal sobre el costo laboral está por debajo de la OCDE.",
         x = "En 2025, de cada 100 pesos que cuesta contratar a un trabajador soltero con salario promedio, 21.7 se pagan en ISR y cuotas de seguridad social, contra 35.1 en el promedio de la OCDE. La cuña baja refleja cuotas reducidas y un ISR moderado en salarios medios; además, la informalidad deja a una parte grande del empleo fuera de ambos.",
         g = "Gráfica A4"),
    list(t = "La seguridad social casi no se financia con contribuciones, a diferencia del resto de la OCDE.",
         x = "Las contribuciones a la seguridad social pública equivalen a 2.5% del PIB en México, contra 9.3% en el promedio de la OCDE (2024). Otro 1.75% del PIB se va a cuentas individuales privadas (Afores), que la OCDE no cuenta como impuesto (promedio OCDE: 1.0%), y México no tiene impuestos etiquetados para la seguridad social. Con contribuciones tan bajas, buena parte del gasto en pensiones del régimen anterior y en programas como la Pensión para el Bienestar se cubre con impuestos generales.",
         g = "Gráfica A6"),
    list(t = "El petróleo ocupó durante décadas el lugar de los impuestos.",
         x = "Entre 1990 y 2014 las rentas petroleras de México promediaron 3.8% del PIB, casi siete veces el promedio de la OCDE (0.6%). Esa riqueza permitió financiar al gobierno sin construir una base tributaria amplia.",
         g = "Gráfica A7")
  )),
  list(seccion = "II. Ingresos del Gobierno Federal", color = "#31688e", items = list(
    list(t = "Los ingresos petroleros cayeron; los no petroleros los sustituyeron, pero sin elevar la recaudación total.",
         x = "Según la Ley de Ingresos aprobada, los ingresos petroleros bajaron de 7.5% del PIB en 2013 a 3.3% en 2025. Los no petroleros subieron de 13.8% a 19.6% del PIB y compensaron la caída, pero los ingresos presupuestarios totales apenas cambiaron: de 21.2% a 22.9% del PIB. México cambió de fuente de ingresos, pero no aumentó el tamaño de su recaudación.",
         g = "Gráficas 1 y 2"),
    list(t = "Buena parte del aumento tributario se financió con el IEPS.",
         x = "Los ingresos tributarios aprobados crecieron de 9.4% del PIB en 2010 a 15.0% en 2025. El IEPS fue el que más creció en proporción: pasó de 0.4% a 2.0% del PIB y su peso en los ingresos tributarios se triplicó (de 3.8% en 2010 a 13.0% en 2026). Explica cerca de 30% del aumento; el resto vino sobre todo del ISR, que pasó de 4.6% a 8.1% del PIB. El salto ocurrió a partir de 2015–2016, cuando el IEPS a gasolinas dejó de ser un subsidio y empezó a recaudar, y con los impuestos a bebidas azucaradas y alimentos con alta densidad calórica de 2014. Como impuesto al consumo, su carga tiende a ser regresiva, aunque en bienes con externalidades tiene una justificación pigouviana.",
         g = "Gráficas 1 y 3"),
    list(t = "Los demás impuestos perdieron protagonismo.",
         x = "Mientras el ISR y el IEPS ganaron peso, el IVA bajó de 37.1% a 27.2% de los ingresos tributarios entre 2010 y 2026, y apenas creció como porcentaje del PIB (de 3.5% a 4.2% en 2025). El resto de los impuestos (ISAN, accesorios y los ya derogados IETU e IDE) cayó de 8.1% a 2.8%. Los impuestos al comercio exterior repuntan en 2026 (4.4%).",
         g = "Gráfica 3"),
    list(t = "Las exenciones a medicinas, servicios médicos, vivienda y enseñanza son muy costosas y muy regresivas.",
         x = "En 2026 estas cuatro renuncias del IVA cuestan 116 mil millones de pesos (0.22% del PIB): medicinas 43.1 mil millones, enseñanza 38.5, vivienda 26.2 y servicios médicos 8.4. El 10% de los hogares con más ingresos recibe entre 23% (medicinas) y 35% (vivienda) del beneficio, y el 10% más pobre entre 2% y 3%; el 20% más rico se queda con 37% a 49%, frente a 6% a 8% del 20% más pobre (observación 2024). Son más regresivas que la tasa cero a alimentos (25% frente a 12.5%). En conjunto, las renuncias recaudatorias suman 3.1% del PIB en 2026, el equivalente a 28.6% de los ingresos tributarios aprobados.",
         g = "Gráficas 4 y 5")
  )),
  list(seccion = "III. Estados y municipios", color = "#35b779", items = list(
    list(t = "Existen grandes asimetrías en la recaudación local.",
         x = "En 2025 la Ciudad de México recaudó 11,116 pesos por habitante en impuestos, derechos, predial y agua; Oaxaca, 1,355: 8.2 veces menos. La capital concentra 18.9% de la recaudación local del país con 7.1% de la población, y 31% de todo el predial. Entre municipios la brecha es aún mayor: uno en el percentil 90 cobra 52 veces más predial por habitante que uno en el percentil 10. Las entidades más pobres son también las que menos recaudan, lo que refuerza su dependencia de las transferencias.",
         g = "Gráficas 6 a 9"),
    list(t = "Los municipios turísticos recaudan mucho predial; los estados agrícolas, muy poco.",
         x = "De 2,183 municipios con dato en 2025, Isla Mujeres (2.º lugar, 7,492 pesos por habitante) y Tulum (3.º, 4,437) están en la cima, y Solidaridad (16.º), Los Cabos (18.º), Santa María Huatulco (22.º), Manzanillo (23.º) y Bahía de Banderas (27.º) están en el 2% superior; el municipio mediano cobra 124 pesos. En el otro extremo, Oaxaca (130 pesos por habitante), Chiapas (138), Tlaxcala (174), Tabasco (179), Veracruz (181) y Guerrero (242) son las entidades que menos cobran, frente a 755 en el promedio nacional. El predial sigue al valor de la propiedad y a la capacidad administrativa: los destinos turísticos tienen inmuebles caros, catastros actualizados y propietarios que no residen ahí (lo que eleva el cociente por habitante), mientras que en las zonas rurales predominan la propiedad social, valores catastrales bajos y menor capacidad de cobro. Los municipios residenciales de altos ingresos, como San Pedro Garza García (1.º) y Huixquilucan, muestran que el ingreso de sus habitantes también importa.",
         g = "Gráficas 8 y 9"),
    list(t = "Las participaciones se dan por confianza: son de libre disposición.",
         x = "Las participaciones (Ramo 28) son la parte de la recaudación federal que se comparte con estados y municipios, y cada gobierno decide en qué gastarlas; las aportaciones (Ramo 33), en cambio, están etiquetadas para educación, salud, infraestructura social y seguridad. En 2025 las participaciones sumaron 1.35 billones de pesos (52% de las transferencias) y las aportaciones 1.25 billones. Se reparten con las fórmulas de la Ley de Coordinación Fiscal, que combinan población, crecimiento económico y recaudación propia, por lo que no están diseñadas para compensar las brechas de origen: la Ciudad de México recibe las participaciones más altas por habitante (14,467 pesos), seguida de Tabasco y Campeche.",
         g = "Gráfica 10"),
    list(t = "Quienes más reciben lo hacen primero por ser petroleros o aislados, y después por pobres e inseguros.",
         x = "En 2025 las entidades con más transferencias por habitante fueron Campeche (26,776 pesos), Baja California Sur (26,020), Tabasco (25,641) y Colima (25,621). Campeche y Tabasco reciben más participaciones por su producción de hidrocarburos; Baja California Sur y Colima tienen poca población y costos fijos de servicios que se reparten entre pocos habitantes, y encabezan el fondo de seguridad pública (FASP) por habitante. Siguen Guerrero (24,252), Zacatecas (23,153) y Oaxaca (22,761), entidades con alta pobreza y, en Guerrero y Zacatecas, altos niveles de violencia; Chiapas, Guerrero y Oaxaca encabezan el fondo de infraestructura social (FAIS), que se asigna por pobreza. En las demás entidades el monto se explica sobre todo por la población y por fondos y proyectos específicos: las más pobladas (Guanajuato, Estado de México y Jalisco) reciben menos de 17,300 pesos por habitante.",
         g = "Gráfica 10")
  ))
)

tarjeta_conclusion <- function(n, it, color) {
  card(class = "conclusion", style = str_glue("border-left: 6px solid {color};"),
       card_header(tags$span(class = "num-conc", style = str_glue("background:{color};"), n), it$t),
       tags$p(it$x),
       card_footer(tags$small(class = "fuente-nota", "Evidencia: ", it$g)))
}

panel_conclusiones <- function() {
  n <- 0
  bloques <- lapply(conclusiones, \(s) {
    tarjetas <- lapply(s$items, \(it) { n <<- n + 1; tarjeta_conclusion(n, it, s$color) })
    tagList(tags$h4(class = "titulo-seccion", s$seccion),
            do.call(layout_column_wrap, c(list(width = 1/2, fill = FALSE, heights_equal = "row"), tarjetas)))
  })
  tagList(
    tags$h3("Conclusiones"),
    tags$p(class = "pregunta", tags$b("¿Por qué México recauda relativamente poco y de dónde obtiene sus ingresos el gobierno?")),
    bloques,
    card(class = "conclusion", style = "border-left: 6px solid #111111;",
         card_header("Síntesis"),
         tags$p("México no recauda poco por tener tasas legales bajas: su tasa corporativa está entre las más altas de la OCDE. ",
                "Recauda poco porque sus bases son estrechas: una cuña laboral baja y un mercado de trabajo informal, contribuciones a la seguridad social reducidas, ",
                "renuncias recaudatorias que equivalen a casi 30% de lo que se cobra en impuestos y que benefician sobre todo a los hogares de mayores ingresos, ",
                "y gobiernos locales que cobran poco predial. Durante décadas el petróleo cubrió ese hueco; al caer, los impuestos lo sustituyeron —con un peso creciente del IEPS— ",
                "pero sin aumentar el tamaño total de la recaudación. Las transferencias federales compensan parte de las desigualdades locales, aunque responden más a fórmulas, ",
                "petróleo y población que a la capacidad de cada entidad para recaudar por su cuenta."))
  )
}

panel_portada <- function() {
  img <- imagen_portada()
  tagList(
    div(class = "portada",
        if (!is.null(img))
          div(class = "portada-img", tags$img(src = img, alt = "Imagen de portada"))
        else
          div(class = "portada-img portada-sin-img",
              tags$span("Coloque la imagen de portada en la carpeta ", tags$code("www/"),
                        " con el nombre ", tags$code("portada.jpg"), " (o .png).")),
        div(class = "portada-texto",
            tags$p(class = "portada-materia", "Economía Pública"),
            tags$h1("Finanzas públicas de México"),
            tags$h3("Recaudación y renuncias recaudatorias"),
            tags$p(class = "pregunta", tags$b("Pregunta del tablero: "),
                   "¿Por qué México recauda relativamente poco y de dónde obtiene sus ingresos el gobierno?"),
            tags$hr(),
            tags$p(tags$b("Integrantes:")),
            tags$ul(tags$li("Arroyo Aguilera Erick Ulises"), tags$li("Aranda Nassar Adiel Alejandro")),
            tags$p(tags$b("Profesor: "), "Dr. Raymundo M. Campos Vázquez"),
            tags$p(tags$b("Fecha: "), "Octubre de 2026"),
            if (!is.null(img)) tags$p(class = "fuente-nota", "Imagen: Secretaría de Hacienda y Crédito Público (SHCP).")
        )
    ),
    layout_column_wrap(
      width = 1/4, fill = FALSE,
      card(card_header("1. Comparación internacional"), tags$p("Cuánto recauda México frente a la OCDE y América Latina, y cómo grava el trabajo y el capital.")),
      card(card_header("2. Recaudación federal"), tags$p("De dónde vienen los ingresos del Gobierno Federal, el peso del petróleo y las renuncias recaudatorias.")),
      card(card_header("3. Estados y municipios"), tags$p("Recaudación local, impuesto predial y transferencias federales.")),
      card(card_header("Conclusiones"), tags$p("Doce hallazgos y una síntesis, con la evidencia de cada sección."))
    )
  )
}

# ---- Panel único de controles (izquierda) ----
tabs_con_controles <- c("int", "fed", "loc")

panel_controles <- sidebar(
  id = "controles", title = "Controles de las gráficas", width = 340,

  conditionalPanel(
    "input.nav === 'int'",
    accordion(
      multiple = TRUE, open = TRUE,
      accordion_panel(
        "Países y promedios (A1, A4, A7)",
        checkboxGroupInput("int_comp", "Comparar a México con",
                           choices = c("Promedio OCDE" = "OECD_REP",
                                       "Promedio América Latina y el Caribe (ALC)" = "A9"),
                           selected = c("OECD_REP", "A9")),
        selectizeInput("int_paises", "Países adicionales",
                       choices = list(
                         "OCDE" = set_names(names(paises_ocde), paises_ocde)[
                           !names(paises_ocde) %in% c("MEX", "CHL", "COL", "CRI")] |>
                           (\(x) x[order(names(x))])(),
                         "América Latina y el Caribe (Chile, Colombia y Costa Rica también son de la OCDE)" =
                           set_names(names(paises_alc), paises_alc)[names(paises_alc) != "MEX"] |>
                           (\(x) x[order(names(x))])()),
                       selected = c("CHL", "BRA"), multiple = TRUE,
                       options = list(maxItems = 5, placeholder = "Hasta cinco países",
                                      plugins = list("remove_button")))
      ),
      accordion_panel(
        "Ranking y seguridad social (A3, A6)",
        sliderInput("int_anio", "Año", min = 2015, max = 2024, value = 2024, step = 1, sep = ""),
        selectInput("int_universo", "Países incluidos en el ranking (A3)",
                    c("OCDE y América Latina y el Caribe" = "ambos",
                      "Solo OCDE" = "ocde", "Solo América Latina y el Caribe" = "alc"))
      ),
      accordion_panel(
        "Tasas sobre dividendos (A5)",
        radioButtons("int_div_medida", "Medida",
                     c("Tasa combinada: corporativo + personal" = "total",
                       "Solo impuesto corporativo" = "cit",
                       "Solo impuesto personal neto sobre el dividendo" = "pit"))
      )
    )
  ),

  conditionalPanel(
    "input.nav === 'fed'",
    accordion(
      multiple = TRUE, open = TRUE,
      accordion_panel(
        "Ingresos federales (gráfica 1)",
        selectizeInput("lif_rubros", "Conceptos de ingreso",
                       choices = levels(lif_armonizado$rubro),
                       selected = c("Ingresos tributarios (total)", "Impuesto sobre la Renta (ISR)",
                                    "Impuesto al Valor Agregado (IVA)", "IEPS", "Ingresos petroleros"),
                       multiple = TRUE, options = list(maxItems = 8, plugins = list("remove_button"))),
        radioButtons("lif_version", "Versión de la Ley de Ingresos",
                     c("Aprobada por el Congreso" = "aprobado", "Iniciativa del Ejecutivo" = "iniciativa")),
        radioButtons("lif_unidad", "Unidad de medida",
                     c("Millones de pesos de 2010", "Millones de pesos corrientes",
                       "Porcentaje de los ingresos totales", "Índice (2010 = 100)"),
                     selected = "Millones de pesos de 2010")
      ),
      accordion_panel(
        "Renuncias recaudatorias (gráfica 4 y recuadros)",
        radioButtons("ren_unidad", "Unidad de medida (gráfica 4)", unidades_ren),
        selectInput("ren_anio", "Año del informe (recuadros)", choices = rev(sort(unique(ren_herr$anio))))
      ),
      accordion_panel(
        "Distribución por decil (gráfica 5)",
        selectInput("dec_herr", "1. Impuesto",
                    choices = herramientas_decil, selected = "Impuesto al valor agregado"),
        selectizeInput("dec_concepto", "2. Renuncia recaudatoria", choices = NULL,
                       options = list(placeholder = "Escriba o elija una renuncia")),
        selectizeInput("dec_anio", "3. Año(s) de observación", choices = NULL, multiple = TRUE,
                       options = list(maxItems = 4, plugins = list("remove_button"),
                                      placeholder = "Elija hasta cuatro años")),
        radioButtons("dec_vista", "4. Tipo de gráfica",
                     c("Barras: proporción por decil" = "barras",
                       "Curva de concentración (acumulada)" = "curva")),
        tags$small(class = "text-muted",
                   "Las renuncias están agrupadas por tratamiento (tasa cero, exención, deducción…). ",
                   "Elija varios años para ver cómo cambió la distribución.")
      )
    )
  ),

  conditionalPanel(
    "input.nav === 'loc'",
    accordion(
      multiple = TRUE, open = TRUE,
      accordion_panel(
        "Recaudación estatal (gráficas 6 y 7)",
        selectInput("est_rubro", "Rubro", choices = unname(etiquetas_local),
                    selected = "Impuestos estatales (incluye ISN)"),
        radioButtons("est_medida", "Medida",
                     c("Pesos por habitante" = "percapita", "Millones de pesos corrientes" = "monto")),
        sliderInput("est_anio", "Año", min = min(recaudacion_local$anio),
                    max = max(recaudacion_local$anio), value = max(recaudacion_local$anio),
                    step = 1, sep = "")
      ),
      accordion_panel(
        "Impuesto predial (gráficas 8 y 9 y recuadros)",
        selectInput("pre_ent", "Ámbito", choices = c("Nacional (todas las entidades)" = "Nacional", entidades),
                    selected = "Nacional"),
        sliderInput("pre_anio", "Año", min = min(predial$anio), max = max(predial$anio),
                    value = max(predial$anio), step = 1, sep = ""),
        radioButtons("pre_orden", "Municipios a mostrar (gráfica 8)",
                     c("Mayor recaudación por habitante" = "alto",
                       "Menor recaudación por habitante" = "bajo")),
        sliderInput("pre_n", "Número de municipios (gráfica 8)", min = 5, max = 30, value = 15),
        sliderInput("pre_clases", "Número de intervalos del mapa (gráfica 9)", min = 4, max = 8, value = 6, step = 1)
      ),
      accordion_panel(
        "Transferencias federales (gráfica 10)",
        radioButtons("tr_concepto", "Concepto que ordena la gráfica", choices = etq_transf, selected = "total"),
        sliderInput("tr_anio", "Año", min = min(anios_transf), max = max(anios_transf),
                    value = max(anios_transf), step = 1, sep = "")
      )
    )
  ),

  conditionalPanel(
    "['int', 'fed', 'loc'].indexOf(input.nav) < 0",
    tags$p(class = "text-muted", "Esta sección no tiene controles. Elija una de las secciones 1 a 3 para modificar las gráficas.")
  )
)

ui <- function(req) {
  page_navbar(
    id = "nav",
    title = "Finanzas públicas de México: recaudación y renuncias recaudatorias",
    theme = tema,
    fillable = FALSE,
    sidebar = panel_controles,
    header = tags$head(tags$meta(charset = "UTF-8"), tags$style(css)),

    # ---- Portada ---------------------------------------------------------------
    nav_panel("Portada", value = "portada", panel_portada()),

    # ---- 1. Comparación internacional ----------------------------------------
    nav_panel(
      "1. Comparación internacional", value = "int",
      tags$h4("¿Cuánto recauda México frente a la OCDE y a América Latina?"),
      card(full_screen = TRUE,
           card_header("Gráfica A1. Ingresos tributarios totales como porcentaje del PIB, 2015–2024"),
           plotlyOutput("g_int_serie", height = "420px"),
           nota("Fuente: OCDE, Global Revenue Statistics Database. Gobierno general (federal, estatal, local y seguridad social). ",
                "Solo ingresos que la OCDE clasifica como impuestos. Los promedios (OCDE y ALC) son los publicados en la base; su cobertura de países puede variar entre años.")),
      card(full_screen = TRUE,
           card_header(textOutput("t_int_rank", inline = TRUE)),
           plotlyOutput("g_int_rank", height = "820px"),
           nota(textOutput("t_int_rank_nota", inline = TRUE))),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE,
             card_header("Gráfica A4. Cuña fiscal laboral, 2016–2025 (porcentaje del costo laboral)"),
             plotlyOutput("g_int_cuna", height = "430px"),
             nota("Fuente: OCDE, Taxing Wages. Persona soltera, sin hijos, con 100% del salario promedio. ",
                  "Suma el ISR de la persona y las cuotas de seguridad social del trabajador y del patrón, como proporción del costo laboral total. ",
                  textOutput("t_int_cuna_nota", inline = TRUE))),
        card(full_screen = TRUE,
             card_header("Gráfica A5. Tasas legales sobre las utilidades distribuidas como dividendos, 2026"),
             plotlyOutput("g_int_div", height = "640px"),
             nota("Fuente: OCDE, Tax Database (tasas legales sobre ingresos por dividendos, 2026). Tasas legales, no efectivas: no miden cuánto se recauda. ",
                  "Con «Promedio OCDE» seleccionado, la línea punteada es el promedio simple de los 36 países de la base (México incluido). ",
                  "De ALC solo aparecen Chile, Costa Rica y México (Colombia no está en la base), por lo que no se calcula promedio ALC."))
      ),
      card(full_screen = TRUE,
           card_header(textOutput("t_int_ss", inline = TRUE)),
           plotlyOutput("g_int_ss", height = "380px"),
           nota("Fuente: OCDE, Revenue Statistics: Financing of social security benefits in OECD member countries. Gobierno general. ",
                "La OCDE solo cuenta como impuesto la primera barra (código 2000); las contribuciones obligatorias a fondos privados no son impuestos. ",
                str_glue("El promedio OCDE lo calculamos por componente como promedio simple de los países con dato ({seg_soc_n[1]} a {seg_soc_n[2]} países)."))),
      card(full_screen = TRUE,
           card_header(str_glue("Gráfica A7. ¿Qué tanto pesa el petróleo en la economía? Rentas del petróleo, {petro_ini}–{petro_fin} (porcentaje del PIB)")),
           plotlyOutput("g_int_petro", height = "430px"),
           nota("Fuente: Banco Mundial, Indicadores del desarrollo mundial (NY.GDP.PETR.RT.ZS). ",
                "Las rentas del petróleo son el valor de la producción de crudo a precios regionales menos su costo de producción: ",
                "miden la riqueza petrolera de la economía, no lo que cobra el gobierno. ",
                str_glue("Promedios simples calculados por nosotros: OCDE con {petro_n$min[petro_n$codigo == 'OECD_REP']} a {petro_n$max[petro_n$codigo == 'OECD_REP']} países ",
                         "y ALC con {petro_n$min[petro_n$codigo == 'A9']} a {petro_n$max[petro_n$codigo == 'A9']} países, según el año. "),
                "Los promedios del Banco Mundial vienen ponderados por PIB y no son comparables con estos."))
    ),

    # ---- 2. Recaudación federal (incluye renuncias recaudatorias) -------------
    nav_panel(
      "2. Recaudación federal", value = "fed",
      tags$h4("¿De dónde obtiene sus ingresos el Gobierno Federal?"),
      card(full_screen = TRUE,
           card_header("Gráfica 1. Evolución de los ingresos presupuestarios del Gobierno Federal, 2010–2026"),
           plotlyOutput("g_lif_series", height = "430px"),
           nota("Fuente: elaboración propia con datos de la SHCP, Ley de Ingresos de la Federación 2010–2026. ",
                "Los conceptos se homologaron entre años porque el clasificador por rubro de ingreso cambió en 2014. ",
                "Petroleros: derechos e impuestos petroleros del Gobierno Federal (desde 2015, transferencias del Fondo Mexicano del Petróleo) más ingresos propios de Pemex. ",
                "No petroleros: ingresos presupuestarios sin financiamiento menos los petroleros.")),
      card(full_screen = TRUE,
           card_header(str_glue("Gráfica 2. ¿Cuánto depende el sector público del petróleo? Ingresos presupuestarios como porcentaje del PIB, {min(petro_fed$anio)}–{max(petro_fed$anio)}")),
           plotlyOutput("g_lif_petro", height = "430px"),
           nota("Fuente: elaboración propia con datos de la SHCP (Ley de Ingresos de la Federación aprobada) y del Banco Mundial (PIB a precios corrientes, NY.GDP.MKTP.CN). ",
                "Son estimaciones aprobadas antes de cada año, no recaudación observada; por eso 2020 sale alto: la ley se aprobó antes de la caída del PIB. ",
                "Petroleros del Gobierno Federal: derechos a los hidrocarburos e impuesto a los rendimientos petroleros hasta 2014; desde 2015, transferencias del Fondo Mexicano del Petróleo ",
                "e impuesto por exploración y extracción. El IEPS a combustibles se cuenta como no petrolero. Sin financiamiento.")),
      card(full_screen = TRUE,
           card_header("Gráfica 3. Estructura de los ingresos tributarios federales (ley aprobada)"),
           plotlyOutput("g_lif_comp", height = "400px"),
           nota(fuentes$lif, " Proporción de cada impuesto en los ingresos tributarios totales. ",
                "«Otros impuestos» incluye ISAN, IETU, IDE, accesorios y ajustes; puede ser negativo.")),

      tags$hr(),
      tags$h4("¿Cuánto deja de cobrar el Gobierno Federal? Renuncias recaudatorias"),
      tags$p(class = "definicion", "Una renuncia recaudatoria es el ingreso que el Estado deja de cobrar por exenciones, tasas reducidas o tasa cero, deducciones y estímulos fiscales."),
      layout_column_wrap(
        width = 1/3, fill = FALSE,
        value_box(textOutput("vb_ren_t1", inline = TRUE), textOutput("vb_ren_pib"),
                  theme = value_box_theme(bg = "#440154", fg = "#ffffff")),
        value_box(textOutput("vb_ren_t2", inline = TRUE), textOutput("vb_ren_mdp"),
                  theme = value_box_theme(bg = "#31688e", fg = "#ffffff")),
        value_box("Equivalente a los ingresos tributarios de la LIF", textOutput("vb_ren_lif"),
                  theme = value_box_theme(bg = "#35b779", fg = "#ffffff"))
      ),
      card(full_screen = TRUE,
           card_header("Gráfica 4. Renuncias recaudatorias por herramienta fiscal, 2002–2026"),
           plotlyOutput("g_ren_evol", height = "460px"),
           nota("Fuente: Oxfam México (2026) con datos de los informes de Gastos Fiscales y de Renuncias Recaudatorias de la SHCP. ",
                "No hay informe para 2023. Para 2002–2009 varias partidas solo se reportan como porcentaje del PIB.")),
      card(full_screen = TRUE,
           card_header(textOutput("t_dec", inline = TRUE)),
           plotlyOutput("g_dec", height = "470px"),
           uiOutput("dec_resumen"),
           nota(fuentes$renuncias, " ", textOutput("t_dec_nota", inline = TRUE)))
    ),

    # ---- 3. Recaudación estatal y municipal (incluye predial y transferencias) -
    nav_panel(
      "3. Recaudación estatal y municipal", value = "loc",
      tags$h4("¿Cuánto recaudan los gobiernos estatales y municipales?"),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE,
             card_header(textOutput("t_est_mapa", inline = TRUE)),
             plotlyOutput("g_est_mapa", height = "520px"),
             nota(textOutput("t_est_mapa_nota", inline = TRUE))),
        card(full_screen = TRUE,
             card_header(textOutput("t_est_rank", inline = TRUE)),
             plotlyOutput("g_est_rank", height = "520px"),
             nota(fuentes$local, " La línea punteada indica el valor nacional (recaudación total entre población total)."))
      ),

      tags$hr(),
      tags$h4("¿Cuánto cobran los municipios? Impuesto predial"),
      layout_column_wrap(
        width = 1/4, fill = FALSE,
        value_box("Predial recaudado", textOutput("vb_total"), theme = value_box_theme(bg = "#31688e", fg = "#ffffff")),
        value_box("Predial por habitante", textOutput("vb_pc"), theme = value_box_theme(bg = "#26828e", fg = "#ffffff")),
        value_box("Municipios con recaudación reportada", textOutput("vb_munis"), theme = value_box_theme(bg = "#1f9e89", fg = "#ffffff")),
        value_box("Cuentas pagadas", textOutput("vb_cuentas"), theme = value_box_theme(bg = "#440154", fg = "#ffffff"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(full_screen = TRUE,
             card_header(textOutput("t_pre_top", inline = TRUE)),
             plotlyOutput("g_pre_top", height = "540px"),
             nota(fuentes$predial, " Recaudación de impuesto predial dividida entre la población municipal.")),
        card(full_screen = TRUE,
             card_header(textOutput("t_pre_mapa", inline = TRUE)),
             plotlyOutput("g_pre_mapa", height = "540px"),
             nota(textOutput("t_pre_mapa_nota", inline = TRUE), " ", fuentes$predial, " ", fuentes$mapa))
      ),
      tags$hr(),
      tags$h4("¿Cuánto reciben de la Federación? Transferencias federales"),
      tags$p(class = "definicion", "Participaciones (Ramo 28): recursos de libre uso, una parte de lo que recauda la Federación. ",
             "Aportaciones (Ramo 33): recursos etiquetados, sobre todo para educación y salud."),
      card(full_screen = TRUE,
           card_header(textOutput("t_tr_rank", inline = TRUE)),
           plotlyOutput("g_tr_rank", height = "820px"),
           nota(textOutput("t_tr_rank_nota", inline = TRUE)))
    ),

    # ---- 4. Conclusiones -------------------------------------------------------
    nav_panel("4. Conclusiones", value = "conc", panel_conclusiones()),


    # ---- Metodología y referencias -------------------------------------------
    nav_panel(
      "Metodología y referencias", value = "metodo",
      card(
        card_header("Datos y metodología"),
        tags$p("Resumen de las fuentes, qué mide cada variable y las decisiones que tomamos al construir el tablero."),
        div(style = "overflow-x: auto;",
          tags$table(class = "table table-sm tabla-metodo",
            tags$thead(tags$tr(tags$th("Sección"), tags$th("Fuente y variable"), tags$th("Qué hicimos"), tags$th("Limitaciones"))),
            tags$tbody(
              tags$tr(
                tags$td("1. Comparación internacional: recaudación (A1, A3)"),
                tags$td("OCDE, Global Revenue Statistics. Ingresos tributarios del gobierno general (federal, estatal, local y seguridad social) como % del PIB, 2015–2024."),
                tags$td("Usamos los promedios OCDE y ALC que publica la base y los países de la OCDE y de América Latina y el Caribe."),
                tags$td("Solo cuenta lo que la OCDE clasifica como impuesto. Algunos países no tienen dato en los años más recientes.")),
              tags$tr(
                tags$td("1. Trabajo y capital (A4, A5, A6)"),
                tags$td("OCDE: Taxing Wages (cuña fiscal, % del costo laboral); Tax Database (tasas legales sobre dividendos, 2026); Revenue Statistics, financiamiento de la seguridad social (% del PIB)."),
                tags$td("Calculamos el promedio OCDE de la seguridad social por componente, como promedio simple de los países con dato. Un guion en los archivos se tomó como dato faltante, no como cero."),
                tags$td("Las tasas legales no son tasas efectivas. La cuña es para un trabajador soltero con salario promedio. La etiqueta «como las Afores» es interpretación nuestra.")),
              tags$tr(
                tags$td("1. Petróleo en la economía (A7)"),
                tags$td("Banco Mundial, Indicadores del desarrollo mundial: rentas del petróleo (% del PIB), 1990–2021."),
                tags$td("Calculamos promedios simples con los 38 países de la OCDE y los 28 de ALC. No usamos los agregados del Banco Mundial porque están ponderados por PIB."),
                tags$td("Son rentas en la economía (valor de la producción menos su costo), no ingresos del gobierno.")),
              tags$tr(
                tags$td("2. Ingresos federales (gráficas 1 a 3)"),
                tags$td("SHCP, Iniciativa y Ley de Ingresos de la Federación 2010–2026. PIB de México a precios corrientes del Banco Mundial (NY.GDP.MKTP.CN)."),
                tags$td("Homologamos los conceptos entre años porque el clasificador cambió en 2014. Petroleros: derechos e impuestos petroleros (desde 2015, transferencias del Fondo Mexicano del Petróleo) más ingresos propios de Pemex. No petroleros: ingresos presupuestarios sin financiamiento menos petroleros. Dividimos entre el PIB para obtener % del PIB."),
                tags$td("Son estimaciones aprobadas, no recaudación observada (por eso 2020 sale alto). El IEPS a combustibles cuenta como no petrolero. Los derechos bajan bruscamente en 2015 por la reclasificación de los ingresos petroleros.")),
              tags$tr(
                tags$td("2. Renuncias recaudatorias (gráficas 4 y 5)"),
                tags$td("Oxfam México (2026), con datos de los informes de Gastos Fiscales y Renuncias Recaudatorias de la SHCP."),
                tags$td("Mostramos montos en % del PIB o en pesos de 2026, y la distribución por decil de ingreso para uno o varios años de observación, en barras o como curva de concentración acumulada."),
                tags$td("No hay informe para 2023. De 2002 a 2009 varias partidas solo vienen en % del PIB. Las cifras del informe más reciente pueden ser estimaciones.")),
              tags$tr(
                tags$td("3. Recaudación estatal y predial (gráficas 6 a 9)"),
                tags$td("Recaudación local por entidad y predial por municipio (archivos total_recaudacion.csv y mapa_imp_predial.csv)."),
                tags$td("Por habitante = recaudación entre población. El valor nacional es la suma de la recaudación entre la suma de la población. En el mapa del predial (gráfica 9) las entidades se agrupan en intervalos por cuantiles. Restauramos los acentos de los nombres de municipios con un diccionario de 535 palabras."),
                tags$td("Pesos corrientes. El predial solo incluye municipios que reportan. Los impuestos estatales vienen agregados, sin separar el Impuesto sobre Nóminas.")),
              tags$tr(
                tags$td("3. Transferencias federales (gráfica 10)"),
                tags$td("INEGI, Estadística de Finanzas Públicas Estatales y Municipales (EFIPEM), integrada por Calcu.mx. Participaciones (Ramo 28) y aportaciones (Ramo 33), 2019–2025."),
                tags$td("Calculamos los pesos por habitante (monto entre población), porque la columna del archivo venía vacía para las aportaciones."),
                tags$td("La Ciudad de México no tiene FONE ni FAETA y no es comparable. El total nacional es una suma de entidades: en 2025 queda 0.3% abajo de lo publicado por la SHCP en participaciones y 5.2% en aportaciones. La fuente califica 2025 con confianza media."))
            )
          )
        ),
        tags$p(class = "fuente-nota",
               "Unidades: las cifras monetarias de las secciones 2 y 3 están en pesos corrientes (sin deflactar), salvo las renuncias recaudatorias, que están en pesos de 2026. ",
               "Mapas: si no hay internet la primera vez que se abre la app, los mapas se muestran como cuadrícula.")
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("Elaborado por"),
          tags$p(tags$b("Integrantes: "), "Arroyo Aguilera Erick Ulises y Aranda Nassar Adiel Alejandro."),
          tags$p(tags$b("Profesor: "), "Dr. Raymundo M. Campos Vázquez."),
          tags$p(tags$b("Materia: "), "Economía Pública.")
        ),
        card(
          card_header("Referencias de las bases de datos"),
          tags$p(class = "referencia",
                 "Banco Mundial. (2026). ",
                 tags$i("Indicadores del desarrollo mundial: PIB (UMN a precios actuales)"),
                 " (NY.GDP.MKTP.CN) [Conjunto de datos]."),
          tags$p(class = "referencia",
                 "Banco Mundial. (2026). ",
                 tags$i("Indicadores del desarrollo mundial: Rentas del petróleo (% del PIB)"),
                 " (NY.GDP.PETR.RT.ZS) [Conjunto de datos]."),
          tags$p(class = "referencia",
                 "Benítez, V., Del Río, E., Guzmán, J. y Mendoza, I. (2026). ",
                 tags$i("¿Incentivos para quién? Renuncias recaudatorias que subsidian a los más ricos"),
                 " [Base de datos]. Oxfam México. Elaborada con datos de la SHCP, ",
                 tags$i("Presupuesto de Gastos Fiscales"), " (2002–2020) y ",
                 tags$i("Renuncias Recaudatorias"), " (2021 en adelante)."),
          tags$p(class = "referencia",
                 "Cámara de Diputados del H. Congreso de la Unión. (s. f.). ",
                 tags$i("Ley de Coordinación Fiscal"), ". Diario Oficial de la Federación."),
          tags$p(class = "referencia",
                 "Instituto Nacional de Estadística y Geografía [INEGI]. (s. f.). ",
                 tags$i("Estadística de Finanzas Públicas Estatales y Municipales (EFIPEM)"),
                 " [Conjunto de datos]. Integrado por Calcu.mx. ",
                 tags$a(href = "https://calcu.mx/participaciones-federales", target = "_blank",
                        "https://calcu.mx/participaciones-federales")),
          tags$p(class = "referencia",
                 "Organisation for Economic Co-operation and Development [OECD]. (2025). ",
                 tags$i("Financing of social security benefits in OECD member countries"),
                 " [Conjunto de datos]. Revenue Statistics."),
          tags$p(class = "referencia",
                 "Organisation for Economic Co-operation and Development [OECD]. (2026). ",
                 tags$i("Global Revenue Statistics Database"), " [Conjunto de datos]."),
          tags$p(class = "referencia",
                 "Organisation for Economic Co-operation and Development [OECD]. (2026). ",
                 tags$i("OECD Tax Database: overall statutory tax rates on dividend income"),
                 " [Conjunto de datos]."),
          tags$p(class = "referencia",
                 "Organisation for Economic Co-operation and Development [OECD]. (2026). ",
                 tags$i("Taxing Wages: tax wedge, single person without children at 100% of the average wage"),
                 " [Conjunto de datos]."),
          tags$p(class = "referencia",
                 "Secretaría de Hacienda y Crédito Público [SHCP]. (s. f.). ",
                 tags$i("Recaudación de impuestos estatales, impuesto predial y recaudación local por entidad federativa y municipio, 2004–2025"),
                 " [Conjunto de datos]."),
          tags$p(class = "referencia",
                 "Secretaría de Hacienda y Crédito Público [SHCP]. (2010–2026). ",
                 tags$i("Iniciativa de Ley de Ingresos de la Federación"), " y ",
                 tags$i("Ley de Ingresos de la Federación"), ", ejercicios fiscales 2010 a 2026. ",
                 tags$a(href = "https://www.finanzaspublicas.hacienda.gob.mx", target = "_blank",
                        "https://www.finanzaspublicas.hacienda.gob.mx")),
          tags$p(class = "referencia",
                 "Secretaría de Hacienda y Crédito Público [SHCP]. (2021–2026). ",
                 tags$i("Renuncias Recaudatorias"), ". Informes al Congreso de la Unión. ",
                 tags$a(href = "https://www.finanzaspublicas.hacienda.gob.mx", target = "_blank",
                        "https://www.finanzaspublicas.hacienda.gob.mx")),
          tags$p(class = "referencia",
                 "angelnmara. (s. f.). ", tags$i("geojson"),
                 " [Repositorio de GitHub; archivo «mexicoHigh.json», cartografía estatal]. ",
                 tags$a(href = "https://github.com/angelnmara/geojson", target = "_blank",
                        "https://github.com/angelnmara/geojson"))
        )
      )
    ),

    nav_spacer(),
    nav_item(tags$span(class = "navbar-text", "Elaborado por: Arroyo Aguilera Erick Ulises y Aranda Nassar Adiel Alejandro"))
  )
}

# -----------------------------------------------------------------------------
# 5. Servidor
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  # Panel de controles: se abre en las secciones con gráficas y se cierra en las demás
  observeEvent(input$nav, {
    if (exists("toggle_sidebar", where = asNamespace("bslib"), mode = "function"))
      bslib::toggle_sidebar("controles", open = input$nav %in% tabs_con_controles)
  })

  # ===== 1.A Comparación internacional =========================================
  int_sel <- reactive(list(
    prom = input$int_comp %||% character(0),
    pais = setdiff(input$int_paises %||% character(0), "MEX")
  ))

  output$g_int_serie <- renderPlotly({
    s <- int_sel()
    d <- series_comparacion(rec_int, s$prom, s$pais) |>
      mutate(y = valor,
             texto = str_glue("<b>{serie}</b><br>{anio}: {number(y, accuracy = 0.1)}% del PIB"))
    graf_lineas(d, "Ingresos tributarios (% del PIB)")
  })

  output$t_int_rank <- renderText(
    str_glue("Gráfica A3. Ingresos tributarios como porcentaje del PIB por país, {input$int_anio}"))

  int_rank_datos <- reactive({
    cods <- switch(input$int_universo, ocde = names(paises_ocde), alc = names(paises_alc),
                   names(nombres_pais))
    a <- input$int_anio
    rec_paises |>
      filter(anio == a, codigo %in% cods) |>
      mutate(grupo = factor(grupo_pais(codigo), levels = niveles_grupo),
             pais  = fct_reorder(pais, valor),
             texto = str_glue("<b>{pais}</b> ({grupo})<br>{number(valor, accuracy = 0.1)}% del PIB, {a}"))
  })

  output$t_int_rank_nota <- renderText({
    d <- int_rank_datos()
    str_glue("Fuente: OCDE, Global Revenue Statistics Database. {nrow(d)} países con dato en {input$int_anio}; ",
             "algunos países no tienen dato en los años más recientes. ",
             "Chile, Colombia y Costa Rica pertenecen a la OCDE y a ALC. ",
             "Las líneas verticales marcan los promedios seleccionados.")
  })

  output$g_int_rank <- renderPlotly({
    d <- int_rank_datos()
    a <- input$int_anio
    lineas <- promedios_int |> filter(anio == a, codigo %in% (input$int_comp %||% character(0)))
    p <- ggplot(d, aes(valor, pais, fill = grupo, text = texto)) +
      geom_col(width = 0.78)
    if (nrow(lineas))
      p <- p + geom_vline(data = lineas, aes(xintercept = valor, linetype = serie),
                          color = tinta, linewidth = 0.6)
    p <- p +
      scale_fill_manual(values = colores_grupo, drop = TRUE) +
      scale_linetype_manual(values = c("Promedio OCDE" = "dashed", "Promedio ALC" = "dotted")) +
      scale_x_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Ingresos tributarios (% del PIB)", y = NULL) +
      theme_eco(base_size = 11) +
      theme(panel.grid.major.x = element_line(color = "#e3e3e3", linewidth = 0.3),
            panel.grid.major.y = element_blank())
    a_plotly(p)
  })

  output$g_int_cuna <- renderPlotly({
    s <- int_sel()
    d <- series_comparacion(cuna, intersect(s$prom, "OECD_REP"), s$pais) |>
      mutate(y = valor,
             texto = str_glue("<b>{serie}</b><br>{anio}: {number(y, accuracy = 0.1)}% del costo laboral"))
    graf_lineas(d, "Cuña fiscal (% del costo laboral)")
  })

  output$t_int_cuna_nota <- renderText({
    s <- int_sel()
    faltan <- setdiff(s$pais, unique(cuna$codigo))
    c(if ("A9" %in% s$prom)
        "No existe promedio ALC para esta variable; de ALC solo hay Chile, Colombia, Costa Rica y México (los miembros de la OCDE).",
      if (length(faltan))
        str_c("Sin dato en esta base: ", str_c(nombre_serie(faltan), collapse = ", "), ".")) |>
      str_c(collapse = " ")
  })

  output$t_int_ss <- renderText(
    str_glue("Gráfica A6. ¿Cómo se financia la seguridad social? México frente a la OCDE, {input$int_anio} (porcentaje del PIB)"))

  output$g_int_ss <- renderPlotly({
    a <- input$int_anio
    cods <- c("MEX", "OECD_REP")   # solo México frente al promedio OCDE
    etq  <- \(cod) if_else(cod == "MEX", "<b>México</b>", nombre_serie(cod))
    d <- seg_soc |>
      filter(anio == a, codigo %in% cods) |>
      group_by(codigo) |> mutate(total = sum(valor)) |> ungroup() |>
      mutate(serie = factor(etq(codigo), levels = rev(unique(etq(cods)))),
             comp  = factor(comp, levels = rev(names(comp_ss)), labels = rev(unname(comp_ss))),
             texto = str_glue("<b>{nombre_serie(codigo)}</b><br>{comp}: {number(valor, accuracy = 0.1)}% del PIB",
                              "<br>Total: {number(total, accuracy = 0.1)}% del PIB")) |>
      droplevels()
    shiny::validate(need(nrow(d) > 0, "No hay datos para este año."))
    p <- ggplot(d, aes(valor, serie, fill = comp, text = texto)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = col_ss) +
      scale_x_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Porcentaje del PIB", y = NULL) +
      theme_eco(base_size = 11) +
      theme(panel.grid.major.x = element_line(color = "#e3e3e3", linewidth = 0.3),
            panel.grid.major.y = element_blank())
    a_plotly(p)
  })

  output$g_int_petro <- renderPlotly({
    s <- int_sel()
    d <- series_comparacion(petro, s$prom, s$pais) |>
      mutate(y = valor,
             texto = str_glue("<b>{serie}</b><br>{anio}: {number(y, accuracy = 0.01)}% del PIB"))
    graf_lineas(d, "Rentas del petróleo (% del PIB)", cortes = seq(1990, 2020, 5))
  })

  output$g_int_div <- renderPlotly({
    med <- input$int_div_medida %||% "total"
    d0 <- div_ocde |>
      mutate(pais  = unname(nombres_pais[codigo]),
             etiq  = case_when(codigo == "MEX" ~ "<b>México</b>",
                               codigo %in% c("CHL", "CRI") ~ str_c(pais, " (ALC)"),
                               TRUE ~ pais),
             es_mx = codigo == "MEX")
    etq <- c(cit = "Impuesto corporativo", adic = "Impuesto personal sobre el dividendo",
             pit = "Impuesto personal neto sobre el dividendo")

    d <- switch(med,
      total = d0 |> mutate(orden = total, ref = mean(total)) |>
        pivot_longer(c(cit, adic), names_to = "comp", values_to = "valor") |>
        mutate(comp = factor(comp, levels = c("adic", "cit"), labels = unname(etq[c("adic", "cit")]))),
      cit   = d0 |> mutate(orden = cit, ref = mean(cit), comp = factor(etq[["cit"]]), valor = cit),
      pit   = d0 |> filter(!is.na(pit_neto)) |>
        mutate(orden = pit_neto, ref = mean(pit_neto), comp = factor(etq[["pit"]]), valor = pit_neto)
    )
    d <- d |>
      mutate(etiq  = fct_reorder(etiq, orden, .fun = max),
             texto = str_glue("<b>{pais}</b> (sistema {trat_es})<br>{comp}: {number(valor, accuracy = 0.1)}%",
                              "<br>Tasa combinada: {number(total, accuracy = 0.1)}%"))
    cols <- c("Impuesto corporativo" = "#0d0887",
              "Impuesto personal sobre el dividendo" = "#f89540",
              "Impuesto personal neto sobre el dividendo" = "#f89540")

    p <- ggplot(d, aes(valor, etiq, fill = comp, text = texto)) +
      geom_col(aes(color = es_mx), width = 0.78, linewidth = 0.7) +
      scale_color_manual(values = c(`TRUE` = "#111111", `FALSE` = "#ffffff"), guide = "none") +
      scale_fill_manual(values = cols) +
      scale_x_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Tasa legal (%)", y = NULL) +
      theme_eco(base_size = 11) +
      theme(panel.grid.major.x = element_line(color = "#e3e3e3", linewidth = 0.3),
            panel.grid.major.y = element_blank())
    if ("OECD_REP" %in% (input$int_comp %||% character(0)))
      p <- p + geom_vline(xintercept = d$ref[1], linetype = "dashed", color = tinta, linewidth = 0.6)
    a_plotly(p)
  })

  # ===== Transferencias federales =============================================
  tr_sel <- \(d, conc) if (conc == "total") d else d |> filter(concepto == conc)

  output$t_tr_rank <- renderText(
    str_glue("Gráfica 10. Transferencias federales por habitante y entidad, {input$tr_anio} (pesos corrientes)"))

  output$t_tr_rank_nota <- renderText({
    d <- transf |> filter(serie == "anual", anio == as.integer(input$tr_anio),
                          concepto %in% c("participaciones_ramo28", "aportaciones_ramo33"))
    conc <- input$tr_concepto %||% "total"
    s <- tr_sel(d, conc)
    nac <- sum(s$monto) / sum(distinct(d, cve_ent, hab)$hab)
    str_glue("Fuente: INEGI, Estadística de Finanzas Públicas Estatales y Municipales (EFIPEM), integrada por Calcu.mx. ",
             "La línea punteada es el valor nacional de «{names(etq_transf)[etq_transf == conc]}»: {number(nac, accuracy = 1, big.mark = ',')} pesos por habitante. ",
             "*Ciudad de México no tiene FONE ni FAETA: su cifra no es comparable. El total nacional es una suma de las entidades; en 2025 queda 5.2% abajo del total de aportaciones que publica la SHCP.")
  })

  output$g_tr_rank <- renderPlotly({
    a    <- as.integer(input$tr_anio)
    conc <- input$tr_concepto %||% "total"
    d <- transf |>
      filter(serie == "anual", anio == a,
             concepto %in% c("participaciones_ramo28", "aportaciones_ramo33")) |>
      mutate(comp = factor(concepto, levels = c("aportaciones_ramo33", "participaciones_ramo28"),
                           labels = c("Aportaciones (Ramo 33)", "Participaciones (Ramo 28)")))
    orden <- tr_sel(d, conc) |> group_by(etiq) |> summarise(o = sum(pc), .groups = "drop")
    nac   <- sum(tr_sel(d, conc)$monto) / sum(distinct(d, cve_ent, hab)$hab)
    d <- d |>
      left_join(orden, by = "etiq") |>
      mutate(etiq  = fct_reorder(etiq, o),
             texto = str_glue("<b>{etiq}</b><br>{comp}: {number(pc, accuracy = 1, big.mark = ',')} pesos por habitante",
                              "<br>{number(monto / 1e6, accuracy = 0.1, big.mark = ',')} millones de pesos"))
    p <- ggplot(d, aes(pc, etiq, fill = comp, text = texto)) +
      geom_col(width = 0.78) +
      geom_vline(xintercept = nac, linetype = "dashed", color = tinta, linewidth = 0.6) +
      scale_fill_manual(values = col_part_apor) +
      scale_x_continuous(labels = label_dollar(accuracy = 1, big.mark = ","),
                         expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Pesos corrientes por habitante", y = NULL) +
      theme_eco(base_size = 11) +
      theme(panel.grid.major.x = element_line(color = "#e3e3e3", linewidth = 0.3),
            panel.grid.major.y = element_blank())
    a_plotly(p)
  })

  # ===== 1. Federal ============================================================
  output$g_lif_petro <- renderPlotly({
    d <- petro_fed |>
      mutate(comp  = factor(comp, levels = rev(names(comp_petro)), labels = rev(unname(comp_petro))),
             texto = str_glue("<b>{anio}</b><br>{comp}: {number(valor, accuracy = 0.1)}% del PIB",
                              "<br>Total presupuestario: {number(total, accuracy = 0.1)}% del PIB"))
    p <- ggplot(d, aes(anio, valor, fill = comp, text = texto)) +
      geom_col(width = 0.75) +
      scale_fill_manual(values = col_petro, breaks = unname(comp_petro)) +
      scale_x_continuous(breaks = seq(2010, 2026, 2)) +
      scale_y_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.05))) +
      labs(x = NULL, y = "Porcentaje del PIB") +
      theme_eco()
    a_plotly(p)
  })

  lif_valores <- reactive({
    req(input$lif_rubros)
    base  <- lif_armonizado |> mutate(valor = .data[[input$lif_version]])
    total <- base |> filter(clave == "total") |> select(anio, total = valor)

       base |>
      filter(rubro %in% input$lif_rubros) |>
      left_join(total, by = "anio") |>
      left_join(deflactor, by = "anio") |>
      group_by(rubro) |>
      arrange(anio, .by_group = TRUE) |>
      mutate(y = case_when(
        input$lif_unidad == "Millones de pesos de 2010"           ~ valor * a_2010,
        input$lif_unidad == "Porcentaje de los ingresos totales" ~ valor / total * 100,
        input$lif_unidad == "Índice (2010 = 100)"                 ~ valor / first(valor) * 100,
        TRUE                                                      ~ valor
      )) |>
      ungroup() |>
      mutate(rubro = factor(rubro, levels = input$lif_rubros))
  })

  output$g_lif_series <- renderPlotly({
    d <- lif_valores()
    unidad <- input$lif_unidad
     sufijo <- switch(unidad, "Millones de pesos corrientes" = " mdp",
                     "Millones de pesos de 2010" = " mdp de 2010",
                     "Porcentaje de los ingresos totales" = " %", "")
    d <- d |> mutate(texto = str_glue("<b>{rubro}</b><br>{anio}: {number(y, accuracy = 0.1, big.mark = ',')}{sufijo}"))
    colores <- set_names(vir(length(input$lif_rubros)), input$lif_rubros)

    p <- ggplot(d, aes(anio, y, color = rubro, group = rubro, text = texto)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2) +
      scale_color_manual(values = colores) +
      scale_x_continuous(breaks = seq(2010, 2026, 2)) +
      scale_y_continuous(labels = label_comma()) +
      labs(x = "Ejercicio fiscal", y = unidad) +
      theme_eco()
    a_plotly(p)
  })

  output$g_lif_comp <- renderPlotly({
    niveles <- c("ISR", "IVA", "IEPS", "Comercio exterior", "Otros impuestos")
    d <- lif_armonizado |>
      select(anio, clave, aprobado) |>
      pivot_wider(names_from = clave, values_from = aprobado) |>
      transmute(anio, ISR = isr, IVA = iva, IEPS = ieps, `Comercio exterior` = comext,
                `Otros impuestos` = impuestos - isr - iva - ieps - comext, impuestos) |>
      pivot_longer(-c(anio, impuestos), names_to = "impuesto", values_to = "monto") |>
      mutate(impuesto = factor(impuesto, levels = niveles),
             share = monto / impuestos,
             texto = str_glue("<b>{impuesto}</b>, {anio}<br>{fmt_pct(share)} de los ingresos tributarios",
                              "<br>{fmt_mdp(monto)} millones de pesos"))

    p <- ggplot(d, aes(factor(anio), share, fill = impuesto, text = texto)) +
      geom_col(width = 0.75, color = "#ffffff", linewidth = 0.3) +
      scale_fill_manual(values = set_names(vir(5), niveles)) +
      scale_y_continuous(labels = label_percent()) +
      scale_x_discrete(breaks = as.character(seq(2010, 2026, 2))) +
      labs(x = "Ejercicio fiscal", y = "Proporción de los ingresos tributarios") +
      theme_eco()
    a_plotly(p)
  })

  # ===== 2. Estatal ============================================================
  es_pc <- reactive(input$est_medida == "percapita")
  medida_lab <- reactive(if (es_pc()) "Pesos por habitante" else "Millones de pesos corrientes")
  fmt_val <- function(x, pc) if (pc) fmt_pesos(x) else str_c(fmt_mdp(x), " mdp")

  est_anio <- reactive({
    local_largo |>
      filter(anio == input$est_anio, rubro == input$est_rubro) |>
      mutate(y = if (es_pc()) percapita else monto / 1e6)
  })

  nac_anio <- reactive({
    local_nacional |>
      filter(anio == input$est_anio, rubro == input$est_rubro) |>
      mutate(y = if (es_pc()) percapita else monto / 1e6 / 32)
  })

  output$t_est_mapa <- renderText(
    str_glue("Gráfica 6. {input$est_rubro} por entidad federativa, {input$est_anio}"))
  output$t_est_mapa_nota <- renderText(
    str_c(medida_lab(), ". ",
          if (is.null(geo_mx)) texto_sin_mapa else "Pase el cursor sobre cada entidad para ver su valor.",
          " ", fuentes$local, " ", fuentes$mapa))

  output$g_est_mapa <- renderPlotly({
    d <- est_anio() |>
      mutate(texto = str_glue("<b>{entidad}</b><br>{fmt_val(y, es_pc())}"))
    mapa_estatal(d, medida_lab())
  })

  output$t_est_rank <- renderText(
    str_glue("Gráfica 7. Ordenamiento de entidades: {input$est_rubro}, {input$est_anio}"))

  output$g_est_rank <- renderPlotly({
    d <- est_anio() |> filter(!is.na(y))
    nac <- nac_anio()
    etiqueta_ref <- if (es_pc()) "Nacional" else "Promedio por entidad"

    d <- d |> mutate(entidad = fct_reorder(entidad, y),
                     texto = str_glue("<b>{entidad}</b><br>{fmt_val(y, es_pc())}"))

    p <- ggplot(d, aes(y, entidad, fill = y, text = texto)) +
      geom_col(width = 0.72) +
      geom_vline(xintercept = nac$y, linetype = "dashed", color = tinta_sec, linewidth = 0.4) +
      annotate("text", x = nac$y, y = 1.3, label = etiqueta_ref, hjust = -0.05,
               size = 3.6, color = tinta_sec, family = fuente) +
      scale_fill_viridis_c(guide = "none", end = 0.9) +
      scale_x_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.05))) +
      labs(x = medida_lab(), y = NULL) +
      theme_eco(11) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#e3e3e3", linewidth = 0.3))
    a_plotly(p, leyenda = FALSE)
  })

  # ===== 3. Predial ============================================================
  pre_nac   <- reactive(identical(input$pre_ent, "Nacional"))
  pre_ambito <- reactive(if (pre_nac()) predial else predial |> filter(entidad == input$pre_ent))
  pre_lab   <- \(d) if (pre_nac()) d$mun_lab else d$municipio   # en la vista nacional se agrega la entidad al nombre

  pre_sel <- reactive(pre_ambito() |> filter(anio == input$pre_anio))

  output$vb_total   <- renderText(str_c(fmt_mdp(sum(pre_sel()$monto_predial, na.rm = TRUE) / 1e6), " mdp"))
  output$vb_pc      <- renderText({
    d <- pre_sel() |> filter(!is.na(monto_predial), !is.na(poblacion))
    fmt_pesos(sum(d$monto_predial) / sum(d$poblacion))
  })
  output$vb_munis   <- renderText(str_glue("{sum(pre_sel()$monto_predial > 0, na.rm = TRUE)} de {nrow(pre_sel())}"))
  output$vb_cuentas <- renderText(label_comma()(sum(pre_sel()$cuentas_pagadas, na.rm = TRUE)))

  output$t_pre_top <- renderText(
    str_glue("Gráfica 8. Municipios con {if (input$pre_orden == 'alto') 'mayor' else 'menor'} ",
             "recaudación de predial por habitante: {if (pre_nac()) 'nacional' else input$pre_ent}, {input$pre_anio}"))

  output$g_pre_top <- renderPlotly({
    d <- pre_sel() |> filter(!is.na(percapita), percapita > 0)
    d <- if (input$pre_orden == "alto") slice_max(d, percapita, n = input$pre_n, with_ties = FALSE)
         else slice_min(d, percapita, n = input$pre_n, with_ties = FALSE)
    shiny::validate(need(nrow(d) > 0, "No hay datos para esta selección."))

    d <- d |> mutate(
      municipio = fct_reorder(pre_lab(d), percapita),
      texto = str_glue("<b>{municipio}</b><br>{fmt_pesos(percapita)} por habitante",
                       "<br>Recaudación: {fmt_pesos(monto_predial)}",
                       "<br>Población: {label_comma()(poblacion)}",
                       "<br>Lugar estatal: {lugar_estatal_percapita} · nacional: {lugar_nacional_percapita}"))

    p <- ggplot(d, aes(percapita, municipio, fill = percapita, text = texto)) +
      geom_col(width = 0.72) +
      scale_fill_viridis_c(guide = "none", end = 0.9) +
      scale_x_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.05))) +
      labs(x = "Pesos por habitante", y = NULL) +
      theme_eco(11) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#e3e3e3", linewidth = 0.3))
    a_plotly(p, leyenda = FALSE)
  })

  output$t_pre_mapa <- renderText(
    str_glue("Gráfica 9. Recaudación de impuesto predial por habitante por entidad federativa, {input$pre_anio}"))
  output$t_pre_mapa_nota <- renderText(
    str_c("Pesos por habitante: suma del predial municipal entre la población de los municipios que reportan. ",
          "Las entidades se agrupan en ", input$pre_clases %||% 6, " intervalos con un número parecido de entidades cada uno (cuantiles), ",
          "para que la alta recaudación de la Ciudad de México no concentre toda la escala de color. ",
          if (pre_nac()) "" else str_c("Con borde negro: ", input$pre_ent, ". "),
          if (is.null(geo_mx)) texto_sin_mapa else ""))

  output$g_pre_mapa <- renderPlotly({
    d <- predial |>
      filter(anio == input$pre_anio) |>
      group_by(cve_ent, entidad) |>
      summarise(monto    = sum(monto_predial, na.rm = TRUE),
                pob      = sum(poblacion[!is.na(monto_predial)], na.rm = TRUE),
                mediana  = median(percapita[percapita > 0], na.rm = TRUE),
                con_dato = sum(monto_predial > 0, na.rm = TRUE),
                n_mun    = n(), .groups = "drop") |>
      mutate(y     = if_else(pob > 0, monto / pob, NA_real_),
             clase = intervalos(y, input$pre_clases %||% 6),
             texto = str_glue("<b>{entidad}</b><br>{fmt_pesos(y)} por habitante",
                              "<br>Intervalo: {clase}",
                              "<br>Recaudación: {fmt_mdp(monto / 1e6)} mdp",
                              "<br>Mediana municipal: {fmt_pesos(mediana)} por habitante",
                              "<br>Municipios con dato: {con_dato} de {n_mun}"))
    cve_sel <- if (pre_nac()) character(0) else nombres_ent |> filter(entidad == input$pre_ent) |> pull(cve_ent)
    mapa_estatal(d, "Pesos por habitante", resaltar = cve_sel, clases = d$clase)
  })

  # ===== 4. Renuncias recaudatorias ============================================
  ren_col  <- reactive(input$ren_unidad)
  ren_lab  <- reactive(names(unidades_ren)[unidades_ren == input$ren_unidad])
  fmt_ren  <- function(x, u) {
    if (u == "pib") str_c(number(x, accuracy = 0.001), " % del PIB") else str_c(fmt_mdp(x), " mdp")
  }

  ren_anual <- reactive(ren_herr |> filter(anio == as.integer(input$ren_anio)))

  output$vb_ren_t1 <- renderText(str_glue("Renuncias recaudatorias {input$ren_anio}"))
  output$vb_ren_t2 <- renderText(str_glue("Monto {input$ren_anio} (millones de pesos de 2026)"))
  output$vb_ren_pib <- renderText(str_c(number(sum(ren_anual()$pib, na.rm = TRUE), accuracy = 0.01), " % del PIB"))
  output$vb_ren_mdp <- renderText(fmt_mdp(sum(ren_anual()$real2026, na.rm = TRUE)))
  output$vb_ren_lif <- renderText({
    a <- as.integer(input$ren_anio)
    imp <- impuestos_lif |> filter(anio == a) |> pull(impuestos)
    if (!length(imp)) return("Sin LIF para ese año")
    str_c(fmt_pct(sum(ren_anual()$nominal, na.rm = TRUE) / imp), " de los ingresos tributarios")
  })

  output$g_ren_evol <- renderPlotly({
    u <- ren_col()
    herr <- sort(unique(ren_herr$herramienta))
    d <- ren_herr |>
      group_by(anio, herramienta) |>
      summarise(y = sum(.data[[u]], na.rm = TRUE), .groups = "drop") |>
      complete(anio = 2002:2026, herramienta, fill = list(y = NA_real_)) |>
      mutate(herramienta = factor(herramienta, levels = herr),
             texto = str_glue("<b>{herramienta}</b>, {anio}<br>{fmt_ren(y, u)}"))
    tot <- d |> group_by(anio) |>
      summarise(y = if (all(is.na(y))) NA_real_ else sum(y, na.rm = TRUE)) |>
      mutate(texto = str_glue("<b>Total</b>, {anio}<br>{fmt_ren(y, u)}"))

    p <- ggplot(d |> filter(!is.na(y)), aes(anio, y, fill = herramienta, text = texto)) +
      geom_col(width = 0.75, color = "#ffffff", linewidth = 0.2) +
      geom_point(data = tot |> filter(!is.na(y)), aes(anio, y, text = texto), inherit.aes = FALSE,
                 shape = 21, fill = "#ffffff", color = tinta, size = 2) +
      scale_fill_manual(values = set_names(vir(length(herr)), herr)) +
      scale_x_continuous(breaks = seq(2002, 2026, 2)) +
      scale_y_continuous(labels = label_comma()) +
      labs(x = "Año del informe", y = ren_lab()) +
      theme_eco()
    a_plotly(p)
  })

  # Renuncias disponibles para el impuesto elegido, agrupadas por tratamiento
  observeEvent(input$dec_herr, {
    conc <- ren_decil |>
      filter(herramienta_fiscal == input$dec_herr) |>
      distinct(tratamiento, concepto) |>
      arrange(tratamiento, concepto) |>
      mutate(op = str_c(tratamiento, ": ", concepto))
    opciones <- lapply(split(set_names(conc$op, conc$concepto), conc$tratamiento), as.list)
    sel <- if (any(conc$concepto == "Alimentos")) conc$op[conc$concepto == "Alimentos"][1] else conc$op[1]
    updateSelectizeInput(session, "dec_concepto", choices = opciones, selected = sel)
  })

  # Años de observación disponibles para la renuncia elegida
  dec_opciones <- reactive({
    req(input$dec_concepto)
    ren_decil |>
      filter(herramienta_fiscal == input$dec_herr,
             str_c(tratamiento, ": ", concepto) == input$dec_concepto, !is.na(valor)) |>
      distinct(anio, ciclo_observacion) |>
      arrange(anio) |>
      mutate(obs   = as.character(ciclo_observacion),
             # si dos informes comparten año de observación, se distingue el informe
             label = if_else(duplicated(obs) | duplicated(obs, fromLast = TRUE),
                             str_c(obs, " (informe ", anio, ")"), obs))
  })

  observeEvent(dec_opciones(), {
    o   <- dec_opciones()
    sel <- intersect(isolate(input$dec_anio) %||% character(0), as.character(o$anio))
    if (!length(sel)) sel <- as.character(max(o$anio))
    updateSelectizeInput(session, "dec_anio", choices = set_names(as.character(o$anio), o$label),
                         selected = sel)
  })

  dec_datos <- reactive({
    req(input$dec_concepto, input$dec_anio)
    o <- dec_opciones() |> filter(anio %in% as.integer(input$dec_anio))
    ren_decil |>
      filter(herramienta_fiscal == input$dec_herr,
             str_c(tratamiento, ": ", concepto) == input$dec_concepto,
             anio %in% o$anio, !is.na(valor)) |>
      inner_join(select(o, anio, label), by = "anio") |>
      mutate(serie = factor(str_c("Observación ", label),
                            levels = str_c("Observación ", o$label))) |>
      group_by(serie) |>
      arrange(decil, .by_group = TRUE) |>
      mutate(acum = cumsum(valor) / sum(valor) * 100) |>
      ungroup()
  })

  output$t_dec <- renderText({
    conc <- str_remove(input$dec_concepto %||% "", "^[^:]+: ")
    trat <- str_extract(input$dec_concepto %||% "", "^[^:]+")
    str_glue("Gráfica 5. ¿Quién se beneficia de la renuncia? Distribución por decil de ingreso: {conc} ({str_to_lower(trat)})")
  })

  output$t_dec_nota <- renderText(
    if (identical(input$dec_vista, "curva"))
      "Curva de concentración: proporción acumulada de la renuncia que reciben los hogares desde el decil I hasta cada decil. La diagonal punteada indicaría una distribución igualitaria; una curva por debajo de ella significa que la renuncia beneficia más a los hogares de mayores ingresos."
    else
      "Proporción del monto total de la renuncia que recibe cada decil de ingreso. La línea punteada (10 %) indicaría una distribución igualitaria."
  )

  output$g_dec <- renderPlotly({
    d <- dec_datos()
    shiny::validate(need(nrow(d) > 0, "No hay datos de distribución para esta selección."))
    series  <- levels(droplevels(d$serie))
    colores <- if (length(series) == 1) set_names(vir_uno, series) else set_names(vir(length(series)), series)

    if (identical(input$dec_vista, "curva")) {
      d0 <- tibble(serie = factor(series, levels = series), x = 0, acum = 0, texto = "")
      d  <- d |>
        mutate(x = as.integer(decil) * 10,
               texto = as.character(str_glue("<b>{serie}</b><br>Deciles I a {decil} (el {x}% más pobre):",
                                             "<br>{number(acum, accuracy = 0.1)} % de la renuncia"))) |>
        select(serie, x, acum, texto) |>
        bind_rows(d0) |>
        arrange(serie, x)
      p <- ggplot(d, aes(x, acum, color = serie, group = serie, text = texto)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = tinta, linewidth = 0.5) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 1.8) +
        scale_color_manual(values = colores) +
        scale_x_continuous(breaks = seq(0, 100, 10), labels = label_number(suffix = "%")) +
        scale_y_continuous(breaks = seq(0, 100, 10), labels = label_number(suffix = "%")) +
        coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
        labs(x = "Población acumulada, del decil más pobre al más rico",
             y = "Proporción acumulada de la renuncia") +
        theme_eco(11)
    } else {
      d <- d |>
        mutate(texto = str_glue("<b>Decil {decil}</b><br>{serie}<br>{number(valor, accuracy = 0.1)} % de la renuncia"))
      p <- ggplot(d, aes(decil, valor, fill = serie, text = texto)) +
        geom_col(position = position_dodge(width = 0.82), width = 0.78) +
        geom_hline(yintercept = 10, linetype = "dashed", color = tinta, linewidth = 0.5) +
        scale_fill_manual(values = colores) +
        scale_y_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.05))) +
        labs(x = "Decil de ingreso (I = más pobre, X = más rico)", y = "Proporción de la renuncia") +
        theme_eco(11)
    }
    a_plotly(p, leyenda = length(series) > 1)
  })

  # Resumen en texto: cuánto recibe el 20 % más rico frente al 20 % más pobre
  output$dec_resumen <- renderUI({
    d <- dec_datos()
    req(nrow(d) > 0)
    r <- d |>
      group_by(serie) |>
      summarise(ricos  = sum(valor[decil %in% c("IX", "X")]) / sum(valor) * 100,
                pobres = sum(valor[decil %in% c("I", "II")]) / sum(valor) * 100,
                .groups = "drop")
    tags$ul(class = "fuente-nota", style = "margin-top: 8px;",
            lapply(seq_len(nrow(r)), \(i) {
              veces <- if (r$pobres[i] > 0) str_glue(" ({number(r$ricos[i] / r$pobres[i], accuracy = 0.1)} veces más)") else ""
              tags$li(tags$b(as.character(r$serie[i]), ": "),
                      str_glue("el 20% más rico (deciles IX y X) recibe {number(r$ricos[i], accuracy = 0.1)}% de la renuncia; ",
                               "el 20% más pobre (deciles I y II), {number(r$pobres[i], accuracy = 0.1)}%{veces}."))
            }))
  })

}

shinyApp(ui, server)
