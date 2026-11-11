library(rvest)
library(xml2)

# URL de la página
pagina_url = "http://www.dgis.salud.gob.mx/contenidos/intercambio/clues_gobmx.html"

# Leer el HTML
pagina = read_html(pagina_url)

# Extraer los href de los enlaces
hrefs = pagina |>
  html_elements("a[href]") |>
  html_attr("href") |>
  unique()

# Eliminar valores vacíos
hrefs = hrefs[!is.na(hrefs) & nzchar(hrefs)]

# Convertir a URL absoluta
hrefs = url_absolute(hrefs, pagina_url)

# Ver los enlaces encontrados
print(hrefs)

# Filtro
hrefs_xlsx = hrefs[grepl("\\.xlsx", hrefs, ignore.case = TRUE)]


download.file(
  url = hrefs_xlsx,
  destfile = "Inputs/Page 2/ESTABLECIMIENTO_SALUD.xlsx",
  mode = "wb",
  method = "libcurl"
)




datos = "Inputs/Page 2/ESTABLECIMIENTO_SALUD.xlsx" |> 
  readxl::read_excel()

datos = datos |> 
  dplyr::filter(
    `CLAVE DE LA ENTIDAD` == "13" & 
    `ESTATUS DE OPERACION` == "EN OPERACION"
    ) |> 
  dplyr::filter(!is.na(LATITUD) | !is.na(LONGITUD))

# datos = datos |> 
#   dplyr::mutate(
#     LATITUD = LATITUD |>  as.numeric(),
#     LONGITUD = LONGITUD |>  as.numeric()
#   )


datos = datos |> 
  sf::st_as_sf(
    coords = c("LONGITUD","LATITUD"),
    crs = 4326,
    remove = F,
    na.fail = F
  )


mun = "../../Importantes_documentos_usar/Municipios/municipiosjair.shp" |>
  sf::read_sf() |>
  sf::st_transform(crs = 4326)



datos = datos |> 
  sf::st_filter(y = mun |>  sf::st_union()) |> 
  sf::st_drop_geometry()


source("../../Importantes_documentos_usar/Conexion_Postgres_CITNOVA.R")

DBI::dbWriteTable(
  conn = buig,
  name = "Establecimientos_salud_Operacion_Hidalgo",
  value = datos,
  overwrite = T
)
