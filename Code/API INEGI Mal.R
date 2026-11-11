library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

# Token
token = "502d3ff3-2ca0-2979-ea88-7c799d9c0974"

# Funcion para consultar la API
consultar_inegi <- function(tipo, clave, area = NULL) {
  
  if (tipo == "catalogo") {
    url <- paste0(
      "https://www.inegi.org.mx/app/api/indicadores/",
      "desarrolladores/jsonxml/",
      clave, "/null/es/BISE/2.0/",
      token, "?type=json"
    )
  } else {
    url <- paste0(
      "https://www.inegi.org.mx/app/api/indicadores/",
      "desarrolladores/jsonxml/INDICATOR/",
      clave, "/es/", area,
      "/false/BISE/2.0/",
      token, "?type=json"
    )
  }
  
  tryCatch({
    resp <- request(url) |>
      req_timeout(60) |>
      req_perform()
    
    resp_body_json(resp, simplifyVector = FALSE)
    
  }, error = function(e) NULL)
}

# 1. Descargar catalogo de indicadores
catalogo <- consultar_inegi(
  "catalogo",
  "CL_INDICATOR"
)

indicadores <- map_dfr(
  catalogo$CODE,
  function(x) {
    tibble(
      indicador = x$value,
      nombre = x$Description
    )
  }
)

# 2. Consultar todos los indicadores a nivel municipal
descargar_indicador <- function(id) {
  
  resultado <- consultar_inegi(
    "indicador",
    id,
    area = "999"
  )
  
  if (is.null(resultado$Series)) {
    return(tibble())
  }
  
  map_dfr(resultado$Series, function(serie) {
    
    if (is.null(serie$OBSERVATIONS)) {
      return(tibble())
    }
    
    map_dfr(serie$OBSERVATIONS, function(obs) {
      
      tibble(
        indicador = serie$INDICADOR,
        periodo = obs$TIME_PERIOD,
        valor = obs$OBS_VALUE,
        cve_geo = obs$COBER_GEO,
        unidad = serie$UNIT,
        frecuencia = serie$FREQ
      )
    })
  })
}


# 3. Descargar indicadores
resultados <- map(
  indicadores$indicador,
  function(id) {
    Sys.sleep(0.15)
    descargar_indicador(id)
  }
)


# 4. Unir resultados
base_inegi <- bind_rows(resultados)

# 5. Filtrar Hidalgo (clave estatal 13)
base_hidalgo <- base_inegi |>
  mutate(cve_geo = as.character(cve_geo)) |>
  filter(str_starts(cve_geo, "13")) |>
  left_join(indicadores, by = "indicador") |>
  select(
    cve_geo,
    indicador,
    nombre,
    periodo,
    valor,
    unidad,
    frecuencia
  )
