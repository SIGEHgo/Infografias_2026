library(httr2)
library(rvest)

# ==================================================
# 1. Descargar página principal
# ==================================================

pagina_url = "https://www.gob.mx/sesnsp/documentos/historico-de-incidencia-delictiva-del-fuero-comun?state=published"

ua = paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/120.0.0.0 Safari/537.36"
)

respuesta = request(pagina_url) |>
  req_user_agent(ua) |>
  req_error(is_error = function(resp) FALSE) |>
  req_perform()

if (resp_status(respuesta) == 200) {
  
  pagina = resp_body_html(respuesta, check_type = FALSE)
  
  print("Página descargada correctamente")
  
} else {
  
  stop(paste("Error HTTP:", resp_status(respuesta)))
  
}


# ==================================================
# 2. Extraer enlaces
# ==================================================

hrefs = pagina |>
  html_elements("a[href]") |>
  html_attr("href") |>
  unique()

hrefs = hrefs[!is.na(hrefs) & nzchar(hrefs)]

hrefs = url_absolute(hrefs, pagina_url)


# ==================================================
# 3. Filtrar archivos Excel
# ==================================================

hrefs_xlsx = hrefs[
  grepl("\\.xlsx", hrefs, ignore.case = TRUE)
]

hrefs_xlsx = hrefs_xlsx[
  !grepl(
    "RNID-Delitos|CNSP-Delitos|IncidenciaDelictiva|Estatal-Delito",
    hrefs_xlsx,
    ignore.case = TRUE
  )
]

hrefs_xlsx = unique(hrefs_xlsx)

nombres = basename(hrefs_xlsx) |>  gsub(pattern =  "_.*", replacement = "") |> 
  gsub(pattern =  ".xlsx", replacement = "") |>  stringr::str_squish()



# ==================================================
# 4. Crear directorio
# ==================================================

dir_destino = "Inputs/Page 3/Secretariado"

dir.create(
  dir_destino,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==================================================
# 5. Descargar todos los archivos
# ==================================================

resultados = data.frame(
  archivo = nombres,
  url = hrefs_xlsx,
  status = NA_integer_,
  descargado = FALSE
)

for (i in seq_along(hrefs_xlsx)) {
  
  destino = file.path(dir_destino, paste0(nombres[i], ".xlsx"))
  
  print(paste("Descargando:", nombres[i]))
  
  respuesta_archivo = tryCatch({
    
    request(hrefs_xlsx[i]) |>
      
      req_user_agent(ua) |>
      
      req_headers(
        Referer = pagina_url,
        Accept = "*/*",
        `Accept-Language` = "es-MX,es;q=0.9"
      ) |>
      
      req_options(
        followlocation = TRUE
      ) |>
      
      req_error(is_error = function(resp) FALSE) |>
      
      req_perform(path = destino)
    
  }, error = function(e) {
    
    message("Error: ", conditionMessage(e))
    
    NULL
    
  })
  
  
  if (is.null(respuesta_archivo)) {
    next
  }
  
  # Código HTTP
  status = resp_status(respuesta_archivo)
  
  resultados$status[i] = status
  
  # Verificar que sea un archivo Excel válido
  valido = FALSE
  
  if (status == 200 && file.exists(destino)) {
    
    firma = readBin(
      destino,
      what = "raw",
      n = 2
    )
    
    valido = identical(
      firma,
      as.raw(c(0x50, 0x4B))
    )
    
  }
  
  resultados$descargado[i] = valido
  
  if (valido) {
    
    message("Descarga correcta: ", nombres[i])
    
  } else {
    
    message("No se pudo descargar: ", nombres[i])
    
    if (file.exists(destino)) {
      file.remove(destino)
    }
    
  }
  
}


# ==================================================
# 6. Revisar resultados
# ==================================================

print(resultados)

# Archivos descargados correctamente
resultados[resultados$descargado, ]

# Archivos que fallaron
resultados[!resultados$descargado, ]










### Abrir los archivos y filtrar

archivos = "Inputs/Page 3/Secretariado/" |> 
  list.files(pattern = "\\.xlsx$", full.names = TRUE)

nombres = archivos |>  
  basename() |>  
  gsub(pattern = "\\.xlsx$", replacement = "") |>  
  stringr::str_squish()

datos = archivos |> 
  lapply(function(x) {
    datos = readxl::read_excel(x)
    
    datos = datos |> 
      dplyr::filter(Clave_Ent == 13) |> 
      dplyr::mutate(
        Total = rowSums(
          dplyr::across(Enero:Diciembre),
          na.rm = TRUE
        )
      )
    
    return(datos)
  })

datos |>  names() = nombres



source("../../Importantes_documentos_usar/Conexion_Postgres_CITNOVA.R")


for (i in seq_along(datos)) {
  DBI::dbWriteTable(
    conn = buig,
    name = paste0("Incidencia del fuero común Secretariado Hidalgo ", nombres[i]),
    value = datos[[i]],
    overwrite = T,
    row.names = F
  )
  
}



