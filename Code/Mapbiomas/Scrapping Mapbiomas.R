# Descarga respuestas JSON de MapBiomas para cada territorio indicado en
# `patron`: el recurso `area` de 1985 y el recurso
# `<patron>?themeKey=coverage`.

# Paquetes necesarios: chromote y jsonlite
# if (!requireNamespace("chromote", quietly = TRUE)) {
#   stop("Falta el paquete 'chromote'. Instálalo con install.packages('chromote').")
# }
#
# if (!requireNamespace("jsonlite", quietly = TRUE)) {
#   stop("Falta el paquete 'jsonlite'. Instálalo con install.packages('jsonlite').")
# }


# -----------------------------------------------------------------------------
# Configuración
# -----------------------------------------------------------------------------

patron = paste0("37-3-", 400:600)
anio_pagina = 2025L
anio_area = 1985L
carpeta = "descargas_mapbiomas"
espera_segundos = 10


# -----------------------------------------------------------------------------
# Funciones auxiliares
# -----------------------------------------------------------------------------

crear_url_plataforma = function(patron, anio) {
  patron_url = utils::URLencode(patron, reserved = TRUE)

  paste0(
    "https://plataforma.mapbiomas.org/coverage/coverage_lclu",
    "?t[regionKey]=mexico",
    "&t[ids][]=", patron_url,
    "&t[divisionCategoryId]=3",
    "&tl[id]=99",
    "&tl[themeKey]=coverage",
    "&tl[subthemeKey]=coverage_lclu",
    "&tl[pixelValues][]=22",
    "&tl[pixelValues][]=1",
    "&tl[legendKey]=default",
    "&tl[year]=", as.integer(anio)
  )
}


obtener_parametros_url = function(url) {
  url_sin_fragmento = sub("#.*$", "", url)
  inicio_query = regexpr("?", url_sin_fragmento, fixed = TRUE)[1]

  if (inicio_query < 0L) {
    return(list())
  }

  query = substring(url_sin_fragmento, inicio_query + 1L)
  if (!nzchar(query)) {
    return(list())
  }

  pares = strsplit(query, "&", fixed = TRUE)[[1]]
  parametros = vector("list", length(pares))
  nombres = character(length(pares))

  for (i in seq_along(pares)) {
    posicion_igual = regexpr("=", pares[i], fixed = TRUE)[1]

    if (posicion_igual < 0L) {
      llave = pares[i]
      valor = ""
    } else {
      llave = substring(pares[i], 1L, posicion_igual - 1L)
      valor = substring(pares[i], posicion_igual + 1L)
    }

    nombres[i] = tolower(utils::URLdecode(llave))
    parametros[[i]] = utils::URLdecode(valor)
  }

  names(parametros) = nombres
  parametros
}


obtener_parametro = function(parametros, nombre) {
  valor = parametros[[tolower(nombre)]]
  if (is.null(valor) || length(valor) == 0L) {
    return(NA_character_)
  }
  as.character(valor[[1L]])
}


inspeccionar_url_area = function(url, patron) {
  url_sin_fragmento = sub("#.*$", "", url)
  ruta = sub("[?].*$", "", url_sin_fragmento)
  ruta = utils::URLdecode(ruta)
  ruta = sub("^.*://[^/]*", "", ruta, perl = TRUE)

  es_area = grepl("(^|/)area/?$", ruta, ignore.case = TRUE, perl = TRUE)
  parametros = obtener_parametros_url(url)

  territorio = obtener_parametro(parametros, "territoryId")
  metodo = obtener_parametro(parametros, "spatialMethod")
  subtema = obtener_parametro(parametros, "subthemeKey")
  leyenda = obtener_parametro(parametros, "legendKey")
  anio_recurso = obtener_parametro(parametros, "year")

  coincide = es_area &&
    !is.na(territorio) && identical(territorio, patron) &&
    identical(metodo, "union") &&
    identical(subtema, "coverage_lclu") &&
    identical(leyenda, "default") &&
    !is.na(anio_recurso)

  list(
    es_area = es_area,
    coincide = coincide,
    territorio = territorio,
    anio = anio_recurso
  )
}


inspeccionar_url_patron_coverage = function(url, patron) {
  url_sin_fragmento = sub("#.*$", "", url)
  ruta = sub("[?].*$", "", url_sin_fragmento)
  ruta = utils::URLdecode(ruta)
  ruta = sub("^.*://[^/]*", "", ruta, perl = TRUE)
  ruta = sub("/+$", "", ruta)
  segmento_final = sub("^.*/", "", ruta)

  parametros = obtener_parametros_url(url)
  theme_key = obtener_parametro(parametros, "themeKey")
  ruta_patron = identical(segmento_final, patron)

  list(
    ruta_patron = ruta_patron,
    coincide = ruta_patron && identical(theme_key, "coverage"),
    theme_key = theme_key
  )
}


guardar_respuesta_json = function(sesion, request_id, archivo, recurso) {
  contenido = tryCatch(
    sesion$Network$getResponseBody(requestId = request_id),
    error = function(e) {
      stop(
        "No fue posible recuperar el cuerpo de la respuesta ", recurso, ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (is.null(contenido$body) || length(contenido$body) == 0L) {
    stop("El servidor devolvió una respuesta ", recurso, " vacía.")
  }

  cuerpo_raw = if (isTRUE(contenido$base64Encoded)) {
    jsonlite::base64_dec(contenido$body)
  } else {
    charToRaw(enc2utf8(as.character(contenido$body)))
  }

  texto_validacion = rawToChar(cuerpo_raw)
  Encoding(texto_validacion) = "UTF-8"
  json_valido = tryCatch(
    jsonlite::validate(texto_validacion),
    error = function(e) FALSE
  )

  if (!isTRUE(json_valido)) {
    stop(
      "La respuesta ", recurso,
      " no es JSON válido; no se guardó el archivo."
    )
  }

  writeBin(cuerpo_raw, archivo)
  message("JSON guardado: ", normalizePath(archivo, winslash = "/"))

  invisible(archivo)
}


# -----------------------------------------------------------------------------
# Inspección de red y descargas
# -----------------------------------------------------------------------------

capturar_mapbiomas = function(
    patron, anio_pagina, anio_area, carpeta, espera_segundos = 10
) {
  if (!is.character(patron) || length(patron) == 0L ||
      anyNA(patron) || any(!nzchar(patron)) ||
      any(grepl("[/\\]", patron))) {
    stop("'patron' debe ser un vector de identificadores válidos.")
  }

  if (length(anio_pagina) != 1L || is.na(anio_pagina) ||
      !is.finite(anio_pagina)) {
    stop("'anio_pagina' debe ser un año numérico válido.")
  }
  if (length(anio_area) != 1L || is.na(anio_area) || !is.finite(anio_area)) {
    stop("'anio_area' debe ser un año numérico válido.")
  }

  patrones = unique(patron)

  if (!dir.exists(carpeta)) {
    dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(carpeta)) {
    stop("No fue posible crear la carpeta de salida: ", carpeta)
  }

  navegador = chromote::Chromote$new()
  sesion = NULL
  on.exit({
    if (!is.null(sesion)) {
      try(sesion$close(), silent = TRUE)
    }
    try(navegador$close(wait = TRUE), silent = TRUE)
  }, add = TRUE)

  sesion = navegador$new_session()

  estado = new.env(parent = emptyenv())
  estado$patron_actual = NULL
  estado$solicitudes_fetch_xhr = list()
  estado$recursos_area = list()
  estado$recursos_patron_coverage = list()
  estado$finalizadas = character()
  estado$fallidas = character()

  # Se inspeccionan las respuestas reales Fetch/XHR recibidas al cargar cada
  # territorio. No se construye ni se supone la URL del recurso secundario.
  sesion$Network$responseReceived(function(msg) {
    tipo = if (is.null(msg$type)) "" else as.character(msg$type)
    if (!tolower(tipo) %in% c("fetch", "xhr")) {
      return(invisible(NULL))
    }

    id = as.character(msg$requestId)
    url_respuesta = as.character(msg$response$url)
    status = as.numeric(msg$response$status)
    mime = if (is.null(msg$response$mimeType)) {
      NA_character_
    } else {
      as.character(msg$response$mimeType)
    }

    recurso_area = inspeccionar_url_area(url_respuesta, estado$patron_actual)
    recurso_territorio = inspeccionar_url_patron_coverage(
      url_respuesta,
      estado$patron_actual
    )
    registro = list(
      url = url_respuesta,
      tipo = tipo,
      status = status,
      mime = mime,
      territorio = recurso_area$territorio,
      anio = recurso_area$anio,
      es_area = recurso_area$es_area,
      coincide_area = recurso_area$coincide,
      ruta_patron = recurso_territorio$ruta_patron,
      theme_key = recurso_territorio$theme_key,
      coincide_patron_coverage = recurso_territorio$coincide
    )

    estado$solicitudes_fetch_xhr[[id]] = registro
    if (recurso_area$es_area) {
      estado$recursos_area[[id]] = registro
    }
    if (recurso_territorio$ruta_patron) {
      estado$recursos_patron_coverage[[id]] = registro
    }

    invisible(NULL)
  })

  sesion$Network$loadingFinished(function(msg) {
    estado$finalizadas = unique(c(
      estado$finalizadas,
      as.character(msg$requestId)
    ))
    invisible(NULL)
  })

  sesion$Network$loadingFailed(function(msg) {
    estado$fallidas = unique(c(
      estado$fallidas,
      as.character(msg$requestId)
    ))
    invisible(NULL)
  })

  archivos_generados = character()

  for (patron_actual in patrones) {
    estado$patron_actual = patron_actual
    estado$solicitudes_fetch_xhr = list()
    estado$recursos_area = list()
    estado$recursos_patron_coverage = list()
    estado$finalizadas = character()
    estado$fallidas = character()

    url_pagina = crear_url_plataforma(patron_actual, anio_pagina)
    message("Abriendo MapBiomas para el territorio ", patron_actual, "...")
    sesion$go_to(url_pagina, delay = espera_segundos)

    message(
      "Solicitudes Fetch/XHR observadas: ",
      length(estado$solicitudes_fetch_xhr)
    )

    # Selecciona y guarda el recurso area correspondiente a 1985.
    if (length(estado$recursos_area) == 0L) {
      stop(
        "No se observó una respuesta Fetch/XHR con ruta 'area' para ",
        patron_actual, "."
      )
    }

    message("Recursos 'area' observados para ", patron_actual, ":")
    for (recurso in estado$recursos_area) {
      marca = if (isTRUE(recurso$coincide_area)) {
        "coincide con el territorio"
      } else {
        "no coincide con los filtros"
      }
      message(
        " - [", recurso$tipo, "] HTTP ", recurso$status,
        " | territorio=", recurso$territorio,
        " | año=", recurso$anio,
        " | ", marca,
        "\n   ", recurso$url
      )
    }

    coincidencias_area = Filter(
      function(recurso) isTRUE(recurso$coincide_area) &&
        !is.na(recurso$status) &&
        recurso$status >= 200 && recurso$status < 300,
      estado$recursos_area
    )
    ids_area_completos = setdiff(
      intersect(names(coincidencias_area), estado$finalizadas),
      estado$fallidas
    )
    coincidencias_area = coincidencias_area[ids_area_completos]

    ids_area_1985 = names(Filter(
      function(recurso) identical(
        recurso$anio,
        as.character(as.integer(anio_area))
      ),
      coincidencias_area
    ))

    if (length(ids_area_1985) == 0L) {
      stop(
        "No se encontró una respuesta 'area' completada para ", patron_actual,
        " con spatialMethod=union, subthemeKey=coverage_lclu, ",
        "legendKey=default y year=", anio_area, "."
      )
    }

    id_area = ids_area_1985[[1L]]
    recurso_area_elegido = coincidencias_area[[id_area]]
    message("Recurso 'area' seleccionado:")
    message(recurso_area_elegido$url)

    archivo_area = file.path(carpeta, paste0(patron_actual, ".json"))
    guardar_respuesta_json(
      sesion,
      id_area,
      archivo_area,
      "area"
    )
    archivos_generados = c(archivos_generados, archivo_area)

    # Selecciona el recurso real cuya ruta termina en patron y lleva
    # ?themeKey=coverage; guarda la respuesta del servidor sin reserializarla.
    if (length(estado$recursos_patron_coverage) == 0L) {
      stop(
        "No se observó una solicitud cuya ruta termine en ", patron_actual,
        " para el recurso themeKey=coverage."
      )
    }

    message(
      "Solicitudes con ruta de territorio y themeKey=coverage observadas:"
    )
    for (recurso in estado$recursos_patron_coverage) {
      marca = if (isTRUE(recurso$coincide_patron_coverage)) {
        "coincide"
      } else {
        "no coincide con themeKey=coverage"
      }
      message(
        " - [", recurso$tipo, "] HTTP ", recurso$status,
        " | ", marca,
        "\n   ", recurso$url
      )
    }

    coincidencias_territorio = Filter(
      function(recurso) isTRUE(recurso$coincide_patron_coverage) &&
        !is.na(recurso$status) &&
        recurso$status >= 200 && recurso$status < 300,
      estado$recursos_patron_coverage
    )
    ids_territorio_completos = setdiff(
      intersect(names(coincidencias_territorio), estado$finalizadas),
      estado$fallidas
    )
    coincidencias_territorio = coincidencias_territorio[
      ids_territorio_completos
    ]

    if (length(coincidencias_territorio) == 0L) {
      stop(
        "No se encontró una respuesta exitosa y completada para la ruta ",
        patron_actual, "?themeKey=coverage."
      )
    }

    id_territorio = names(coincidencias_territorio)[[1L]]
    recurso_territorio_elegido = coincidencias_territorio[[id_territorio]]
    message("Recurso del territorio seleccionado:")
    message(recurso_territorio_elegido$url)

    archivo_territorio = file.path(
      carpeta,
      paste0(patron_actual, " información.json")
    )
    guardar_respuesta_json(
      sesion,
      id_territorio,
      archivo_territorio,
      "del territorio"
    )
    archivos_generados = c(archivos_generados, archivo_territorio)
  }

  message("Proceso finalizado. Archivos generados: ", length(archivos_generados))
  invisible(archivos_generados)
}


# -----------------------------------------------------------------------------
# Ejecución
# -----------------------------------------------------------------------------

archivo_generado = capturar_mapbiomas(
  patron = patron,
  anio_pagina = anio_pagina,
  anio_area = anio_area,
  carpeta = carpeta,
  espera_segundos = espera_segundos
)
