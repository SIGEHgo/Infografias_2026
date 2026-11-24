#### Propuesta de trabajo ####
### Definimos grupos de variables. Por ejemplo, 
  ### Hoja 1 - Información Geográfica,
  ### Hoja 1 - Información Demográfica,
  ### Hoja 1 - Información Migratoria

### Al inicio, se consumen las fuentes de información. Propongo que se utilice el BUIE/BUIG

#### Hoja 1 - Información Geográfica ####

#source("../../Reutilizables/Postgres_BUIG/conexion_buie.R") ##Contraseña va a cambiar cada mes
source("../../Reutilizables/Postgres_BUIG/conexion_buig.R") ##No durará para siempre

cartografia_13_mun_2025=st_read(buig,"cartografia_13_mun_2025")
cartografia_13_loc_2025=st_read(buig,"cartografia_13_loc_2025")
eic_2025_h=DBI::dbReadTable(conn = buig,name = "encuesta_intercensal_2025")
eic_2025_h=eic_2025_h |> 
  dplyr::mutate(CVEGEO=substr(CVEGEO,1,5))
dicc_eic_2025=get_col_descriptions(connection = buig,table_name = "encuesta_intercensal_2025")
micro_eic_2025_h=dplyr::tbl(src = buig,"microdatos_encuesta_intercensal_2025")
nacimientos_2025=DBI::dbReadTable(conn = buig,name = "estadistica_nacimientos_2025")
regiones=st_read(buig,"regionalizacion")

hoja_1=list()
hoja_1[['info_geo']]=cartografia_13_mun_2025 |> 
  dplyr::select(CVEGEO,NOMGEO) |> 
  st_drop_geometry() |> 
  dplyr::left_join(y = regiones |> dplyr::select(nom_mun,region) |> st_drop_geometry(),
                   by=dplyr::join_by(NOMGEO==nom_mun))
### dicc_eic_2025 contiene todas las variables disponibles de la EIC. 
### Agregamos las variables
##### superficie ######
hoja_1[['info_geo']]=hoja_1[['info_geo']] |> 
  dplyr::left_join(
    y=cartografia_13_mun_2025 |> 
      dplyr::select(CVEGEO,geometry) |> 
      dplyr::mutate(superficie_km2=round(as.numeric(sf::st_area(geometry))/(1000**2) ,2)) |> 
      st_drop_geometry(),
    by=dplyr::join_by(CVEGEO)
  )
##### densidad ######
hoja_1[['info_geo']]=hoja_1[['info_geo']] |> 
  dplyr::left_join(
    cartografia_13_mun_2025 |> 
      dplyr::select(CVEGEO,geometry) |> 
      dplyr::mutate(superficie_km2=round(as.numeric(sf::st_area(geometry))/(1000**2) ,2)) |> 
      st_drop_geometry() |> 
      dplyr::left_join(y = eic_2025_h |> dplyr::select(CVEGEO,POBTOT) ,by=dplyr::join_by(CVEGEO)) |> 
      dplyr::mutate(densidad_hab_km2=round(POBTOT/superficie_km2,2)) |> 
      dplyr::select(CVEGEO,densidad_hab_km2,POBTOT),
    by=dplyr::join_by(CVEGEO)
  )
##### localidades ######
hoja_1[['info_geo']]=hoja_1[['info_geo']] |> 
  dplyr::left_join(
    cartografia_13_loc_2025 |> 
      st_drop_geometry() |> 
      dplyr::mutate(CVEGEO=substr(CVEGEO,1,5)) |> 
      dplyr::group_by(CVEGEO,AMBITO) |> 
      dplyr::count() |> 
      tidyr::pivot_wider(names_from = AMBITO,names_prefix = "locs_",values_from = n),
    by=dplyr::join_by(CVEGEO)
  )
##### población ######
POBTOT_HGO=eic_2025_h$POBTOT |> sum()
hoja_1[['info_geo']]=hoja_1[['info_geo']] |> 
  dplyr::left_join(
    micro_eic_2025_h |> 
      dplyr::select(CVEGEO,CVE_MUN,LOC50K,FACTOR,TAMLOC) |> 
      dplyr::mutate(TAMLOC_2500=ifelse(TAMLOC==1,'rural','urbana')) |> 
      dplyr::group_by(CVEGEO,TAMLOC_2500) |> 
      dplyr::summarise(FACTOR=sum(FACTOR)) |> 
      tidyr::pivot_wider(names_from = TAMLOC_2500,names_prefix = "pob_",
                         values_from = FACTOR,values_fill = 0) |>
      dplyr::collect() |> 
      dplyr::mutate(CVEGEO=as.character(CVEGEO)), 
    by=dplyr::join_by(CVEGEO)
  ) |> 
  dplyr::mutate(porcentaje_POBTOT=paste0(round(100*POBTOT/POBTOT_HGO,2),"%"),
                porcentaje_pob_rural= paste0(round(100*pob_rural/POBTOT,2),"%"),
                porcentaje_pob_urbana=paste0((100-round(100*pob_rural/POBTOT,2)),"%")## Para que siempre sume 100
                )
#### Hoja 1 - Indicadores demográficos #### 
hoja_1[['info_demo']]=hoja_1[['info_geo']] |> 
  dplyr::select(CVEGEO)
##### Tasa de natalidad #####
### Fórmula:
  ### 1000* Num_nacimientos en un año determinado / tot_pob(hombres y mujeres) (por cada 1000 habitantes)
micro_eic_2025_h |> ###Usar nacimientos_2025 
  dplyr::select(CVEGEO,HIJOS_NAC_VIVOS,FACTOR,FECHA_NAC_A) |> 
  dplyr::filter(FECHA_NAC_A==2025) |> ## Proxy. Faltan nacimientos de octubre, noviembre y diciembre
  dplyr::group_by(CVEGEO) |> 
  dplyr::summarise(HIJOS_NAC_VIVOS=sum(HIJOS_NAC_VIVOS*FACTOR)) |> 
  dplyr::collect() |> 
  dplyr::mutate(CVEGEO=as.character(CVEGEO)) |> 
  dplyr::left_join(
    y=eic_2025_h |> dplyr::select(CVEGEO,POBTOT),
    by=dplyr::join_by(CVEGEO)
  ) |> 
  dplyr::mutate(tasa_natalidad=1000*HIJOS_NAC_VIVOS/POBTOT)
##### Tasa de fecundidad #####
### Fórmula:
### 1000* Num_nacimientos / tot_pob(solo mujeres en edad fertil) (por cada 1000 habitantes(mujeres))


### Hoja 1 - Información Migratoria ----
hoja_1[['info_migratoria']]=cartografia_13_mun_2025 |> 
  dplyr::select(CVEGEO,NOMGEO) |> 
  st_drop_geometry() |> 
  dplyr::left_join(y = regiones |> dplyr::select(nom_mun,region) |> st_drop_geometry(),
                   by=dplyr::join_by(NOMGEO==nom_mun))

hoja_1[['info_migratoria']]=hoja_1[['info_migratoria']] |> 
  dplyr::left_join(
    micro_eic_2025_h |> 
      dplyr::select(CVEGEO,ENT_PAIS_RES_5A,MUN_RES_5A,CAUSA_MIG_C,FACTOR) |> 
      dplyr::group_by(CVEGEO,ENT_PAIS_RES_5A) |> 
      dplyr::summarise(FACTOR=sum(FACTOR)) |> 
      dplyr::collect() |> 
      dplyr::mutate(CVEGEO=as.character(CVEGEO)) |> 
      dplyr::ungroup() |> 
      dplyr::filter(!is.na(ENT_PAIS_RES_5A) & (ENT_PAIS_RES_5A)!=13 & ENT_PAIS_RES_5A<900) |> 
      dplyr::group_by(CVEGEO) |> 
      dplyr::summarise(POB_NAC_OTRO_PAIS=sum(FACTOR,na.rm = T)),
    by=dplyr::join_by(CVEGEO)
  )
hoja_1[['info_migratoria']]=hoja_1[['info_migratoria']] |> 
  dplyr::left_join(
    hoja_1[['info_geo']] |> dplyr::select(CVEGEO,POBTOT),
    by=dplyr::join_by(CVEGEO)
  ) |> 
  dplyr::mutate(POBTOT=round(100*POB_NAC_OTRO_PAIS/POBTOT,5) ) |> 
  dplyr::rename(PORC_POB_NAC_OTRO_PAIS=POBTOT)
hoja_1[['info_migratoria']]=hoja_1[['info_migratoria']] |> 
  dplyr::left_join(
    micro_eic_2025_h |> 
      dplyr::select(CVEGEO,ENT_PAIS_RES_5A,MUN_RES_5A,CAUSA_MIG_C,FACTOR) |> 
      dplyr::group_by(CVEGEO,ENT_PAIS_RES_5A) |> 
      dplyr::summarise(FACTOR=sum(FACTOR)) |> 
      dplyr::collect() |> 
      dplyr::mutate(CVEGEO=as.character(CVEGEO)) |> 
      dplyr::ungroup() |> 
      dplyr::filter(!is.na(ENT_PAIS_RES_5A) & 
                      (ENT_PAIS_RES_5A)!=13 & 
                      ENT_PAIS_RES_5A<900) |> 
      dplyr::group_by(CVEGEO) |> 
      dplyr::arrange(dplyr::desc(FACTOR)) |> 
      dplyr::slice_head(n=3) |> dplyr::group_by(CVEGEO) |> 
      dplyr::mutate(row_number=dplyr::row_number())|> 
      dplyr::rename(VALOR=FACTOR) |> 
      tidyr::pivot_wider(id_cols = CVEGEO,
                         names_from = row_number,
                         values_from = c(ENT_PAIS_RES_5A,VALOR),##Falta renombrar los códigos por el nombre de la entidad
                         names_glue = "PRINCIPAL_ENTIDAD_{.name}"), 
    by=dplyr::join_by(CVEGEO)
  )


