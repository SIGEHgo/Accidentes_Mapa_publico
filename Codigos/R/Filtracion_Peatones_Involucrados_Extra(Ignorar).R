datos$vehi_invol = apply(datos |> sf::st_drop_geometry() |> dplyr::select(AUTOMOVIL:BICICLETA),MARGIN = 1,FUN=function(row){
  names_reng=names(row)
  cadena_final=''
  for(id in 1:12){
    val=row[id]
    if(val!=0){
      cadena_final=paste0(cadena_final,val," ",names_reng[id]," ")
    }
  }
  return(cadena_final)
})

datos = datos |> 
  dplyr::mutate(
    MPIO = MPIO |>  as.numeric()
  )


datos = datos |> 
  dplyr::mutate(
    TIPACCID = TIPACCID |> as.character(),
    TIPACCID = dplyr::case_when(
      TIPACCID == "0" ~ "Certificado cero",
      TIPACCID == "1" ~ "Colisión con vehículo automotor",
      TIPACCID == "2" ~ "Colisión con peatón (atropellamiento)",
      TIPACCID == "3" ~ "Colisión con animal",
      TIPACCID == "4" ~ "Colisión con objeto fijo",
      TIPACCID == "5" ~ "Volcadura",
      TIPACCID == "6" ~ "Caída de pasajero",
      TIPACCID == "7" ~ "Salida del camino",
      TIPACCID == "8" ~ "Incendio",
      TIPACCID == "9" ~ "Colisión con ferrocarril",
      TIPACCID == "10" ~ "Colisión con motocicleta",
      TIPACCID == "11" ~ "Colisión con ciclista",
      TIPACCID == "12" ~ "Otro",
      T ~ TIPACCID
    ),
    TIPACCID = TIPACCID |> stringr::str_squish()
  )


datos = datos |> 
  dplyr::mutate(
    CLASE = CLASE |> as.character(),
    CLASE = dplyr::case_when(
      CLASE == "1" ~ "Fatal",
      CLASE == "2" ~ "No fatal",
      CLASE == "3" ~ "Sólo daños",
      TRUE ~ CLASE
    ),
    CLASE = CLASE |>  stringr::str_squish()
  )


datos = datos |> 
  dplyr::mutate(
    EDO = EDO |>  as.character(),
    EDO = dplyr::case_when(
      EDO == "13" ~ "Hidalgo",
      TRUE ~ EDO
    ),
    EDO = EDO |>  stringr::str_squish()
  )


datos = datos |> 
  dplyr::mutate(
    SEXO = SEXO |>  as.character(),
    SEXO = dplyr::case_when(
      SEXO == "1" ~ "Se fugó",
      SEXO == "2" ~ "Hombre",
      SEXO == "3" ~ "Mujer",
      TRUE ~ SEXO
    ),
    SEXO = SEXO |>  stringr::str_squish()
  )


datos = datos |> 
  dplyr::mutate(
    CAUSAACCI = CAUSAACCI |>  as.character(),
    CAUSAACCI = dplyr::case_when(
      CAUSAACCI == "1" ~ "Conductor",
      CAUSAACCI == "2" ~ "Peatón o pasajero",
      CAUSAACCI == "3" ~ "Falla del vehículo",
      CAUSAACCI == "4" ~ "Mala condición del camino",
      CAUSAACCI == "5" ~ "Otra",
      TRUE ~ CAUSAACCI
    ),
    CAUSAACCI = CAUSAACCI |>  stringr::str_squish()
  )


datos = datos |> 
  dplyr::mutate(
    DIASEMANA = DIASEMANA |>  as.character(),
    DIASEMANA = dplyr::case_when(
      DIASEMANA == "1" ~ "Lunes",
      DIASEMANA == "2" ~ "Martes",
      DIASEMANA == "3" ~ "Miércoles",
      DIASEMANA == "4" ~ "Jueves",
      DIASEMANA == "5" ~ "Viernes",
      DIASEMANA == "6" ~ "Sábado",
      DIASEMANA == "7" ~ "Domingo",
      TRUE ~ DIASEMANA
    ),
    DIASEMANA = DIASEMANA |> stringr::str_squish()
  )




mun = "../../Importantes_documentos_usar/Municipios/municipiosjair.shp" |> sf::read_sf() |> 
  dplyr::select(CVE_MUN, NOM_MUN) |> sf::st_drop_geometry() |> 
  dplyr::mutate(CVE_MUN = CVE_MUN |>  as.numeric())

datos = datos |> 
  dplyr::left_join(y = mun, by = c("MPIO" = "CVE_MUN")) |> 
  dplyr::arrange(ANIO, MES, DIA, HORA, MINUTOS)



datos = datos |> 
  dplyr::rename(
    TOT_MUERT = TOTMUERTOS,
    TOT_HER = TOTHERIDOS,
    CONDMUE = CONDMUERTO,
    CONDHER = CONDHERIDO,
    PEATMUE = PEATMUERTO,
    PEATHER = PEATHERIDO
  )


datos = datos |> 
  dplyr::select(TIPACCID, CLASE, ANIO, MES, DIA, HORA, MINUTOS, EDO, NOM_MUN, SEXO, EDAD, CAUSAACCI, 
                DIASEMANA, vehi_invol, TOT_MUERT, TOT_HER, CONDMUE, CONDHER, PEATMUE, PEATHER)
