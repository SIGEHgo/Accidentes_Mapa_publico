archivos = list.files(path = "Input/", pattern = "\\.shp$", recursive = T, all.files = T)
nombres = archivos |> sub(pattern = "/.*", replacement = "") |>  stringr::str_squish()  |>  gsub(pattern = "_shp", replacement = "") |>   stringr::str_squish()


for (i in seq_along(archivos)) {
  cat("Vamos en: ", nombres[i], " se esta abriendo ", archivos[i], "\n")
  datos = paste0("Input/", archivos[i]) |>  sf::read_sf() |>  dplyr::filter(EDO == 13) 
  source("Codigos/R/Filtracion_Inegi.R")
  datos |>  sf::write_sf( paste0("Output/", nombres[i], ".geojson"))
}
