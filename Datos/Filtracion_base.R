datos = readxl::read_excel(path = "Datos/Originales/Poblacion Total.xlsx", sheet = 3)

names(x = datos)
names(datos)[1:4]
columna5 = datos[5,]
columna5[1:4]


names(datos)[1:4] = columna5[1:4]
names(datos)

columna6 = datos[6,]
names(datos)[5:ncol(datos)] = columna6[5:ncol(datos)]
names(datos)

datos$`Entidad federativa` |>  unique()
datos$Sexo |>  unique()
datos = datos |>
  dplyr::filter(Sexo == "Total",
                `Grupos quinquenales de edad` == "Total")

datos = datos |> 
  dplyr::select(`Entidad federativa`,Sexo,`Grupos quinquenales de edad`,`Población total1`)

datos_cortados = datos |> 
  dplyr::select(`Entidad federativa`,`Población total1`)

datos_cortados = datos_cortados[-1,]

datos_cortados = datos_cortados |> 
  dplyr::mutate(`Entidad federativa` = sub(x = `Entidad federativa`, pattern = "^.*? ", replacement = ""))





geometria = sf::read_sf("Datos/Geometria del pais/00ent.shp")
plot(geometria$geometry)


datos_unir = datos[-1,]
datos_unir = datos_unir |> 
  dplyr::mutate(clave = sub(x = `Entidad federativa`, pattern = " .*$", replacement = "",),
                `Entidad federativa` = sub(x = `Entidad federativa`, pattern = "^.*? ", replacement = ""))

datos_unir = datos_unir |> 
  dplyr::select(clave,`Entidad federativa`,`Población total1`)

geometria = geometria |> 
  dplyr::select(-CVEGEO) 


unir = merge(x = datos_unir, y = geometria, by.x = "clave", by.y = "CVE_ENT", all.x = T)

unir = unir |> 
  dplyr::select(-NOMGEO)

sf::st_crs(geometria)

unir = sf::st_as_sf(x = unir, crs = sf::st_crs(geometria)) #
unir = sf::st_transform(x = unir, crs = 4326) # Es una transformacion lineal, es una reproyeccion


paste("Hola", "Mundo")

library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
install.packages("leaflet.extras")
install.packages("RColorBrewer")

unir$`Población total1` = as.numeric(unir$`Población total1`)

unir = unir |> 
  dplyr::mutate(`Población total1` = as.numeric(`Población total1`))

paletas = colorNumeric(
  palette = "YlGnBu",
  domain = unir$`Población total1`,
  reverse = T
)


mapa_web = leaflet() |> 
  addTiles() |> 
  addPolygons(data = unir, label =  unir$`Entidad federativa`, 
              popup = paste("Clave Municipal:",  "<b>", unir$clave, "</b>", "<br>",
                            "Nombre del Municipio:",  "<b>", unir$`Entidad federativa`, "</b>",  "<br>",
                            "Poblacion total:", "<b>", unir$`Población total1`, "</b>"),
              color = paletas(x = unir$`Población total1`),
              group = "nacional") |> 
  addLegend(position = "bottomright", pal = paletas, title = "Simbologia Tilines", values = unir$`Población total1`) |>
  addSearchFeatures(targetGroups = "nacional",
                    options = searchFeaturesOptions(
                      zoom = 12,
                      position = "topleft"
                    ))


mapa_web



library(htmlwidgets)
htmlwidgets::saveWidget(mapa_web, "Datos/Primer_mapa.html")




library(shiny)
