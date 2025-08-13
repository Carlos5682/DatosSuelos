rm(list=ls())
library(ggrepel)
library(ggspatial)
library(mapSpain)
library(sf)
library(dplyr)
library(ggplot2)
library(future.apply)
setwd("C://Users//samue//Desktop//ProyectoTFG/DatosSuelos/")


#---------------------------------------------1 Recortar Comunidades autonomas--------------------------------------------

##---------------------------------------1.1 Procesar capas-----------------------------------

#El zip descargado de: https://centrodedescargas.cnig.es/CentroDescargas/detalleArchivo?sec=11262857
#Contiene tanto peninsula, baleares y canarias en una sola capa.


####---------------------1.1.1 Leer capa--------------------------

suelos <- st_read("Datos/Datosdescargados/mesuelos1M.shp_v3/suelos_1m_v3.shp") 

####--------------------1.1.2 Corregir geometrias----------------
suelos <- st_cast(suelos, "MULTIPOLYGON")
suelos <- st_make_valid(suelos)

####----------------1.1.3 Disolver geometrias----------------

suelosdisueltos <- suelos %>%
  group_by(orden) %>%
  summarise(geom = st_union(geometry), .groups = "drop") 

####---------------1.1.4 Reproyectar geometrías--------------

suelosreproy <-  st_transform(suelosdisueltos, 3035)

suelosreproy$orden <- paste0(
  toupper(substr(suelosreproy$orden, 1, 1)),
  tolower(substr(suelosreproy$orden, 2, nchar(suelosreproy$orden)))
)


####------------------1.1.5 Personalizar leyenda------------------
#Para los colores de la leyenda se tomó el color del suborden mayoritario del mapa de suelos de españa desarrollado por el IGN 

leyenda_suelos <- data.frame(
  orden = c(
    "Alfisol",
    "Andisol",
    "Aridisol",
    "Entisol",
    "Histosol",
    "Inceptisol",
    "Mollisol", 
    "Spodosol", 
    "Ultisol", 
    "Vertisol"
    
  ),
  color = c(
    "#CFA565",  
    "#289FD5",
    "#F39F86",
    "#ACD19C",
    "#8D9686",
    "#FBF6BA",
    "#F098A5",
    "#A8B8BE",
    "#8498C9",
    "#DFCCE2"
  ),
  stringsAsFactors = FALSE
)


suelosfinal <- left_join(suelosreproy, leyenda_suelos, by = c("orden" = "orden"))

####------------------1.1.6 Guardar capa final-------------------

st_write(suelosfinal, 
         dsn = "Datos/Datoscorregidos/Sueloscorregido/sueloscorregido.gpkg", 
         driver = "GPKG", 
         delete_layer = TRUE)


##---------------------------------------1.2 Bucle para las comunidades autonomas------------------------------------

suelosfinal <- st_read("Datos/Datoscorregidos/Sueloscorregido/sueloscorregido.gpkg")

####-------------1.2.1 Obtener comunidades autónomas---------
ccaa_sf <- esp_get_ccaa(moveCAN = FALSE, epsg = 3035)
ccaa_sf <- st_make_valid(ccaa_sf)

for (i in 1:nrow(ccaa_sf)) {
  nombre_ccaa <- ccaa_sf$ccaa.shortname.es[i]
  codigo_ccaa <- ccaa_sf$codauto[i]
  
  cat("Procesando:", nombre_ccaa, "\n")
  
  ccaa_geom <- ccaa_sf[i, ]
  
  ####----------1.2.2 Calcular y expandir bbox---------------
  bbox <- st_bbox(ccaa_geom)
  
  x_diff <- bbox$xmax - bbox$xmin
  y_diff <- bbox$ymax - bbox$ymin
  max_diff <- max(x_diff, y_diff)
  
  x_buffer <- max_diff * 0.15
  y_buffer <- max_diff * 0.15
  
  bbox_expandida <- structure(
    c(
      xmin = bbox$xmin - x_buffer,
      ymin = bbox$ymin - y_buffer,
      xmax = bbox$xmax + x_buffer,
      ymax = bbox$ymax + y_buffer
    ),
    class = "bbox",
    crs = st_crs(ccaa_geom)
  )
  
  ####----------1.2.3 Convertir bbox a polígono-------------
  bbox_poly <- st_as_sfc(st_bbox(bbox_expandida, crs = st_crs(ccaa_geom)))
  
  ####----------1.2.4 Intersección con polígono--------------
  suelos_crop <- st_intersection(suelosfinal, bbox_poly)
  
  ####------------1.2.5 Guardar como GeoPackage---------------
  st_write(suelos_crop, paste0("Datos/DatosporComunidad/Suelos/", nombre_ccaa, ".gpkg"), delete_dsn = TRUE)
}



#---------------------------------------2 Recortar municipios-----------------------------------
##-----------------------------2.1 Configurar paralelización--------------------
###-------------------2.1.1 Maximo de memoria----------------
options(future.globals.maxSize = 2 * 1024^3)  

###-------------------2.1.2 Numero de nucleos-------------------
parallel::detectCores() #Detectamos cuantos nucleos tenemos
future::availableCores() #Cuantos nucleos estan disponibles para ser usaros

plan(multisession, workers = 12)

##-----------------------------2.2 Obtener municipios-----------------------
municipios <- esp_get_munic(moveCAN = FALSE, epsg = 3035)
CCAA_sf <- esp_get_ccaa(moveCAN = FALSE, epsg = 3035)

##----------------------------2.3 Comunidades a paralelizar------------------
comunidades <- unique(CCAA_sf$ccaa.shortname.es)

##-----------------------------2.4 Función a paralelizar--------------------------

procesar_comunidad <- function(comunidad_objetivo) {
  cat("▶ Procesando comunidad:", comunidad_objetivo, "\n")
  
  ###------------------------2.4.1 Codigo de comunidad------------------------
  cod_comunidad <- CCAA_sf %>%
    filter(ccaa.shortname.es == comunidad_objetivo) %>%
    pull(codauto)
  
  ###------------------------2.4.2 Muncipios de comunidad---------------------
  municipios_comunidad <- municipios %>%
    filter(codauto == cod_comunidad) %>%
    st_make_valid()
  
  ###-----------------------2.4.3 Ruta de litologia por comunidad---------------
  
  lit_path <- paste0("Datos/DatosporComunidad/Suelos/", comunidad_objetivo, ".gpkg")
  dir_out <- paste0("Capasfinales/Suelos/", comunidad_objetivo)
  
  if (!file.exists(lit_path)) {
    return(paste0("⚠ Archivo suelos no encontrado para ", comunidad_objetivo))
  }
  
  dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
  
  ###-----------------------2.4.4 Leer suelos---------------------------------
  lit_ccaa <- st_read(lit_path, quiet = TRUE)
  
  resultados <- vector("list", nrow(municipios_comunidad))
  ###-------------------------2.4.5 Bucle--------------------------------------
  for (i in seq_len(nrow(municipios_comunidad))) {
    muni <- municipios_comunidad[i, ]
    nombre_muni <- gsub(" ", "_", gsub("/", "o", muni$name))
    output_path <- file.path(dir_out, paste0(nombre_muni, ".geojson"))
    
    ####--------------------2.4.5.1 Mantener municipios ya obtenidos------------    
    if (file.exists(output_path)) {
      resultados[[i]] <- paste0("✔ Ya existe: ", nombre_muni)
      next
    }
    
    tryCatch({
      
      ###------------------2.4.5.2 Calcular y expandir bbox-----------------
      bbox <- st_bbox(muni)
      x_diff <- bbox$xmax - bbox$xmin
      y_diff <- bbox$ymax - bbox$ymin
      max_diff <- max(x_diff, y_diff)
      
      x_buffer <- max_diff * 0.15
      y_buffer <- max_diff * 0.15
      
      bbox_expandido <- structure(
        c(
          xmin = bbox$xmin - x_buffer,
          ymin = bbox$ymin - y_buffer,
          xmax = bbox$xmax + x_buffer,
          ymax = bbox$ymax + y_buffer
        ),
        class = "bbox",
        crs = st_crs(muni)
      )
      
      ###------------------2.4.5.3 Convertir bbox a polígono--------------
      bbox_poly <- st_as_sfc(st_bbox(bbox_expandido, crs = st_crs(muni)))
      
      ###------------------2.4.5.4 Intersección con poligono--------------
      recorte <- st_intersection(lit_ccaa, bbox_poly)
      
      ###------------------2.4.5.5 Detector de error---------------------
      
      if (nrow(recorte) > 0) {
        st_write(recorte, output_path, delete_layer = TRUE, quiet = TRUE)
        resultados[[i]] <- paste0("💾 Guardado: ", nombre_muni)
      } else {
        st_write(muni[0, ], output_path, delete_layer = TRUE, quiet = TRUE)  # Archivo vacío
        resultados[[i]] <- paste0("⚠ Vacío (guardado): ", nombre_muni)
      }
    }, error = function(e) {
      resultados[[i]] <- paste0("❌ Error en ", nombre_muni, ": ", e$message)
    })
  }
  
  return(resultados)
}

##----------------------------2.5 Ejecutar en paralelo-------------------
resultados_totales <- future_lapply(comunidades, procesar_comunidad)
##----------------------------2.6 Resumen final--------------------------
cat("\n✅ Procesamiento completado.\n")
