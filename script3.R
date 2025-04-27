

##################################### COPERNICUS MARINE ################################################################



# Chargement de reticulate
library(reticulate)

pas_de_temps="heure" # ou quart_heure

# 👉 Chemin vers le Python configuré dans GitHub Actions
use_python("/opt/hostedtoolcache/Python/3.10.17/x64/bin/python", required = TRUE)

# Ensuite on importe le module copernicusmarine
cmt <- import("copernicusmarine")

#  Récupération des identifiants depuis les variables d'environnement GitHub Actions
user <- Sys.getenv("CMEMS_USER")
pwd  <- Sys.getenv("CMEMS_PWD")


# Se connecter à Copernicus Marine
cmt$login(user, pwd)

#  Créer un dossier pour recevoir les fichiers
dir.create("data_maree", showWarnings = FALSE)

# les dates à telecharger
date_min<-format(Sys.Date()-6,"%Y-%m-%dT00:00:00")
date_max<-format(Sys.Date()+10,"%Y-%m-%dT23:00:00")

if (pas_de_temps=='heure') {data_id<-"cmems_mod_ibi_phy_anfc_0.027deg-2D_PT1H-m" } else 
 {data_id<- "cmems_mod_ibi_phy_anfc_0.027deg-2D_PT15M-i" } 

# 📥 Téléchargement des données
d<-cmt$subset(
  dataset_id = data_id,
  #variables = list("mlotst", "thetao", "ubar", "uo", "vbar", "vo", "zos"),
  variables = list("zos"),
  minimum_longitude=-5.6566816,
  maximum_longitude=0.5569,
  minimum_latitude=42.2901,
  maximum_latitude=49.8846,
  start_datetime = date_min,
  end_datetime   = date_max,
  output_directory = "./data_maree"
)

cat("✅ Données téléchargées dans le dossier /data_maree\n")



library(terra)

spots<-data.frame(
  id=c("aytre","saint_trojan","hossegor"),
  lat=c(46.1188,45.826,43.6654),
  lon=c(-1.130,-1.2486,-1.4429)
)
# on s'assure qu'on est sur mer et pas sur terre
spots$lon<-spots$lon-0.05


# on cherche le fichier le plus récent
fichs<-list.files("./data_maree")

if (length(fichs) == 0) stop("❌ Aucun fichier NC trouvé dans /data_maree")

ctime<-file.info(paste0("./data_maree/",fichs))$ctime
fich<-fichs[ctime==max(ctime)]

# Charger la variable 'zos' comme SpatRaster
# variable zos : sea_surface_height_above_geoid
zos_stack <- rast(paste0("./data_maree/",fich), subds = "zos")

# Récupérer les coordonnées
min_lon <- d$coordinates_extent[[1]]$minimum
max_lon <- d$coordinates_extent[[1]]$maximum
min_lat <- d$coordinates_extent[[2]]$minimum
max_lat <- d$coordinates_extent[[2]]$maximum

# Forcer le CRS
crs(zos_stack) <- "EPSG:4326"

# Forcer l'extent
terra::ext(zos_stack) <- c(min_lon, max_lon, min_lat, max_lat)


#construire la date
datemin <- as.POSIXct(d$coordinates_extent[[3]]$minimum, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
datemax <-  as.POSIXct(d$coordinates_extent[[3]]$maximum, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")


# Extraire les valeurs de chaque couche (temps) pour chaque point
valeurs <- terra::extract(zos_stack, spots[, c("lon", "lat")])

# Ajouter l'identifiant pour retrouver à qui appartiennent les données
valeurs$spot <- spots$id[valeurs$ID]

library(tidyr)
#  pivot_longer(
#    cols = -c(spot,ID), names_to = c("heure", "mesure"),
#    names_sep = "[^[:alnum:]]+", values_to = "maree")
#data_long$date<-rep(time(zos_stack),3)

data_long <- valeurs %>%
  pivot_longer(
    cols = -c(spot,ID), names_to = c("date_UTC"),
    values_to = "maree")
if (pas_de_temps=="heure") {
  data_long$date_UTC<-rep(seq(datemin,datemax,by=60*60),3)} else {
 data_long$date_UTC<-rep(seq(datemin,datemax,by=60*15),3)
 }
data_long$date_paris<- format(data_long$date_UTC, tz = "Europe/Paris", usetz = TRUE)

data_long$jour<-substr(data_long$date_paris,1,10)
data_long$heure<-substr(data_long$date_paris,12,13)
data_long$minute<-substr(data_long$date_paris,15,16)

#exporte le csv
write.csv(data_long,"zos_points.csv",row.names=FALSE)





##################################### ECMWF ################################################################

#' url_exists
#'
#' @param x a single URL
#' @param non_2xx_return_value what to do if the site exists but the
#'        HTTP status code is not in the `2xx` range. Default is to return `FALSE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value` condition
#'        arises a warning message will be displayed. Default is `FALSE`.
#' @param ... other params (`timeout()` would be a good one) passed directly
#'        to `httr::HEAD()` and/or `httr::GET()`
#' @return
#' @export
#' @examples
#' \dontrun{
#' c("http://google.com","http://atmo-na.org") -> some_urls
#' data.frame(
#'  exists = sapply(some_urls, url_exists, USE.NAMES = FALSE),
#'  some_urls,
#'  stringsAsFactors = FALSE
#' ) %>% dplyr::tbl_df() %>% print()
#' }
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...)
{
  
  suppressPackageStartupMessages({
    require("httr", quietly = FALSE, warn.conflicts = FALSE)
  })
  
  # you don't need thse two functions if you're alread using `purrr`
  # but `purrr` is a heavyweight compiled pacakge that introduces
  # many other "tidyverse" dependencies and this doesnt.
  
  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)
        
        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }
  
  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }
  
  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)
  
  # Try HEAD first since it's lightweight
  #res <- sHEAD(x, ...)
  res <-sHEAD(x, timeout(10))
  
  if (is.null(res$result) ||
      ((httr::status_code(res$result) %/% 200) != 1)) {
    
    res <- sGET(x, timeout(10))
    
    if (is.null(res$result)) return(NA) # or whatever you want to return on "hard" errors
    
    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }
    
    return(TRUE)
    
  } else {
    return(TRUE)
  }
  
}




library(httr)
library(rvest)
library(jsonlite)

source("ecmwf_app.R")

cat("✅ Le fichier ecmwf_app.R a été chargé directement depuis le dépôt cloné\n")










jour_ech<-format(Sys.Date(),"%Y-%m-%d")
premiere_heure<-0
nheure<-200 
     
     # on regarde quelles sont les run disponibles sur le serveur
     ech_IFS<-NULL
     ech_IFS<-get_echeances_dispo(filiere="ecpds",modele="ifs",type="oper")
     
     # on regarde ce qu'il existe (ou pas) pour la journée d'aujourd'hui comme run
     echtoday<-subset(ech_IFS,date==Sys.Date())
     
     # si la donnée du jour est dispo on prend la plus recente, sinon on prend la plus récente la veille
     
     if (nrow(echtoday)==1) {
       date_run_<-format(Sys.Date(),"%Y-%m-%d")
       heure_run<-max(echtoday$heure_run)
     } else
     {
       date_run_<-format(Sys.Date()-1,"%Y-%m-%d")
       heure_run<-"18"
       premiere_heure<-premiere_heure+6
       nheure<-nheure+6
     }
     
     data<-NULL
     #1 - traitement par echéance
     
     message(" ################ TRAITEMENT DE l'echeance  #################### ")


       #  Créer un dossier pour recevoir les fichiers
       dir.create("data_meteo", showWarnings = FALSE)

     for (h in seq(premiere_heure,nheure,3))
     {
       print(paste0("Heure :",h))
       
       data_h<-NULL
       # on télécharge le fichier le d'heure demandée
       

       
       fichier_grib2<- download_meteo_ecmwf_forecast(date_run=date_run_,run_hour=heure_run,filiere="ecpds",type="oper",step=h,modele="ifs",destination_dir ="./data_meteo")
       # on fait l'extraction au niveau des points des stations
       niveaux_<-c("highCloudLayer","meanSea","mediumCloudLayer","soilLayer","surface","heightAboveGround","lowCloudLayer")
       # Forcer le chemin vers eccodes sous GitHub Actions
if (Sys.info()["sysname"] == "Linux") {
  Sys.setenv(PATH = paste("/home/runner/conda/bin", Sys.getenv("PATH"), sep = ":"))
}

       data_h<-traitement_grb2ecmwf(grib2_file=fichier_grib2[1],points=spots,niveaux=niveaux_,destination_dir ="./data_meteo")
       # calcul de l'heure et de la date du run
       dateheure_run<-paste0(fichier_grib2[3]," ",fichier_grib2[2],":00")
       data_h$date_run<-as.POSIXct( strptime(dateheure_run,"%Y-%m-%d %H:%M"))
       # on ajoute la date et heure de l'échéance
       data_h$date_ech<-data_h$date_run+h*60*60
       data<-rbind(data,data_h)
     }
     
     write.csv(data,"data_meteo.csv",row.names=FALSE)



