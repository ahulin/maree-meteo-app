# Chargement de reticulate
library(reticulate)

# 👉 Chemin vers le Python configuré dans GitHub Actions
use_python("/opt/hostedtoolcache/Python/3.10.17/x64/bin/python", required = TRUE)


library(terra)

library(tidyr)
library(ncdf4)
library(dplyr)
library(stringr)
library(jsonlite)



# 📥 Téléchargement des données



spots<-read.table("spots.csv",sep=",",header=TRUE)

spots$lon<-spots$lon-0.05
points_vect <- vect(spots, geom = c("lon", "lat"), crs = "EPSG:4326")


#spots<-data.frame(
#  id=c("aytre","saint_trojan","hossegor"),
#lat=c(46.1188,45.826,43.6654),
#  lon=c(-1.130,-1.2486,-1.4429)
#)
# on s'assure qu'on est sur mer et pas sur terre


source("maree_api_open_meteo.R")


# les dates à telecharger
date_min<-format(Sys.Date()-6,"%Y-%m-%d")
#date_max<-format(Sys.Date()+10,"%Y-%m-%dT23:00:00")
df<-NULL
for (s in 1:nrow (spots))
{
  ms<-NULL

  ms<-get_maree_from_open_meteo(lon=spots[s,"lon"],lat=spots[s,"lat"],date_deb=date_min,njour=16)
  ms$id<-spots[s,"id"]
  df<-rbind(df,ms)
}




df$jour<-substr(df$datetime,1,10)
df$heure<-substr(df$datetime,12,13)

# calcul des coeff de maree
dftide<-na.omit(df[,c("jour","id","tide_height")])
mini<-aggregate(tide_height~jour+id,dftide,min,na.rm=TRUE)
maxi<-aggregate(tide_height~jour+id,dftide,max,na.rm=TRUE)
nb<-aggregate(tide_height~jour+id,dftide,length)

# calcul du coefficient de maree
tide<-merge(maxi,mini,by=c("jour","id"))[nb$tide_height==24,]
tide$amplitude<-tide$tide_height.x-tide$tide_height.y
tide<-merge(tide,spots,by="id")
tide$val<-round(tide$amplitude*70/tide$maree_median) # calcul du coeff

tide$heure<-"00"
tide$param<-"coeff_maree"
tide$datetime<-paste(tide$jour,"T",tide$heure,":00")

data_long <- pivot_longer(
  df,
  cols = -c(id, datetime,jour,heure),
  names_to = "param",
  values_to = "val"
)


data_long<-rbind(data_long,tide[,c("datetime","id","jour","heure","param","val")])
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


library(raster)


source("ecmwf_app.R")

cat("✅ Le fichier ecmwf_app.R a été chargé directement depuis le dépôt cloné\n")






jour_ech<-format(Sys.Date(),"%Y-%m-%d")
premiere_heure<-0
nheure<-135
     
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
  print(paste0("Nombre d'heure à traiter : ",nheure))
     for (h in seq(premiere_heure,nheure,3))
     {
       print(paste0("Heure :",h))
       
       data_h<-NULL
       # on télécharge le fichier le d'heure demandée
       

       
       fichier_grib2<- download_meteo_ecmwf_forecast(date_run=date_run_,run_hour=heure_run,filiere="ecpds",type="oper",step=h,modele="ifs",destination_dir ="./data_meteo")

      if (!file.exists(fichier_grib2[1])) {stop(paste0("Le fichier ",fichier_grib2[1]," n'a pas ete telecharge"))} else {message(paste0("Le fichier ",fichier_grib2[1]," a bien ete telecharge"))}
       
       # on fait l'extraction au niveau des points des stations
       # niveaux_<-c("highCloudLayer","meanSea","mediumCloudLayer","soilLayer","surface","heightAboveGround","lowCloudLayer")
       niveaux_<-c("meanSea","surface","heightAboveGround")
       # Forcer le chemin vers eccodes sous GitHub Actions


       data_h<-traitement_grb2ecmwf(grib2_file=fichier_grib2[1],points=spots,niveaux=niveaux_)
       # calcul de l'heure et de la date du run
       dateheure_run<-paste0(fichier_grib2[3]," ",fichier_grib2[2],":00")
       data_h$date_run<-as.POSIXct( strptime(dateheure_run,"%Y-%m-%d %H:%M"))
       # on ajoute la date et heure de l'échéance
       data_h$date_ech<-data_h$date_run+h*60*60
       data<-rbind(data,data_h)
     }
     
     write.csv(data,"data_meteo.csv",row.names=FALSE)
     if(file.exists("data_meteo.csv")) { print("Le fichier data_meteo.csv a ete correctement cree") }

