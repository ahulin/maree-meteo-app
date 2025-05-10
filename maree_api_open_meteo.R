 
#' geocode_place
#'
#' @param place_name 
#'
#' @return
#' @export
#'
#' @examples
geocode_place <- function(place_name) {
    url <- paste0("https://geocoding-api.open-meteo.com/v1/search?name=", URLencode(place_name), "&count=1")
    res <- fromJSON(url)
    
    if (length(res$results) == 0) stop("Aucun résultat trouvé pour ce lieu.")
    
    coords <- res$results[1, c("latitude", "longitude")]
    return(coords)
  }


#' get_maree_from_open_meteo
#'
#' @param place 
#' @param date_deb 
#' @param njour 
#'
#' @return
#' @export
#'
#' @examples
get_maree_from_open_meteo<-function(lon,lat,date_deb=NULL,njour=NULL)
{
#https://open-meteo.com/en/docs/marine-weather-api?hourly=sea_level_height_msl,sea_surface_temperature&latitude=46&longitude=-1&timezone=Europe%2FBerlin&time_mode=time_interval&start_date=2025-05-03&end_date=2025-05-17


# Packages nécessaires
library(httr)
library(jsonlite)
library(dplyr)

if (is.null(date_deb))
{
  date_deb=format(Sys.Date()-5,"%Y-%m-%d")
  njour=15
}

if (is.null(njour))
{
  njour=15
}

 url <- paste0("https://marine-api.open-meteo.com/v1/marine?",
                "latitude=", lat,
                "&longitude=", lon,
                "&hourly=sea_level_height_msl,sea_surface_temperature,wave_height",
                "&start_date=", date_deb,
                "&end_date=", as.Date(date_deb) + njour,
                "&timezone=auto")
  
  res <- fromJSON(url)

  
  if (is.null(res$hourly$sea_level_height_msl)) stop("Pas de données de marée retournées.")
  
  tide_df <- data.frame(
    datetime = res$hourly$time,
    tide_height = res$hourly$sea_level_height_msl,
    sea_temp=res$hourly$sea_surface_temperature,
    wave_height=res$hourly$wave_height
  )

# Affichage et sauvegarde

return(tide_df)
}



#' calcul_base_70
#'
#' @param place ; as text
#' @description fonction à lancer manuellement. Permet le calcul de la médiane sur un an des amplitudes de marées pour un site, 
#' qui va correspondre à un coefficient de 70. Les résultats par site sont intégrés dans le fichier Spots.csv à a racine des scripts.
#' @return
#' @export
#'
#'
#' @examples
#' calcul_base_70("Biarritz")
calcul_base_70<-function(place)
{
  
 
coords<-  geocode_place(place)
r<-get_maree_from_open_meteo(lon=coords$longitude, lat=coords$latitude,format(Sys.Date()-365),365)



r$jour<-substr(r$datetime,1,10)                          
mini<-aggregate(tide_height ~jour,r,min,na.rm=TRUE)
maxi<-aggregate(tide_height ~jour,r,max,na.rm=TRUE)
rj<-merge(mini,maxi,by="jour")
rj$ampli<-rj$tide_height.y-rj$tide_height.x

#rj$coeff<-round((rj$ampli*70)/median(rj$ampli))

return(median(rj$ampli))

}

