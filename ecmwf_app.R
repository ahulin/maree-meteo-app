
#' traitement_grb2ecmwf
#'
#' @param grib2_file  as text :  le chemin complet et le nom du fichier du grib2 à traiter
#' @param points as dataframe les points où extraires les data, doit contenir les champs "lon" et "lat"
#' @param niveaux as vector of text : les niveaux à traiter (tous par défaut)
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#' # ################## la liste des stations sur lesquelles on extrait les valeurs
#'
#' Le code suivant ne traite qu'une seule heure de donnees (step=0)
#' #get les noms des stations
#' con<-PollensCpt::get_connection_interpollens_Vtunnelssh2()
#' query<-NULL
#' query<-paste0("select * from general.station ")
#' villes<-NULL
#' villes <- DBI::dbGetQuery(con, query)
#' DBI::dbDisconnect(con)
#'
#' fichier_grib2<- download_meteo_ecmwf_forecast(date_run=NULL,run_hour=NULL,filiere="forecast",step=0,modele="ifs",type="oper")
#' d<-traitement_grb2ecmwf(grib2_file=fichier_grib2[1],points=villes,niveaux="soilLayer")
#' }
traitement_grb2ecmwf<-function(grib2_file,points,domaine=c(-6.5, 10.3, 40.1, 51.5),destination_dir = NULL,
                               niveaux=c("highCloudLayer","meanSea","mediumCloudLayer","soilLayer","surface","heightAboveGround","lowCloudLayer","isobaricInhPa","entireAtmosphere"))
{
 message(grib2_file)
  library(ncdf4)
  library(terra)
  library(raster)

  ##################conn = #################### traitement du grib2 téléchargé  avec eccodes #####################
  # eccodes doit avoir au préalable été installé avec la commande suivante :

  #system("conda install conda-forge::eccodes")

  # le nom et chemin du fichier téléchargé
  grib_file<-destination_path <-grib2_file

  # Forcer le chemin vers eccodes sous GitHub Actions

  
  Sys.setenv(PATH = paste("/usr/share/miniconda/Library/bin", Sys.getenv("PATH"), sep = ":"))

  #les infos ont été prises ici pour la suite : https://confluence.ecmwf.int/display/OIFS/How+to+convert+GRIB+to+netCDF

  # on sépare les niveaux, un fichier grb par niveau
res <- system(paste0("grib_copy ",destination_path," ",destination_dir,"/ICMGG_[typeOfLevel].grb"))
if (res != 0) stop("❌ Erreur dans grib_copy")



  ##### à partir de là, on traite les fichiers niveaux par niveau ##########################

  #niveaux<-c("highCloudLayer","meanSea","mediumCloudLayer","soilLayer","surface","heightAboveGround","lowCloudLayer","isobaricInhPa","entireAtmosphere")
  #niveaux<-c("highCloudLayer","meanSea","mediumCloudLayer","soilLayer","surface","heightAboveGround","lowCloudLayer")
  data_points<-NULL
  for (niv in niveaux)
  {
    print(paste0("niveau : ",niv))
    # on converti les fichiers par niveau en ncdf (l'étape précédente est nécessaire pour le faire)
    #system("grib_to_netcdf -D NC_FLOAT -o C:/TEMP/ICMGG_surface.nc C:/TEMP/ICMGG_surface.grb")
    commande<-NULL
   commande <- sprintf("grib_to_netcdf -D NC_FLOAT -o %s/ICMGG_%s.nc %s/ICMGG_%s.grb", destination_dir, niv, destination_dir, niv)
   res <- system(commande)
   if (res != 0) stop("❌ Erreur dans grib_to_netcdf")

  

    # on lit le ncdf
    fich_nc<-sprintf("%s/ICMGG_%s.nc",
                     destination_dir,niv)

    nc<-nc_open(fich_nc)
    tt<-strptime("1900-01-01 00:00:00","%Y-%m-%d %H:%M:%S")+ ncvar_get(nc, "time")*60*60
    #attributes(nc)$names
    print(paste("The file has",nc$nvars,"variables,",nc$ndims,"dimensions and",nc$natts,"NetCDF attributes"))
    #names(nc$dim)

    ################################################################
    # Extraire les valeurs aux points des stations
    ###################################################################

    # récupère les données au stations
    # en faire un raster

    rj <-rast(fich_nc)
    tmp_raster <- rj

    # on gère le cas où il y a deux heure dans le fichier : on ne prend que la première. Vu pour le niveau "heightAboveGround"
    if (length(tt)>1) {
      indice<-seq(1,length(names(rj)),length(tt))
      tmp_raster<-tmp_raster[[indice]]
      names(tmp_raster)<-gsub("_1","",names(tmp_raster))

      }

    for (var2 in names(tmp_raster)) # pour une variable donnéees (ex vsw) il peut y avoir plusieures levels, traités ici
    {
      print(var2)
      sub_level<-NULL # par ex profondeur du sol. Pour la pression au niveau du sol, ca sera NA
      # on traite le cas sot et vsw (ne perturbe pas les autres variables)
      sub_level<-strsplit(var2,"_")[[1]][2]
      var<-strsplit(var2,"_")[[1]][1]

      #  limiter le raster au domaine national
      e <- ext(domaine)
      rc <- crop(tmp_raster[var2], e)
      rc2<-disaggregate(raster(rc),4,method='bilinear')

      #library(Ani.sSatellite)
      #raster2map(raster(rc))
      #raster2map(rc2)


      # Extraction rapide des valeurs aux points
      data_points <- rbind(data_points,data.frame(id_station=points$id_station,
                                                  val= terra::extract(rc2, points[, c("lon", "lat")]),
                                                  param=var,
                                                  level=niv,
                                                  sublevel=sub_level
      ))
      
    } # fin de for var2
    nc<-nc_close(nc)

    #library(tidyr)
    #res<-gather(data_points, "heure", "val", -insee)
    #res$time_reel<-as.POSIXct(as.numeric(res$heure) * 3600, origin = time_origin, tz = "UTC")

  } #fin de for niv in niveau

  #parametres recuperés :
  #unique(points[,c("param","level","sublevel")])
  #unique(points[duplicated(points),"param"])

  return(data_points)

}

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################


#' download_meteo_ecmwf_forecast
#'
#' @param date_run as character : date run au format YYYY-MM-DD
#' @param run_hour as character : "00" # "00" "06" "12" "18" : l'heure de la journée date-run à laquelle le modele a tourné. 4 runs par jour
#' @param step as character : les heure de prévi, de 3 en 3 : commence à 0 - 3 - 6
#' @param modele  as character : "aifs-single" ou "ifs". ifs est plus complet, aifs est post-traite par une ia avec moins de variables
#' @param filiere  as character "ecpds" ou "forecast" (epcds est la plus recente)
#' @param type  as character: "oper" : modele opérationel ou "enfo" : modele d'ensemble : pas traité ici
#'
#' @description : récupération des données meteo de ecmwf : une seule heure récupérée (step)
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#'  dispo<-NULL
#'  dispo<-get_echeances_dispo(filiere="ecpds",modele="ifs")
#'
#'  dispo<-dispo[dispo$url==max(dispo$url),]
#'  print(dispo)
#'  fichier_grib2<-download_meteo_ecmwf_forecast(date_run=dispo$date,run_hour=dispo$heure_run,step=0,modele="ifs")
#'  # ou et ca revient au même :
#'  fichier_grib2<-download_meteo_ecmwf_forecast(date_run=NULL,run_hour=NULL,step=0,modele="ifs")
#'
#'  fichier_grib2<- download_meteo_ecmwf_forecast(date_run="2025-04-13",run_hour="18",filiere="ecpds",step=h,modele="ifs",type="oper")
#'  }
download_meteo_ecmwf_forecast<-function(date_run=NULL,run_hour=NULL,step,modele,filiere="ecpds",type="oper",destination_dir = "N:/4_developpements/COPERNICUS/ECMWF")
{

# Verifier le dossier de destination
if (!dir.exists(destination_dir)) {stop(paste0("Le dossier ",destination_dir," n'existe pas"))}


##Les infos sur le modele :
##  https://confluence.ecmwf.int/display/FUG/Forecast+User+Guide
## https://codes.ecmwf.int/grib/param-db/

library(tidyr)

if (is.null(date_run)) { # si la date n'est pas précisée, on prend l'echeance la plus récente dispo
    dispo<-NULL
    dispo<-get_echeances_dispo(filiere,modele)
    if (!is.null(run_hour)) {dispo<-subset(dispo,heure_run ==run_hour)}
    dispo<-dispo[dispo$url==max(dispo$url),]
    date_run <- as.Date(dispo$date)
    run_hour<-dispo$heure_run
} else {
    date_run <- as.Date(date_run)
    if (is.null(run_hour)) run_hour<-"00"
    if (is.element(run_hour,c("06","18"))) {
      type<-"scda"
      filiere<-"ecpds"}
}

  # vérifier les autres parametres
  if (!is.element(run_hour,c("00" ,"06" ,"12" ,"18"))) {Stop("Le paramètre run_hour est incorrecte. Il doit être soit '00' '06' '12' '18'")}
  if (!is.element(modele,c("ifs","aifs-single"))) {Stop("Le paramètre modele est incorrecte. Il doit être soit 'ifs','aifs-single'")}

################### 1 . telechargement de la donnee ######################################


#le modele : ifs. Sa version améliorée par IA : aifs
# le mode de diffusion : ancien : forecast. Nouveau depuis 2023 : ecpds
#https://data.ecmwf.int/ecpds/home/opendata/20250409/00z/aifs-single/0p25/oper/
#"https://data.ecmwf.int/forecasts/20250409/00z/ifs/0p25/oper/20250409000000-24h-oper-fc.grib2"


url_base<-NULL
if (filiere=="ecpds")
{
    # construction de l'URL de la filière ecpds
    url_base<-sprintf("https://data.ecmwf.int/ecpds/home/opendata/%s/%sz/",
      format(date_run, "%Y%m%d"),
      run_hour
      )
}
if (filiere=="forecast")
{
    # construction de l'URL de la filière forecast( l'ancienne)
    url_base<-sprintf("https://data.ecmwf.int/forecasts/%s/%sz/" ,
      format(date_run, "%Y%m%d"),
      run_hour)
}



# on continue à construire l'url de téléchargement
if (modele=="aifs"|modele=="aifs-single")
{
  url_base<-paste0(url_base,"aifs-single/0p25/",type,"/")
} else if (modele=="ifs")
{
  url_base<-paste0(url_base,"ifs/0p25/",type,"/")
}

nom_fich<-NULL
nom_fich<-sprintf("%s%s0000-%sh-%s-fc.grib2",
                        format(date_run, "%Y%m%d"),
                        run_hour,
                        step,
                  type)
nom_fich_local<-sprintf("%s_%s_%s%s0000-%sh-%s-fc.grib2",
                  filiere,
                  modele,
                  format(date_run, "%Y%m%d"),
                  run_hour,
                  step,
                  type)


url<-paste0(url_base,nom_fich)

# on test l'existence du fichier sur le serveur
if (!url_exists(url))
{
  stop(paste0("Le fichier ",url," n'existe pas sur le serveur ecmwf "))
}

# emplacement de destination des fichiers
destination_path <- file.path(destination_dir, nom_fich_local)


# Télécharger avec chemin de destination
if (!file.exists(destination_path))
{
    download_cmd <- sprintf('curl -o "%s" "%s"', destination_path, url)
    system(download_cmd)
}

if (!file.exists(destination_path)) {
  message(paste0("Probleme lors du telechargement, fichier non recupere : ",destination_path))
  return(NULL)}

return(c(destination_path,run_hour,format(date_run,"%Y-%m-%d"),type,modele,filiere))

}




#' get_echeances_dispo
#'
#' @param filiere as text"ecpds" ou "forecast"
#' @param modele as text"ifs" ou "aifs-single"
#' @param type as text "oper" ou "enfo" : modèle opérationnel ou modèle d'ensemble
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#' get_echeances_dispo(filiere="ecpds",modele="ifs",type="enfo")
#' get_echeances_dispo(filiere="ecpds",modele="ifs",type="oper")
#' get_echeances_dispo(filiere="forecast",modele="ifs")
#' get_echeances_dispo(filiere="ecpds",modele="aifs-single")
#' get_echeances_dispo(filiere="forecast",modele="ifs",type="enfo")
#' }
#'
get_echeances_dispo<-function(filiere="ecpds",modele="ifs",type="oper")
{
    library(rvest)
    library(dplyr)
    library(stringr)

    if (filiere=="forecast")
    {
      url <- "https://data.ecmwf.int/forecasts/"

    } else if (filiere=="ecpds")
    {
      url <- "https://data.ecmwf.int/ecpds/home/opendata/"

      if (type=="oper") { type<-c("oper","scda")}
    }

   liste_heure_run<-c("00" ,"06","12" ,"18" )

    # Lire la page HTML
    page <- read_html(url)

    # Extraire les lignes qui contiennent les répertoires date_run
    entries <- page %>% html_elements("pre a") %>% html_text()

    # on construit à partir des dates présentes les url qui peuvent potentiellement exister
    url_theorique<-NULL
    url_theorique<-data.frame(date=sort(rep(entries,length(liste_heure_run)*length(type))),heure_run=liste_heure_run,type=sort(rep(type,length(liste_heure_run))))
    url_theorique$url<-paste0(url,url_theorique$date,url_theorique$heure_run,"z/",modele,"/0p25/",url_theorique$type,"/" )

    url_theorique<-url_theorique[order(url_theorique$url),]

    # on regarde si l'url du chemin du dossier existe, et one garde que les url qui existent
    url_theorique$exists <- sapply(url_theorique$url, url_exists)
    url_theorique<-subset(url_theorique,exists)

    # regarde si les fichiers ont ete cree, car meme si la page existe, ce nest pas forcement le cas des fichiers
    test_completude<-function(url_n)
    {
      page <- NULL
      page <- read_html(url_n)
      entries <- page %>% html_elements("pre a") %>% html_text()
      return (length(entries)>100) # peut se tester sur l'existance de .bufr le fichier qui est là par défaut au début
    }
    url_theorique$fichiers_presents <- sapply(url_theorique$url, test_completude)
    url_theorique<-subset(url_theorique,fichiers_presents)
    url_theorique$date<-format(strptime(url_theorique$date,"%Y%m%d/"),"%Y-%m-%d")
    return(url_theorique)

}
