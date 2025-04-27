# Chargement de reticulate
library(reticulate)

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

# 📥 Téléchargement des données
d<-cmt$subset(
  dataset_id = "cmems_mod_ibi_phy_anfc_0.027deg-2D_PT1H-m",
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
points_vect <- vect(spots, geom = c("lon", "lat"), crs = "EPSG:4326")


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
ext(zos_stack) <- c(min_lon, max_lon, min_lat, max_lat)


#construire la date
datemin <- as.POSIXct(d$coordinates_extent[[3]]$minimum, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
datemax <-  as.POSIXct(d$coordinates_extent[[3]]$maximum, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

# puis conversion vers Europe/Paris
datemin_paris <- format(datemin, tz = "Europe/Paris", usetz = TRUE)
datemax_paris <- format(datemax, tz = "Europe/Paris", usetz = TRUE)



# Extraire les valeurs de chaque couche (temps) pour chaque point
valeurs <- extract(zos_stack, points_vect)

# Ajouter l'identifiant pour retrouver à qui appartiennent les données
valeurs$spot <- points_vect$id[valeurs$ID]

library(tidyr)
#data_long <- valeurs %>%
#  pivot_longer(
#    cols = -c(spot,ID), names_to = c("heure", "mesure"),
#    names_sep = "[^[:alnum:]]+", values_to = "maree")
#data_long$date<-rep(time(zos_stack),3)

data_long <- valeurs %>%
  pivot_longer(
    cols = -c(spot,ID), names_to = c("date_UTC"),
    values_to = "maree")
data_long$date_UTC<-rep(seq(datemin,datemax,by=60*60),3)
data_long$date_paris<- format(data_long$date_UTC, tz = "Europe/Paris", usetz = TRUE)

data_long$jour<-substr(data_long$date_paris,1,10)
data_long$heure<-substr(data_long$date_paris,12,13)

#exporte le csv
write.csv(data_long,"zos_points.csv",row.names=FALSE)
