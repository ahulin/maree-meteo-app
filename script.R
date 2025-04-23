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

date_min<-format(Sys.Date()-6,"%Y-%m-%dT23:00:00")
date_max<-format(Sys.Date()+7,"%Y-%m-%dT23:00:00")

# 📥 Téléchargement des données
cmt$subset(
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
spots$lon<-spots$lon-0.01

# on cherche le fichier le plus récent
fichs<-list.files("./data_maree")
ctime<-file.info(paste0("./data_maree/",fichs))$ctime
fich<-fichs[ctime==max(ctime)]

# Charger la variable 'zos' comme SpatRaster
# variable zos : sea_surface_height_above_geoid
zos_stack <- rast(paste0("./data/",fich), subds = "zos")

points_vect <- vect(spots, geom = c("lon", "lat"), crs = crs(zos_stack))

# Extraire les valeurs de chaque couche (temps) pour chaque point
valeurs <- extract(zos_stack, points_vect)

# Ajouter l'identifiant pour retrouver à qui appartiennent les données
valeurs$id <- points_vect$id[valeurs$ID]

#exporte le csv
write.csv(valeurs,"./data/maree.csv",row.names=FALSE)


