# Chargement de reticulate
library(reticulate)

# Utiliser le Python global (où le package a été installé dans le YAML)
use_python("/usr/bin/python3", required = TRUE)

#  Récupération des identifiants depuis les variables d'environnement GitHub Actions
user <- Sys.getenv("CMEMS_USER")
pwd  <- Sys.getenv("CMEMS_PWD")

# Importer la librairie Python Copernicus Marine Toolbox
cmt <- import("copernicusmarine")

# Se connecter à Copernicus Marine
cmt$login(user, pwd)

#  Créer un dossier pour recevoir les fichiers
dir.create("data_maree", showWarnings = FALSE)

#  Téléchargement des données
cmt$subset(
  dataset_id = "cmems_mod_ibi_phy_anfc_0.027deg-2D_PT1H-m",
  variables = list("mlotst", "thetao", "ubar", "uo", "vbar", "vo", "zos"),
  minimum_longitude = -3.0484,
  maximum_longitude = 1.6309,
  minimum_latitude = 42.3668,
  maximum_latitude = 46.7394,
  start_datetime = "2025-05-01T23:00:00",
  end_datetime   = "2025-05-01T23:00:00",
  output_directory = "./data"
)

cat("✅ Données téléchargées dans le dossier /data_maree\n")

