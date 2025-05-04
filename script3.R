name: Script quotidien marée et météo

on:
  schedule:
    - cron: '0 6 * * *'   # Tous les jours à 6 h UTC
  workflow_dispatch:

jobs:
  run-script:
    runs-on: ubuntu-latest

    steps:
      # 1) Cloner ton dépôt
      - name: Checkout
        uses: actions/checkout@v3
        with:
          token: ${{ secrets.GITHUB_TOKEN }}

      # 2) Installer R
      - name: Setup R
        uses: r-lib/actions/setup-r@v2

      # 3) Installer Python 3.10
      - name: Setup Python
        uses: actions/setup-python@v5
        with:
          python-version: '3.10'

      # 4) Installer ECCODES (grib_copy, grib_to_netcdf) + libs système
      - name: Installer ECCODES & dépendances système
        run: |
          sudo apt-get update
          sudo apt-get install -y \
            libeccodes-tools libeccodes-dev \
            libgdal-dev libproj-dev libgeos-dev libudunits2-dev \
            libpng-dev

      # 5) Installer la librairie Python copernicusmarine
      - name: Installer copernicusmarine (Python)
        run: |
          python -m pip install --upgrade pip
          pip install copernicusmarine

      # 6) Installer les packages R nécessaires
      - name: Installer les packages R
        run: |
          Rscript -e "install.packages(c(
            'terra','tidyr',
            'httr','rvest','ncdf4','raster','dplyr','stringr','jsonlite'
          ), repos='https://cloud.r-project.org')"

      # 7) Réinstaller reticulate
      - name: Réinstaller reticulate
        run: |
          Rscript -e "install.packages('reticulate', repos='https://cloud.r-project.org')"

      # 8) Vérifier l'installation des packages R
      - name: Vérifier les packages R installés
        run: |
          Rscript -e "installed.packages()[,'Package']"

      # 9) Exposer la bibliothèque R où les packages ont été installés
      - name: Exposer R_LIBS_USER
        run: |
          R_LIB=$(Rscript -e '.libPaths()[1]' --vanilla)
          echo "R_LIBS_USER=${R_LIB}" >> $GITHUB_ENV

      # 10) Vérifier le chemin des bibliothèques R
      - name: Vérifier R_LIBS_USER
        run: |
          echo "R_LIBS_USER is set to $R_LIBS_USER"

      # 11) Afficher le chemin des bibliothèques R avant d'exécuter le script
      - name: Afficher le chemin des bibliothèques R
        run: |
          Rscript -e ".libPaths()"

      # 12) Exécuter ton script R principal
      - name: Exécuter script3.R
        env:
          CMEMS_USER:      ${{ secrets.CMEMS_USER }}
          CMEMS_PWD:       ${{ secrets.CMEMS_PWD }}
          DATA_PUSH_TOKEN: ${{ secrets.DATA_PUSH_TOKEN }}
        run: |
          Rscript script3.R

      # 13) Commit + push vers le dépôt principal
      - name: Commit des CSV générés
        run: |
          git config --global user.name "github-actions"
          git config --global user.email "github-actions@github.com"
          git add zos_points.csv data_meteo.csv
          git commit -m "Mise à jour automatique des CSV" || echo "Rien à committer"
          git push

      # 14) Push vers le dépôt privé de données
      - name: Push vers maree-donnees
        run: |
          git clone https://x-access-token:${{ secrets.DATA_PUSH_TOKEN }}@github.com/ahulin/maree-donnees.git data-push
          cp zos_points.csv data_meteo.csv data-push/
          cd data-push
          git config user.name "github-actions"
          git config user.email "github-actions@github.com"
          git add zos_points.csv data_meteo.csv
          git commit -m "Mise à jour quotidienne des CSV" || echo "Rien à committer"
          git push
