#!/bin/bash
set -e

# repertoire app par defaut
APP_DIR="/root/GTA_NC_DataPaper"

# si on est en mode dev, on veut travailler dans /home/rstudio/pour eviter les problemes de cache renv
if [ "$MODE" = "dev" ]; then
  APP_DIR="/home/rstudio/GTA_NC_DataPaper"
fi

# Variables renv (communes aux 2 modes), on force un cache dans le projet
: "${RENV_CONFIG_CACHE_ENABLED:=FALSE}"
: "${RENV_PATHS_ROOT:=${APP_DIR}/renv}"
: "${RENV_PATHS_CACHE:=${APP_DIR}/renv/library}"

mkdir -p "$RENV_PATHS_CACHE" || true

if [ "$MODE" = "dev" ]; then
  echo "🔧 MODE=dev → Preparation de l'environnement RStudio"

  # creer l'utilisateur s'il n'existe pas
  useradd -ms /bin/bash rstudio 2>/dev/null || echo "Utilisateur rstudio deja present"

  # si le projet n'est pas encore dans /home/rstudio, on le copie
  if [ ! -d "$APP_DIR" ]; then
    echo "ok? Copie des fichiers vers $APP_DIR"
    mkdir -p "$APP_DIR"
    cp -a /root/GTA_NC_DataPaper/. "$APP_DIR"/
  fi

  # R demarre dans le bon dossier
  echo "setwd('$APP_DIR')" > /home/rstudio/.Rprofile

  # droits
  chown -R rstudio:rstudio /home/rstudio

  echo "okMode dev : lancement de RStudio Server"
  exec /init
else
  echo "okMode prod : lancement de l'application Shiny"
  exec R -e "shiny::runApp('$APP_DIR', port=3838, host='0.0.0.0')"
fi
