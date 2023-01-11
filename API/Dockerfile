# Dockerfile
FROM rocker/r-base:4.0.3 

# Instalacion librerias linux
RUN apt-get update -qq && apt-get install -y \ 
    libssl-dev \ 
    libcurl4-gnutls-dev \
    libsodium-dev

# Creando la carpeta para los archivos de la API
RUN mkdir -p ~/API

# Copiamos todo lo del actual directorio a la carpeta de la API
COPY "/" "R/" 
WORKDIR "R/" 

# Exponemos a traves del puerto 8080
EXPOSE 8080 

# Instalacion paquetes de R
RUN install2.r plumber\
    jsonlite\
    tidyverse\
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds # Borrando archivos temporales


ENTRYPOINT ["Rscript", "execute_plumber.R"]