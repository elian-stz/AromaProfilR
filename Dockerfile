FROM rocker/shiny:latest

RUN rm -rf /srv/shiny-server/*

# Add here
    apt-get -y install libssl-dev libsodium-dev sqlite
    R -e "install.packages(c('shinymanager'))"

WORKDIR /srv/shiny-server/

COPY ./app/* .
