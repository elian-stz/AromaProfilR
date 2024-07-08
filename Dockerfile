FROM rocker/shiny:latest

RUN rm -rf /srv/shiny-server/* \
	&& apt-get update \
	&& apt-get -y install \
		libssl-dev \
		libsodium-dev \
		sqlite \
		libxml2 \
		libmagick++-dev \
		vim \
	&& R -e "install.packages(c('shinymanager', 'webchem', 'PubChemR', 'openxlsx'))"

WORKDIR /srv/shiny-server/

COPY app/. ./

#USER shiny

USER root
