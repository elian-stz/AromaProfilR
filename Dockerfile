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
	&& R -e "install.packages(c('shinymanager', 'webchem', 'PubChemR'))"

WORKDIR /srv/shiny-server/

COPY ./app/*.R ./.Renviron ./

RUN mkdir ./login_db

RUN chown -R shiny:shiny ./login_db

USER shiny
