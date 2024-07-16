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

RUN mkdir -p data && chown -R shiny:shiny data

VOLUME /srv/shiny-server/data

COPY app/*.R ./

COPY app/www/. ./www/

COPY .Renviron ./

#RUN chown shiny:shiny data/compound_knowledge_base.rds

USER shiny
