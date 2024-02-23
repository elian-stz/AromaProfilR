FROM rocker/shiny:latest

RUN rm -rf /srv/shiny-server/* \
	&& apt-get update \
	&& apt-get -y install \
		libssl-dev \
		libsodium-dev \
		sqlite \
		vim \
	&& R -e "install.packages(c('shinymanager'))"

WORKDIR /srv/shiny-server/

COPY ./app/*.R ./

RUN mkdir ./db

RUN chown -R shiny:shiny ./db

USER shiny
