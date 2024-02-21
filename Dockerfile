FROM rocker/shiny:latest

RUN rm -rf /srv/shiny-server/* \
	&& apt-get update \
	&& apt-get -y install \
		libssl-dev \
		libsodium-dev \
		sqlite \
	&& R -e "install.packages(c('shinymanager', 'keyring'))"

WORKDIR /srv/shiny-server/

COPY ./app/* ./Makefile ./

RUN make
