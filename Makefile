DC?=docker-compose
EXEC?=$(DC) exec app

build:
	$(DC) pull --ignore-pull-failures
	$(DC) build --force-rm

up:
	$(DC) up -d --remove-orphans

down:
	$(DC) down

db-init:
	$(EXEC) Rscript create_login_db.R

db-rm:
	$(EXEC) rm data/login_db.sqlite

install:
	$(DC) pull --ignore-pull-failures
	$(DC) build --force-rm
	$(DC) up -d --remove-orphans
	$(EXEC) Rscript create_login_db.R
