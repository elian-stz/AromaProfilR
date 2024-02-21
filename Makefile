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
	$(EXEC) Rscript create_db.R

db-rm:
	$(EXEC) rm db/login_db.sqlite

#init-db:
#	if ! [ -f login_db.sqlite ]; then \
#		Rscript create_db.R; \
#	fi
