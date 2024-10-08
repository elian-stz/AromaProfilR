DC?=docker-compose
EXEC?=$(DC) exec app

build:
	$(DC) pull --ignore-pull-failures
	$(DC) build --force-rm

up:
	$(DC) up -d --remove-orphans

down:
	$(DC) down

db-user-init:
	$(EXEC) Rscript create_login_db.R

db-user-rm:
	$(EXEC) rm data/login_db.sqlite

db-compound-init:
	$(DC) cp -a app/data/compound_knowledge_base_cpy.rds app:/srv/shiny-server/data/compound_knowledge_base.rds
	$(EXEC) mkdir -p data/descriptor_db
	$(DC) cp app/data/descriptor_db/flavornet_odor_descriptors.csv app:/srv/shiny-server/data/descriptor_db/.
	$(DC) cp app/data/descriptor_db/goodscents_odor_descriptors.csv app:/srv/shiny-server/data/descriptor_db/.
	$(DC) cp app/data/example_input_files.zip app:/srv/shiny-server/data/.

db-compound-rm:
	$(EXEC) rm -f data/compound_knowledge_base.rds
	$(EXEC) rm -rf data/descriptor_db/
	$(EXEC) rm -f data/example_input_files.zip

install: build up db-user-init db-compound-init

