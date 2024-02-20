.PHONY: all

all: clean

clean: init-db
	rm create_db.R

init-db:
	if ! [ -f login_db.sqlite ]; then \
		Rscript create_db.R; \
	fi
