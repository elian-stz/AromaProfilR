# Port options------------------------------------------------------------------
#options(shiny.host = "127.0.0.1")
options(shiny.port = 3838)

# Global variables--------------------------------------------------------------
# Environment variables
db.passphrase <- Sys.getenv("DB_PASSPHRASE")
admin1.mail <- Sys.getenv("ADMIN1_MAIL")
admin2.mail <- Sys.getenv("ADMIN2_MAIL")
admin3.mail <- Sys.getenv("ADMIN3_MAIL")

# Knowledge base containing compound info
rds.file <- "data/compound_knowledge_base.rds"
knowledge.base <- readRDS(rds.file)

# Sourcing----------------------------------------------------------------------
file.sources <- setdiff(list.files(), list.dirs(recursive=FALSE, full.names=FALSE))
file.sources <- grep("*.R", file.sources, value=TRUE)
file.sources <- file.sources[!(file.sources %in% c("app.R", "global.R", "create_login_db.R"))]
sapply(file.sources, source)

# Package loading---------------------------------------------------------------
require(shiny)
require(shinymanager)

# Initialising files------------------------------------------------------------
knowledge.base.commit.logs()