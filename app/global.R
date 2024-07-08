# Options-----------------------------------------------------------------------
#options(shiny.host = "127.0.0.1")
options(shiny.port = 3838)
options(shiny.maxRequestSize = 30*1024^2)

# Global variables--------------------------------------------------------------
# Environment variables
db.passphrase <- Sys.getenv("DB_PASSPHRASE")
admin1.mail <- Sys.getenv("ADMIN1_MAIL")
admin2.mail <- Sys.getenv("ADMIN2_MAIL")
admin3.mail <- Sys.getenv("ADMIN3_MAIL")

# Knowledge base
rds.file <- "./data/compound_knowledge_base.rds"
knowledge.base <- readRDS(rds.file)

# Sourcing----------------------------------------------------------------------
file.sources <- list.files()
file.sources <- grep("^(fct|mod)", file.sources, value=TRUE)
sapply(file.sources, source)

# Loading packages--------------------------------------------------------------
require(shiny)
require(shinymanager)

# Initialising files------------------------------------------------------------
knowledge.base.commit.logs()