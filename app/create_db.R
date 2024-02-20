library(shinymanager)
#library(keyring)

# data.frame with credentials info
credentials <- data.frame(
	user = c("user", "admin"), # mandatory
	password = c("azerty", "retraitement2024"), # mandatory
	admin = c(FALSE, TRUE),
	stringsAsFactors = FALSE
)

#key_set("R-shinymanager-key", "test")
# Create the database
create_db(
	credentials_data = credentials,
	sqlite_path = "./login_db.sqlite", # will be created
	passphrase = "testpass"#key_get("R-shinymanager-key", "test")
)
