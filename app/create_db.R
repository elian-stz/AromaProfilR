###########################################################
# Create an encrypted SQLite database			  #
# passphrase: testpass					  #
# Users: ID     password				  #
#        admin  retraitement2024			  #
#        user   azerty 					  #
###########################################################

library(shinymanager)
#library(keyring)

createSQLiteDB <- function() {
	# data.frame with credentials info
	credentials <- data.frame(
		user = c("user", "admin"), # mandatory
		password = c("azerty", "retraitement2024"), # mandatory
		admin = c(FALSE, TRUE),
		stringsAsFactors = FALSE
	)

	#key_set("R-shinymanager-key", "test")
	# Create the encrypted database
	create_db(
		credentials_data = credentials,
		sqlite_path = "./db/login_db.sqlite", # will be created
		passphrase = "testpass"#key_get("R-shinymanager-key", "test")
	)
}

main <- function() {
	if (!file.exists("./db/login_db.sqlite")) {createSQLiteDB()}
	else {
		cat("Do you really want to overwrite the database? (y/n)\n")
		choice <- readLines("stdin", 1)
		if (choice == "y" | choice == "yes") {createSQLiteDB()}
	}

}

main()
