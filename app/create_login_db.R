###########################################################
# Create an encrypted SQLite database                     #
###########################################################

require(shinymanager)

createSQLiteDB <- function() {
	# environment variables
	db.passphrase  <- Sys.getenv("DB_PASSPHRASE")
	admin.id       <- Sys.getenv("ADMIN_ID")
	admin.password <- Sys.getenv("ADMIN_PASSWORD")

	# dataframe with credentials info
	credentials <- data.frame(
		user = c(admin.id),
		password = c(admin.password),
		admin = c(TRUE),
		stringsAsFactors = FALSE
	)

	# Create the encrypted database
	shinymanager::create_db(
				credentials_data = credentials,
				sqlite_path = "./login_db/login_db.sqlite",
				passphrase = db.passphrase
	)
}

main <- function() {
	if (!file.exists("./login_db/login_db.sqlite")) createSQLiteDB()
	else {
		cat("Do you really want to overwrite the database? (y/n)\n")
		choice <- readLines("stdin", 1)
		choice <- tolower(choice)
		if (choice == "y" | choice == "yes") createSQLiteDB()
	}
}

main()