#' Return the SQLite path.
#' @examples
#' getSQLitePath()
getSQLitePath <- function() return("./data/login_db.sqlite")

#' Create an encrypted SQLite database containing users' credentials.
#'
#' Create an encrypted SQLite database containing users' credentials. Credentials
#' are stored in the `.Renviron` file.
#' @examples
#' createSQLiteDB()
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
				sqlite_path = getSQLitePath(),
				passphrase = db.passphrase
	)
}

#' Write or overwrite the SQLite database containing users' credentials.
#' @examples
#' writeLoginDB()
writeLoginDB <- function() {
	if (!file.exists(getSQLitePath())) createSQLiteDB()
	else {
		cat("Do you really want to overwrite the database? (y/n)\n")
		choice <- readLines("stdin", 1)
		choice <- tolower(choice)
		if (choice == "y" || choice == "yes") createSQLiteDB()
	}
}

writeLoginDB()