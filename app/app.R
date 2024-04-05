require(shiny)
require(shinymanager)
source("edit_knowledge_base.R")

# Environment variables
db.passphrase <- Sys.getenv("DB_PASSPHRASE")
admin1.mail <- Sys.getenv("ADMIN1_MAIL")
admin2.mail <- Sys.getenv("ADMIN2_MAIL")
admin3.mail <- Sys.getenv("ADMIN3_MAIL")

# Rdata
## knowledge.base is the variable that contains the compound knowledge base
rdata.file <<- "compound_knowledge_base.Rdata"
load(rdata.file, envir=.GlobalEnv)

# Port options
#options(shiny.host = "127.0.0.1")
options(shiny.port = 3838)

ui <- fluidPage(
  titlePanel("Test authentification"),
  
      mainPanel(
          textInput(inputId="cas.numbers", label="Query"),
          selectInput("separator", "Select separator:", choices=c("Comma", "Space", "Tab")),
          actionButton("confirmButton", "Confirm")
      )
)

# Wrap UI with secure_app
ui <- secure_app(ui,
                 enable_admin = TRUE,
                 tags_bottom = tags$div(
                   tags$p(style="text-align: center;",
                     	  "To create an account or for any questions, please contact the PTV:"
                   ),
		           tags$p(style="text-align: center;",
		                  admin1.mail, tags$br(),
			              admin2.mail, tags$br(),
			              admin3.mail
                   )
                 )
)

server <- function(input, output, session) {
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(
      db = "./login_db/login_db.sqlite",
      passphrase = db.passphrase
    )
  )

  output$myinput <- renderUI({
        
  })
}

shinyApp(ui, server)
#if (reactiveValuesToList(res_auth)$admin == TRUE) {
#    output$role <- renderText("Your role is admin")
#} else {
#    output$role <- renderText("Your role is user here")
#}