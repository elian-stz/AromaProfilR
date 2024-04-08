require(shiny)
require(shinymanager)
source("edit_knowledge_base.R")

# Initialize files
knowledge.base.commit.logs()

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
  titlePanel("Edit the knowledge base"),
  sidebarLayout(
      sidebarPanel(
          textInput(
              inputId = "remove.cas",
              label = "Enter CAS number(s) separated by spaces"
              ),
      actionButton("submit", "Submit")
      ),
      mainPanel(
          verbatimTextOutput("kb.logs")
      )
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
    
    observeEvent(input$submit, {
        remove.cas.numbers(input$remove.cas)
    })
    
    output$kb.logs <- renderText({
        file_content <- readLines("knowledge_base_commit.log")
        paste(file_content, collapse="\n")
        })
}

shinyApp(ui, server)
#if (reactiveValuesToList(res_auth)$admin == TRUE) {
#    output$role <- renderText("Your role is admin")
#} else {
#    output$role <- renderText("Your role is user here")
#}