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
    tags$head(
        tags$style(
            HTML(".shiny-notification {
                 position: fixed;
             top: calc(1%);
             left: calc(85%);
             }
             "
            )
        )
    ),
  titlePanel("Edit the knowledge base"),
  sidebarLayout(
      sidebarPanel(
          textInput(
              inputId = "remove.cas",
              label = "Enter CAS registry number(s) separated by spaces to remove"
              ),
          actionButton("submit.remove", "Submit"),
          br(), br(),
          textInput(
              inputId = "add.cas",
              label = "Enter a single CAS registry number to add"
          ),
          actionButton("submit.add", "Submit")
      ),
      mainPanel(
          tags$head(
              tags$style(HTML("#scrollableText { height: 300px; overflow: auto; }"))
          ),
          div(id = "scrollableText", verbatimTextOutput("kb.logs"))
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
    
    # REMOVE
    observeEvent(input$submit.remove, {
        validate(
            need(input$remove.cas != "", "Field must not be empty")
        )
        remove.cas.numbers(input$remove.cas)
    })
    
    # ADD
    observeEvent(input$submit.add, {
        validate(
            need(input$add.cas != "", "Field must not be empty")
            )
        add.single.cas.number(input$add.cas)
    })
    
    output$kb.logs <- renderText({
        invalidateLater(2000, session)
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