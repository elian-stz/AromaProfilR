library(shiny)
library(shinymanager)

db.passphrase <- Sys.getenv("DB_PASSPHRASE")
#options(shiny.host = "127.0.0.1")
options(shiny.port = 3838)

ui <- fluidPage(
  titlePanel("Test authentification"),
  uiOutput("myinput"),
  textOutput("role"),
  downloadLink("download", "Download RData")
)

# Wrap your UI with secure_app
ui <- secure_app(ui,
                 enable_admin = TRUE,
                 tags_bottom = tags$div(
                   tags$p(
                     "For any question, please contact the ",
                     tags$a(
                       href = "mailto:someone@example.com",
                       target="_top", "PTV"
                     )
                   )
                 )
                )

server <- function(input, output, session) {
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(
      db = "./db/login_db.sqlite",
      passphrase = db.passphrase
    )
  )

  output$myinput <- renderUI({
    output$download <- downloadHandler(
      filename = function() {
        paste("iris_", Sys.Date(), ".Rdata", sep='')
      },
      content = function(con) {
        save(iris, file=con)
      }
    )
    #output$download = load(file=paste("iris", Sys.Date(), ".Rdata"))
    if (reactiveValuesToList(res_auth)$admin == TRUE) {
      output$role <- renderText("Your role is admin")
    } else {
      output$role <- renderText("Your role is user here")
    }
    
  })
}

shinyApp(ui, server)
