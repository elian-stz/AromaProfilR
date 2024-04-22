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
#knowledge.base <<- readRDS(rdata.file)
load(rdata.file, envir=.GlobalEnv)

# Port options
#options(shiny.host = "127.0.0.1")
options(shiny.port = 3838)

ui <- fluidPage(
    tags$head(
        tags$style(
            HTML(c("#shiny-notification-panel {
                 position: fixed;
                 top: 0px;
                 right: 0px;
                 background-color: #00000000;
                 padding: 2px;
                 width: 300px;
                 max-width: 100%;
                 z-index: 999999
                 }
                 ", "hr {border-top: 0.5px solid #636363;}"
            )
                 )
            )
        ),
  titlePanel("Edit the knowledge base"),
  sidebarLayout(
      sidebarPanel(
          tags$h2("Edit entries"),
          tags$ol(
              tags$li("Generate a pre-filled template CSV"),
              textInput(
                  inputId = "prefilled.template",
                  label = HTML("Enter CAS registry number(s) separated by spaces, or type <code>all</code> or <code>empty</code>")
              ),
              downloadLink("download", "Download template"),
              br(),br(),
              tags$li("Edit the template"),
              tags$ul(
                  tags$li("Modify existing entries or add new ones"),
                  tags$li("Decimal separators must be points"),
                  tags$li("LRI values must be separated by semicolons (;)"),
                  tags$li(HTML("Use the <code>keep.previous</code> flag to keep previous information")),
              ),
              br(),
              tags$li("Submit the edited template file"),
              fileInput("upload.template", "Upload the filled template", accept=c(".csv")),
          ),
          
          tags$hr(),
          
          tags$h2("Add an entry"),
          textInput(
              inputId = "add.cas",
              label = "Enter a single CAS registry number"
          ),
          actionButton("submit.add", "Add"),
          
          tags$hr(),
          
          tags$h2("Delete entries"),
          textInput("remove.cas",
              label = "Enter CAS registry number(s) separated by spaces"
          ),
          actionButton("submit.remove", "Delete"),
      ),
      mainPanel(
          textOutput("compound.number"),
          tags$head(
              tags$style(HTML("#scrollableText { height: 800px; overflow: auto; }"))
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
        shinyjs::disable("submit.add")
        tags$style(HTML(".container-fluid {cursor: wait;}"))
        validate(
            need(input$add.cas != "", "Field must not be empty")
            )
        add.single.cas.number(input$add.cas)
        shinyjs::enable("submit.add")
        tags$style(HTML(".container-fluid {cursor: default;}"))
    })
    
    
    # GENERATE TEMPLATE
    output$download <- downloadHandler(
        filename = function() paste(Sys.Date(), "_template.csv", sep=""),
        content = function(con) {
                write.csv(generate.prefilled.template(input$prefilled.template), con, row.names=FALSE)
            }
        )
    
    output$compound.number <- renderText({
        invalidateLater(2000, session)
        paste("Number of compounds in the knowledge base: ", length(knowledge.base), sep="")
    })
    output$kb.logs <- renderText({
        invalidateLater(2000, session)
        file_content <- readLines("knowledge_base_commit.log")
        paste(file_content, collapse="\n")
    })
    
    observeEvent(input$upload.template, {
        dataframe <- read.csv(input$upload.template$datapath, sep=",", dec=".", na.strings=c("", "NA"))
        colnames(dataframe)[colnames(dataframe) == "molecular_weight_g.mol.1"] <- "molecular_weight_g.mol-1"
        edit.with.template(dataframe)
#        ext <- tools::file_ext(input$upload$name)
#        switch(ext,
#               csv = vroom::vroom(input$upload$datapath, delim = ","),
#               tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
#               validate("Invalid file; Please upload a .csv or .tsv file")
#        )
    })
}

shinyApp(ui, server)
#if (reactiveValuesToList(res_auth)$admin == TRUE) {