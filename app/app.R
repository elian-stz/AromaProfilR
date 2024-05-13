source("global.R")

ui <- fluidPage(
    includeCSS("www/global_style.css"),
    
    navbarPage(title="title", id="main", windowTitle="windowTitle",
               tabPanel(title="Upload data",
                        sidebarLayout(
                            sidebarPanel(
                                uploadInputFileUI("input")
                            ),
                            mainPanel(
                                displayPlotUI("test")
                            )
                        )
               ),
               tabPanel(title="Edit knowledge base",
                        sidebarLayout(
                            sidebarPanel(
                                generateTemplateUI("getTemplate"),
                                submitTemplateUI("submitTemplate"),
                                addSingleCASNumberUI("addCAS"),
                                removeCASNumbersUI("removeCAS"),
                            ),
                            mainPanel(
                                showLogsUI("displayLogs")
                            )
                        )
                ),
    )
    
)

# Wrap UI with secure_app
ui <- secure_app(ui, enable_admin = TRUE,
                 tags_bottom = tags$div(
                   tags$p(style="text-align: center;",
                     	  "To create an account or for any questions, please contact the PTV:"
                   ),
		           tags$p(style="text-align: center;",
		                  admin1.mail, br(),
			              admin2.mail, br(),
			              admin3.mail
                   )
                 )
)

server <- function(input, output, session) {
    # call the server part
    # check_credentials returns a function to authenticate users
    res_auth <- secure_server(
        check_credentials = check_credentials(
            db = "./data/login_db.sqlite",
            passphrase = db.passphrase
        )
    )
    
    # Hide the tab "Edit knowledge base" to non admin users
    observe({
        if (!is.null(res_auth$user) && reactiveValuesToList(res_auth)$admin == FALSE) {
            removeTab("main", target="Edit knowledge base")
        }
    })
    
    # "Upload data" tab---------------------------------------------------------
    dataSplit <- uploadInputFileServer("input")
    displayPlotServer("test", dataSplit)
    
    # "Edit knowledge base" tab-------------------------------------------------
    generateTemplateServer("getTemplate")
    submitTemplateServer("submitTemplate")
    addSingleCASNumberServer("addCAS")
    removeCASNumbersServer("removeCAS")
    showLogsServer("displayLogs")
}

shinyApp(ui, server)